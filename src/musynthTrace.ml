(* routines for printing and handling traces *)

open MusynthTypes

module Opts = MusynthOptions
module AST = MusynthAST
module Debug = MusynthDebug

let printState state =
  LLDesigMap.iter
    (fun name v ->
     Debug.dprintf "trace" "%a |--> %a@," AST.pLLDesignator name AST.pLLDesignator v) state

let printTraceFullSafety trace =
  let count = ref 0 in
  let rec printTraceFullRec trace =
    match trace with
    | [] -> ()
    | [ head ] ->
       Debug.dprintf "trace" "Step %d (End of trace):@," !count;
       Debug.dprintf "trace" "----------------------------------------------------------------------@,";
       printState head;
       Debug.dprintf "trace" "----------------------------------------------------------------------@,@,";
       count := !count + 1;
    | head :: rest ->
       Debug.dprintf "trace" "Step %d:@," !count;
       Debug.dprintf "trace" "----------------------------------------------------------------------@,";
       printState head;
       Debug.dprintf "trace" "----------------------------------------------------------------------@,@,";
       count := !count + 1;
       printTraceFullRec rest
  in
  printTraceFullRec trace 

let computeDiff prev curr =
  LLDesigMap.fold
    (fun name v diffmap ->
     if ((LLDesigMap.find name prev) <> v) then
       LLDesigMap.add name v diffmap
     else
       diffmap) curr LLDesigMap.empty

let printTraceDiffSafety trace =
  let count = ref 0 in
  let rec printTraceDiffRec prevOpt trace =
    count := !count + 1;
    match trace with
    | [] -> ()
    | [ head ] ->
       begin
         Debug.dprintf "trace" "Step %d: (End of trace):@," (!count - 1);
         Debug.dprintf "trace" "----------------------------------------------------------------------@,";
         (match prevOpt with
          | Some prev -> 
             printState (computeDiff prev head);
          | None ->
             printState head);
         Debug.dprintf "trace" "----------------------------------------------------------------------@,@,"
       end
    | head :: rest ->
       begin
         Debug.dprintf "trace" "Step %d:@," (!count - 1);
         Debug.dprintf "trace" "----------------------------------------------------------------------@,";
         (match prevOpt with
          | Some prev -> 
             printState (computeDiff prev head);
          | None ->
             printState head);
         Debug.dprintf "trace" "----------------------------------------------------------------------@,@,";
         printTraceDiffRec (Some head) rest
       end
  in
  printTraceDiffRec None trace

let printTraceDiffLiveness prefix loop =
  let count = ref 0 in
  let loopStartStep = ref 0 in
  let rec printPrefix prevOpt prefix = 
    match prefix with
    | [] -> ()
    | [ head ] ->
      begin
        Debug.dprintf "trace" "Step %d (Beginning of loop, in full):@," !count;
        Debug.dprintf "trace" "----------------------------------------------------------------------@,";
        printState head;
        Debug.dprintf "trace" "----------------------------------------------------------------------@,";
        count := !count + 1;
        loopStartStep := !count
      end
    | head :: rest ->
      begin
        Debug.dprintf "trace" "Step %d:@," !count;
        Debug.dprintf "trace" "----------------------------------------------------------------------@,";
        (match prevOpt with
        | Some prev -> 
          printState (computeDiff prev head);
        | None ->
          printState head);
        Debug.dprintf "trace" "----------------------------------------------------------------------@,";
        count := !count + 1;
        printPrefix (Some head) rest
      end
  in
  let rec printLoop prevOpt loop =
    match loop with
    | [] -> ()
    | [ head ] -> 
      begin
        Debug.dprintf "trace" "Step %d (Loopback, same as step %d above, in full):@," !count !loopStartStep;
        Debug.dprintf "trace" "----------------------------------------------------------------------@,";
        printState head;
        Debug.dprintf "trace" "----------------------------------------------------------------------@,";
        count := !count + 1
      end
    | head :: rest ->
      begin
        Debug.dprintf "trace" "Step %d:@," !count;
        Debug.dprintf "trace" "----------------------------------------------------------------------@,";
        (match prevOpt with
        | Some prev -> 
          printState (computeDiff prev head);
        | None ->
          printState head);
        Debug.dprintf "trace" "----------------------------------------------------------------------@,";
        count := !count + 1;
        printLoop (Some head) rest
      end
  in
  printPrefix None prefix;
  printLoop (Some (List.nth prefix ((List.length prefix) - 1))) loop

let printTraceFullLiveness prefix loop = 
  let count = ref 0 in
  let loopStartStep = ref 0 in
  let rec printPrefix prefix = 
    match prefix with
    | [] -> ()
    | [ head ] ->
      Debug.dprintf "trace" "Step %d (Beginning of loop):@," !count;
      Debug.dprintf "trace" "----------------------------------------------------------------------@,";
      printState head;
      Debug.dprintf "trace" "----------------------------------------------------------------------@,";
      count := !count + 1;
      loopStartStep := !count
    | head :: rest ->
      Debug.dprintf "trace" "Step %d:@," !count;
      Debug.dprintf "trace" "----------------------------------------------------------------------@,";
      printState head;
      Debug.dprintf "trace" "----------------------------------------------------------------------@,";
      count := !count + 1;
      printPrefix rest
  in
  let rec printLoop loop =
    match loop with
    | [] -> ()
    | [ head ] -> 
      Debug.dprintf "trace" "Step %d (Loopback, same as step %d above):@," !count !loopStartStep;
      Debug.dprintf "trace" "----------------------------------------------------------------------@,";
      printState head;
      Debug.dprintf "trace" "----------------------------------------------------------------------@,";
      count := !count + 1;
    | head :: rest ->
      Debug.dprintf "trace" "Step %d:@," !count;
      Debug.dprintf "trace" "----------------------------------------------------------------------@,";
      printState head;
      Debug.dprintf "trace" "----------------------------------------------------------------------@,";
      count := !count + 1;
      printLoop rest
  in
  printPrefix prefix;
  printLoop loop  

let printTraceSafety trace =
  if (Debug.debugEnabled ()) then
    match !Opts.tracePrintMode with
    | "full" -> printTraceFullSafety trace
    | "diff" -> printTraceDiffSafety trace
    | _ -> assert false
  else
    ()

let printTraceLiveness prefix loop =
  if (Debug.debugEnabled ()) then
    match !Opts.tracePrintMode with
    | "full" -> printTraceFullLiveness prefix loop
    | "diff" -> printTraceDiffLiveness prefix loop
    | _ -> assert false
  else
    ()
