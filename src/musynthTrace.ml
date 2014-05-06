(* routines for printing and handling traces *)

open MusynthTypes
open Format

module Opts = MusynthOptions
module AST = MusynthAST
module Debug = MusynthDebug

let printState fmt state =
  LLDesigMap.iter
    (fun name v ->
     fprintf fmt "%a |--> %a@," AST.pLLDesignator name AST.pLLDesignator v) state

let printTraceFullSafety fmt trace =
  let count = ref 0 in
  let rec printTraceFullRec trace =
    match trace with
    | [] -> ()
    | [ head ] ->
       fprintf fmt "Step %d (End of trace):@," !count;
       fprintf fmt "----------------------------------------------------------------------@,";
       printState fmt head;
       fprintf fmt "----------------------------------------------------------------------@,@,";
       count := !count + 1;
    | head :: rest ->
       fprintf fmt "Step %d:@," !count;
       fprintf fmt "----------------------------------------------------------------------@,";
       printState fmt head;
       fprintf fmt "----------------------------------------------------------------------@,@,";
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

let printTraceDiffSafety fmt trace =
  let count = ref 0 in
  let rec printTraceDiffRec prevOpt trace =
    count := !count + 1;
    match trace with
    | [] -> ()
    | [ head ] ->
       begin
         fprintf fmt "Step %d: (End of trace):@," (!count - 1);
         fprintf fmt "----------------------------------------------------------------------@,";
         (match prevOpt with
          | Some prev -> 
             printState fmt (computeDiff prev head);
          | None ->
             printState fmt head);
         fprintf fmt "----------------------------------------------------------------------@,@,"
       end
    | head :: rest ->
       begin
         fprintf fmt "Step %d:@," (!count - 1);
         fprintf fmt "----------------------------------------------------------------------@,";
         (match prevOpt with
          | Some prev -> 
             printState fmt (computeDiff prev head);
          | None ->
             printState fmt head);
         fprintf fmt "----------------------------------------------------------------------@,@,";
         printTraceDiffRec (Some head) rest
       end
  in
  printTraceDiffRec None trace

let printTraceDiffLiveness fmt prefix loop =
  let count = ref 0 in
  let loopStartStep = ref 0 in
  let rec printPrefix prevOpt prefix = 
    match prefix with
    | [] -> ()
    | [ head ] ->
       begin
         fprintf fmt "Step %d (Beginning of loop, in full):@," !count;
         fprintf fmt "----------------------------------------------------------------------@,";
         printState fmt head;
         fprintf fmt "----------------------------------------------------------------------@,@,";
         loopStartStep := !count;
         count := !count + 1
       end
    | head :: rest ->
       begin
         fprintf fmt "Step %d:@," !count;
         fprintf fmt "----------------------------------------------------------------------@,";
         (match prevOpt with
          | Some prev -> 
             printState fmt (computeDiff prev head);
          | None ->
             printState fmt head);
         fprintf fmt "----------------------------------------------------------------------@,@,";
         count := !count + 1;
         printPrefix (Some head) rest
       end
  in
  let rec printLoop prevOpt loop =
    match loop with
    | [] -> ()
    | [ head ] -> 
       begin
         fprintf fmt "Step %d (Loopback, same as step %d above, in full):@," !count !loopStartStep;
         fprintf fmt "----------------------------------------------------------------------@,";
         printState fmt head;
         fprintf fmt "----------------------------------------------------------------------@,@,";
         count := !count + 1
       end
    | head :: rest ->
       begin
         fprintf fmt "Step %d:@," !count;
         fprintf fmt "----------------------------------------------------------------------@,";
         (match prevOpt with
          | Some prev -> 
             printState fmt (computeDiff prev head);
          | None ->
             printState fmt head);
         fprintf fmt "----------------------------------------------------------------------@,@,";
         count := !count + 1;
         printLoop (Some head) rest
       end
  in
  printPrefix None prefix;
  printLoop (Some (List.nth prefix ((List.length prefix) - 1))) loop

let printTraceFullLiveness fmt prefix loop = 
  let count = ref 0 in
  let loopStartStep = ref 0 in
  let rec printPrefix prefix = 
    match prefix with
    | [] -> ()
    | [ head ] ->
       fprintf fmt "Step %d (Beginning of loop):@," !count;
       fprintf fmt "----------------------------------------------------------------------@,";
       printState fmt head;
       fprintf fmt "----------------------------------------------------------------------@,@,";
       loopStartStep := !count;
       count := !count + 1
    | head :: rest ->
       fprintf fmt "Step %d:@," !count;
       fprintf fmt "----------------------------------------------------------------------@,";
       printState fmt head;
       fprintf fmt "----------------------------------------------------------------------@,@,";
       count := !count + 1;
       printPrefix rest
  in
  let rec printLoop loop =
    match loop with
    | [] -> ()
    | [ head ] -> 
       fprintf fmt "Step %d (Loopback, same as step %d above):@," !count !loopStartStep;
       fprintf fmt "----------------------------------------------------------------------@,";
       printState fmt head;
       fprintf fmt "----------------------------------------------------------------------@,@,";
       count := !count + 1;
    | head :: rest ->
       fprintf fmt "Step %d:@," !count;
       fprintf fmt "----------------------------------------------------------------------@,";
       printState fmt head;
       fprintf fmt "----------------------------------------------------------------------@,@,";
       count := !count + 1;
       printLoop rest
  in
  printPrefix prefix;
  printLoop loop  

let printTraceSafety fmt trace =
  if (Debug.debugEnabled ()) then
    match !Opts.tracePrintMode with
    | "full" -> printTraceFullSafety fmt trace
    | "diff" -> printTraceDiffSafety fmt trace
    | _ -> assert false
  else
    ()

let printTraceLiveness fmt prefix loop =
  if (Debug.debugEnabled ()) then
    match !Opts.tracePrintMode with
    | "full" -> printTraceFullLiveness fmt prefix loop
    | "diff" -> printTraceDiffLiveness fmt prefix loop
    | _ -> assert false
  else
    ()
