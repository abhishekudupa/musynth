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
    count := !count + 1;
    match trace with
    | [] -> ()
    | [ head ] ->
       Debug.dprintf "trace" "Step %d (End of trace):@," (!count - 1);
       Debug.dprintf "trace" "----------------------------------------------------------------------@,";
       printState head;
       Debug.dprintf "trace" "----------------------------------------------------------------------@,@,"

    | head :: rest ->
       Debug.dprintf "trace" "Step %d:@," (!count - 1);
       Debug.dprintf "trace" "----------------------------------------------------------------------@,";
       printState head;
       Debug.dprintf "trace" "----------------------------------------------------------------------@,@,";
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

let printTraceSafety trace =
  if (Debug.debugEnabled ()) then
    match !Opts.tracePrintMode with
    | "full" -> printTraceFullSafety trace
    | "diff" -> printTraceDiffSafety trace
    | _ -> assert false
  else
    ()
