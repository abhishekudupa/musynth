(* functions for handling safety requirements *)
(* specifically, functions for constructing   *)
(* the deadlock freedom property are in this  *)
(* file                                       *)

open MusynthTypes
open Format

module Utils = MusynthUtils
module Opts = MusynthOptions
module AST = MusynthAST
module Debug = MusynthDebug

let constructDLFProps msglist automata =
  Debug.dprintf 2 "Deadlock freedom props:@,";
  List.fold_left 
    (fun prop msg ->
     let msgprop = Utils.getCSPredsForMsgAll msg automata in
     Debug.dprintf 2 "%a@," AST.pLLProp (Utils.canonicalizePropFP msgprop);
     LLPropOr (msgprop, prop)) LLPropFalse msglist
