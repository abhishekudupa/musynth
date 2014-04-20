(* functions for handling safety requirements *)
(* specifically, functions for constructing   *)
(* the deadlock freedom property are in this  *)
(* file                                       *)

open MusynthTypes
open Format

module Utils = MusynthUtils
module Opts = MusynthOptions
module AST = MusynthAST

let constructDLFProps msglist automata =
  fprintf std_formatter "Deadlock freedom props:\n";
  List.fold_left 
    (fun prop msg ->
     let msgprop = Utils.getCSPredsForMsgAll msg automata in
     fprintf std_formatter "%a\n" AST.pLLProp (Utils.canonicalizePropFP msgprop);
     LLPropOr (msgprop, prop)) LLPropFalse msglist
