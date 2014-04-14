(* functions for handling safety requirements *)
(* specifically, functions for constructing   *)
(* the deadlock freedom property are in this  *)
(* file                                       *)

open MusynthTypes
module Utils = MusynthUtils
module DD = MusynthBDD
module Opts = MusynthOptions

let constructDLFProps msglist automata =
  List.fold_left 
    (fun prop msg ->
     let msgprop = Utils.getCSPredsForMsgAll msg automata in
     LLPropOr (msgprop, prop)) msglist

