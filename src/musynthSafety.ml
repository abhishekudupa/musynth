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
  LLPropNot (LLPropEquals (Utils.makeLCMesgDesig (), 
                           Utils.makeDeadlockDesig ()))
