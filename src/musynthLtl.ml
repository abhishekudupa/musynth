(* module for dealing with ltl formulae and buchi automaton *)

open MusynthTypes
open Format

module Utils = MusynthUtils
module Opts = MusynthOptions

let constructFairnessSpecForOneState allaut automaton state =
  let statename = Utils.getStateNameForAutomaton automaton in
  let choose = LLSimpleDesignator ("choose") in
  let msgs = Utils.getMsgsToSyncOnFromState automaton state in
  let sprop = LLPropEquals (statename, state) in

  List.fold_left
    (fun prop msg ->
     let csprop = Utils.getCSPredsForMsgAll msg allaut in
     let antecedent = 
       (match !Opts.fairnessType with
        | FairnessTypeWeak ->
           LLPropTLF (LLPropTLG (LLPropAnd (sprop, csprop)))
        | FairnessTypeStrong ->
           LLPropTLG (LLPropTLF (LLPropAnd (sprop, csprop)))) 
     in
     let consequent = LLPropTLG (LLPropTLF (LLPropEquals (choose, msg))) in
     let myprop = LLPropOr (LLPropNot (antecedent), consequent) in
     LLPropOr (myprop, prop)) LLPropFalse msgs

let constructFairnessSpecForOneAut allaut automaton =
  let states = Utils.getStatesForAut automaton in
  List.fold_left
    (fun accProp state ->
     let msgs = Utils.getMsgsToSyncOnFromState automaton state in
     if (List.length msgs) = 0 then
       accProp
     else
       LLPropAnd (accProp, constructFairnessSpecForOneState allaut automaton state))
    LLPropTrue states

let constructFairnessSpec prog =
  let _, automata, _, _ = prog in
  List.fold_left 
    (fun accProp aut ->
     LLPropAnd (constructFairnessSpecForOneAut automata aut, accProp)) 
    LLPropTrue automata
  

(* construct a tableau for the ltl property *)
(* returns: (1) a transition relation *)
(*          (2) a list of justice properties *)
(* Initial states and compassion properties aren't required *)
(* because we're only dealing with LTL properties now *)
let constructTableau mgr ltlprop =
  
