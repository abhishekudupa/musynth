(* module for dealing with ltl formulae and buchi automaton *)

open MusynthTypes
module LTL = Ltl3ba
module Utils = MusynthUtils
module Opts = MusynthOptions
open Format

(* stringify the ltlprop so that it can be processed by LTL3BA *)
(* Also returns a mapping of propositional formula to predicates *)
let propToLtl3BAStr prop =
  let rec propToLtl3BAStrInt s2pmap p2smap prop =
    match prop with
    | LLPropTrue -> (s2pmap, p2smap, "true")
    | LLPropFalse -> (s2pmap, p2smap, "false")
    | LLPropNot prop1 ->
       let nmap1, nmap2, nstr = propToLtl3BAStrInt s2pmap p2smap prop1 in
       (nmap1, nmap2, "(!(" ^ nstr ^ "))")
    | LLPropAnd (prop1, prop2) ->
       let nmap1, nmap2, nstr1 = propToLtl3BAStrInt s2pmap p2smap prop1 in
       let nmap1, nmap2, nstr2 = propToLtl3BAStrInt nmap1 nmap2 prop2 in
       (nmap1, nmap2, "(" ^ nstr1 ^ " && " ^ nstr2 ^ ")")
    | LLPropOr (prop1, prop2) ->
       let nmap1, nmap2, nstr1 = propToLtl3BAStrInt s2pmap p2smap prop1 in
       let nmap1, nmap2, nstr2 = propToLtl3BAStrInt nmap1 nmap2 prop2 in
       (nmap1, nmap2, "(" ^ nstr1 ^ " || " ^ nstr2 ^ ")")
    | LLPropEquals (d1, d2) ->
       (* check if we already have a cached version of this prop *)
       begin
         try
           let cachedProp = PropMap.find prop p2smap in
           (s2pmap, p2smap, cachedProp)
         with Not_found ->
              begin
                let newident = "ltl3var_" ^ (string_of_int (Utils.getuid ())) in
                let nmap1 = StringMap.add newident prop s2pmap in
                let nmap2 = PropMap.add prop newident p2smap in
                (nmap1, nmap2, newident)
              end
       end
    | LLPropTLF prop1 ->
       let nmap1, nmap2, nstr = propToLtl3BAStrInt s2pmap p2smap prop1 in
       (nmap1, nmap2, "(F " ^ nstr ^ ")")
    | LLPropTLG prop1 ->
       let nmap1, nmap2, nstr = propToLtl3BAStrInt s2pmap p2smap prop1 in
       (nmap1, nmap2, "(G " ^ nstr ^ ")")
    | LLPropTLX prop1 ->
       let nmap1, nmap2, nstr = propToLtl3BAStrInt s2pmap p2smap prop1 in
       (nmap1, nmap2, "(X " ^ nstr ^ ")")
    | LLPropTLU (prop1, prop2) ->
       let nmap1, nmap2, nstr1 = propToLtl3BAStrInt s2pmap p2smap prop1 in
       let nmap1, nmap2, nstr2 = propToLtl3BAStrInt nmap1 nmap2 prop2 in
       (nmap1, nmap2, "(" ^ nstr1 ^ " U " ^ nstr2 ^ ")")
    | LLPropTLR (prop1, prop2) ->
       let nmap1, nmap2, nstr1 = propToLtl3BAStrInt s2pmap p2smap prop1 in
       let nmap1, nmap2, nstr2 = propToLtl3BAStrInt nmap1 nmap2 prop2 in
       (nmap1, nmap2, "(" ^ nstr1 ^ " R " ^ nstr2 ^ ")")
  in
  let cprop = Utils.canonicalizePropFP prop in
  propToLtl3BAStrInt StringMap.empty PropMap.empty cprop

let constructFairnessSpecForOneState allaut automaton state =
  let statename = Utils.getStateNameForAutomaton automaton in
  let choose = LLSimpleDesignator ("choose") in
  let msgs = Utils.getMsgsToSyncOnFromState automaton state in
  let sprop = LLPropEquals (statename, state) in
  List.fold_left
    (fun prop msg ->
     let csprop = Utils.getCSPredsForMsgAll msg allaut in
     let antecedent = 
       (match !Opts.fairnesstype with
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
  
let ltlspec2BA spec =
  let s2pmap, p2smap, specstr = propToLtl3BAStr spec in
  let specstr = specstr ^ " -> G (a -> F b)" in
  fprintf std_formatter "LTL Spec: %s\n" specstr; pp_print_flush std_formatter ();
  fprintf std_formatter "Going to ltl3ba\n"; pp_print_flush std_formatter ();
  let ba = LTL.ltl3ba specstr false false in
  (s2pmap, p2smap, ba)

let rec ltl3baProp2LLProp ltlprop =
  ()
