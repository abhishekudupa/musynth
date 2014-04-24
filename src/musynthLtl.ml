(* module for dealing with ltl formulae and buchi automaton *)

open MusynthTypes
open Format

module Utils = MusynthUtils
module Opts = MusynthOptions

let constructEnabledProp autlist automaton = 
  let _, outmsgs = Utils.getMsgsForAut automaton in
  List.fold_left 
    (fun prop msg ->
     LLPropOr (Utils.getCSPredsForMsgAll msg autlist, prop)) LLPropFalse outmsgs

(* works with any kind of automaton *)
let constructSchedFairnessSpecs aut =
  let autname = Utils.getNameForAut aut in
  let enprop = constructEnabledProp automata aut in
  let chooseprop = LLPropEquals (choose, autname) in
  match !Opts.fairnessType with
  | FairnessTypeWeak ->
     Justice (LLPropOr (LLPropNot enprop, chooseprop))
  | FairnessTypeStrong ->
     Compassion (enprop, LLPropAnd (enprop, chooseprop))

let constructChannelFairnessSpecs aut =
  let ftype = Utils.getFairnessForAutomaton aut in
  let lftype = Utils.getLFairnessForAutomaton aut in
  let dftype = Utils.getDFairnessForAutomaton aut in
  
  

let constructFairnessSpecs prog = 
  let _, automata, _, _ = prog in
  let choose = LLSimpleDesignator "choose" in
  List.fold_left 
    (fun flist aut ->
     let autname = Utils.getNameForAut aut in
     let enprop = constructEnabledProp automata aut in
     let chooseprop = LLPropEquals (choose, autname) in
     match !Opts.fairnessType with
     | FairnessTypeWeak ->
        Justice (LLPropOr (LLPropNot enprop, chooseprop)) :: flist
     | FairnessTypeStrong ->
        Compassion (enprop, LLPropAnd (enprop, chooseprop)) :: flist)
    [] automata

  


let createTablueaxVars prop =
  let rec createTablueaxVarsRec ptf2varmap var2ptfmap chimap prop =
    match prop with
    | LLPropTrue
    | LLPropFalse
    | LLPropEquals _ -> (ptf2varmap, var2ptfmap,
                         PropMap.add prop prop chimap)
    | LLPropNot prop1 ->
       let p2vmap, v2pmap, chimap = 
         createTablueaxVarsRec ptf2varmap var2ptfmap chimap prop1 in
       (p2vmap, v2pmap, 
        PropMap.add prop 
                    (LLPropNot (PropMap.find prop1 chimap))
                    chimap)
    | LLPropAnd (prop1, prop2)
    | LLPropOr (prop1, prop2) ->
       let p2vmap, v2pmap, chimap =
         createTablueaxVarsRec ptf2varmap var2ptfmap chimap prop1 in
       let p2vmap, v2pmap, chimap =
         createTablueaxVarsRec p2vmap v2pmap chimap prop2 in
       (p2vmap, v2pmap,
        PropMap.add prop
                    (match prop with
                     | LLPropAnd _ ->
                        (LLPropAnd (PropMap.find prop1 chimap,
                                    PropMap.find prop2 chimap))
                     | LLPropOr _ ->
                        (LLPropAnd (PropMap.find prop1 chimap,
                                    PropMap.find prop2 chimap))
                     | _ -> assert false)
                    chimap)
    | LLPropTLX prop1 ->
       let p2vmap, v2pmap, chimap = 
         createTablueaxVarsRec ptf2varmap var2ptfmap chimap prop1 in
       let varuid = Utils.getuid () in
       let varname = "ltltableauvar_" ^ (string_of_int varuid) in
       let vardesig = LLSimpleDesignator varname in
       (PropMap.add prop vardesig p2vmap,
        LLDesigMap.add vardesig prop v2pmap,
        PropMap.add prop (LLPropEquals (vardesig, Utils.makeTrueDesig ())) chimap)
    | LLPropTLU (prop1, prop2) ->
       let p2vmap, v2pmap, chimap =
         createTablueaxVarsRec ptf2varmap var2ptfmap chimap prop1 in
       let p2vmap, v2pmap, chimap =
          createTablueaxVarsRec p2vmap v2pmap chimap prop2 in
       let varuid = Utils.getuid () in
       let varname = "ltltableauvar_" ^ (string_of_int varuid) in
       let vardesig = LLSimpleDesignator varname in
       (PropMap.add prop vardesig p2vmap,
        LLDesigMap.add vardesig prop v2pmap,
        PropMap.add prop (LLPropEquals (vardesig, Utils.makeTrueDesig ())) chimap)
  in
  createTablueaxVarsRec PropMap.empty LLDesigMap.empty PropMap.empty prop

(* util functions to construct transition *)
let getXFormulaVarPairs p2vmap = 
  PropMap.fold
    (fun prop var lst ->
     match prop with
     | LLPropTLX _ -> (prop, var) :: lst
     | _ -> lst) p2vmap []

let getUFormulaVarPairs p2vmap =
  PropMap.fold
    (fun prop var lst ->
     match prop with
     | LLPropTLU _ -> (prop, var) :: lst
     | _ -> lst) p2vmap []

let constructPrimedVarMap v2pmap =
  LLDesigMap.fold 
    (fun vardesig prop map ->
     LLDesigMap.add vardesig (getPrimedLLDesig vardesig) map)
    v2pmap LLDesigMap.empty

let rec substForVars var2PrimedMap prop =
  match prop with
  | LLPropTrue
  | LLPropFalse -> prop
  | LLPropEquals (desig1, desig2) ->
     let substdesig1 = 
       try 
         LLDesigMap.find desig1 var2PrimedMap 
       with Not_found ->
         desig1 in
     let substdesig2 = 
       try 
         LLDesigMap.find desig2 var2PrimedMap 
       with Not_found ->
         desig2 in
     LLPropEquals (substdesig1, substdesig2)
  | LLPropNot prop1 ->
     LLPropNot (substForVars var2PrimedMap prop1)
  | LLPropAnd (prop1, prop2) ->
     LLPropAnd (substForVars var2PrimedMap prop1,
                substForVars var2PrimedMap prop2)
  | LLPropOr (prop1, prop2) ->
     LLPropOr (substForVars var2PrimedMap prop1,
               substForVars var2PrimedMap prop2)
  | LLPropTLX (prop1) ->
     LLPropTLX (substForVars var2PrimedMap prop1)
  | LLPropTLU (prop1, prop2) ->
     LLPropTLU (substForVars var2PrimedMap prop1,
                substForVars var2PrimedMap prop2)


(* construct a tableau for the ltl property *)
(* returns: (1) A prop to var map *)
(*          (2) A var to prop man *)
(*          (3) A mapping chi for subformulas *)
(*          (4) a transition relation *)
(*          (5) a list of justice properties *)
(* Initial states and compassion properties aren't required *)
(* because we're only dealing with LTL properties now *)
(* NOTE: We actually construct the tableau for \neg prop! *)
let constructTableau ltlprop =
  let ltlprop = LLPropNot ltlprop in
  let p2vmap, v2pmap, chimap = createTablueaxVars ltlprop in
  let var2PrimedMap = constructPrimedVarMap v2pmap in
  let xFormulaVarPairs = getXFormulaVarPairs p2vmap in
  let uFormulaVarPairs = getUFormulaVarPairs p2vmap in
  let xprops = 
    List.map
      (fun (formula, vardesig) ->
       match formula with
       | LLPropTLX p ->
          let chiofp = PropMap.find p chimap in
          let chiofform = PropMap.find formula chimap in
          let chiprimeofp = substForVars var2PrimedMap chiofp in
          LLPropAnd (LLPropOr (LLPropNot chiofform, chiprimeofp),
                     LLPropOr (LLPropNot chiprimeofp, chiofform))
       | _ -> assert false) xFormulaVarPairs
  in
  let uprops = 
    List.map 
      (fun (formula, vardesig) ->
       match formula with
       | LLPropTLU (p, q) ->
          let chiofp = PropMap.find p chimap in
          let chiofq = PropMap.find q chimap in
          let chiofform = PropMap.find formula chimap in
          let chiprimeofform = substForVars var2PrimedMap chiofform in
          let antecedent = chiofform in
          let consequent = LLPropOr (chiofq, LLPropAnd (chiofp, chiprimeofform)) in
          LLPropAnd (LLPropOr (LLPropNot antecedent, consequent),
                     LLPropOr (LLPropNot consequent, antecedent))
       | _ -> assert false) uFormulaVarPairs
  in
  let transrel = 
    List.fold_left 
      (fun acc prop ->
       LLPropAnd (prop, acc)) LLPropTrue (xprops @ uprops)
  in
  (* construct the fairness props for the tableau *)
  let jlist = 
    List.map
      (fun (formula, vardesig) ->
       match formula with
       | LLPropTLU (p, q) ->
          let chiofq = PropMap.find q chimap in
          let chiofform = PropMap.find formula chimap in
          (LLPropOr (chiofq, LLPropNot chiofform))
       | _ -> assert false) uFormulaVarPairs
  in
  (p2vmap, v2pmap, chimap, transrel, jlist)
