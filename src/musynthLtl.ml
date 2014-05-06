(* module for dealing with ltl formulae and buchi automaton *)

open MusynthTypes
open Format

module Utils = MusynthUtils
module Opts = MusynthOptions
module Debug = MusynthDebug
module AST = MusynthAST

let constructEnabledProp autlist automaton = 
  let _, outmsgs = Utils.getMsgsForAut automaton in
  List.fold_left 
    (fun prop msg ->
     LLPropOr (Utils.getCSPredsForMsgAll msg autlist, prop)) LLPropFalse outmsgs

(* works with any kind of automaton *)
let constructFairnessSpecsAut autlist aut =
  let autname = Utils.getNameForAut aut in
  let enprop = constructEnabledProp autlist aut in
  let chooseprop = LLPropEquals (Utils.makeLCProcDesig (), autname) in
  let ftype = Utils.getFairnessForAutomaton aut in
  match ftype with
  | LLFairnessJustice ->
     Debug.dprintf "ltl" "Aut %a: Justice@," AST.pLLIdent autname;
     Justice (enprop, chooseprop)
  | LLFairnessCompassion ->
     Debug.dprintf "ltl" "Aut %a: Compassion@," AST.pLLIdent autname;
     Compassion (enprop, chooseprop)
  | LLFairnessNone -> 
     Debug.dprintf "ltl" "Aut %a: None@," AST.pLLIdent autname;
     Justice (LLPropTrue, LLPropTrue)

let constructFiniteLossFairness chan =
  let inmsgs, _ = Utils.getMsgsForAut chan in
  List.map
    (fun msg ->
     LossDupCompassion (LLPropEquals (Utils.makeLCMesgDesig (), msg),
                        LLPropEquals (Utils.makeLCMesgDesig (), getPrimedLLDesig msg)))
    inmsgs

let constructFiniteDupFairness chan =
  let inmsgs, _ = Utils.getMsgsForAut chan in
  List.map
    (fun msg ->
     LossDupCompassion (LLPropEquals (Utils.makeLCMesgDesig (), getPrimedLLDesig msg),
                        LLPropEquals (Utils.makeLCMesgDesig (), msg)))
    inmsgs

let constructFairnessSpecsChan autlist chan =
  let lftype = Utils.getLFairnessForAutomaton chan in
  let dftype = Utils.getDFairnessForAutomaton chan in
  let fspecs = constructFairnessSpecsAut autlist chan in
  let fspecs = 
    match fspecs with
    | Justice (LLPropTrue, LLPropTrue) -> []
    | _ -> [ fspecs ]
  in
  let lfspecs = 
    (match lftype with
     | LLLossFairnessNone -> []
     | LLLossFairnessFinite -> constructFiniteLossFairness chan) 
  in
  let dfspecs = 
    (match dftype with
     | LLDupFairnessNone -> []
     | LLDupFairnessFinite -> constructFiniteDupFairness chan) 
  in
  fspecs @ lfspecs @ dfspecs

let constructFairnessSpecs autlist aut =
  match aut with
  | LLCompleteAutomaton (_, _, _, _, _, _, _, _, true) -> constructFairnessSpecsChan autlist aut
  | _ -> [ constructFairnessSpecsAut autlist aut ]

let constructFairnessSpecs prog = 
  let _, automata, _, _ = prog in
  List.concat (List.map (constructFairnessSpecs automata) automata)

let createTablueaxVars prop =
  let rec createTablueaxVarsRec ptf2varmap var2ptfmap chimap prop =
    try
      let _ = PropMap.find prop ptf2varmap in
      (ptf2varmap, var2ptfmap, chimap)
    with
    | Not_found ->
      begin
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
               (LLPropOr (PropMap.find prop1 chimap,
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
      end
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

  Debug.dprintf "ltl" "Transition relation for prop %a:@,@," AST.pLLProp ltlprop;
  Debug.dprintf "ltl" "%a@,@," AST.pLLProp transrel;

  (* construct the fairness props for the tableau *)
  let jlist = 
    List.map
      (fun (formula, vardesig) ->
       match formula with
       | LLPropTLU (p, q) ->
          let chiofq = PropMap.find q chimap in
          let chiofform = PropMap.find formula chimap in
          LTLJustice (chiofform, chiofq)
       | _ -> assert false) uFormulaVarPairs
  in
  (* also return the chi of the formula *)
  let chiofform = PropMap.find ltlprop chimap in
  Debug.dprintf "ltl" "Chimap:@,@,";
  PropMap.iter 
    (fun prop chiofprop ->
      Debug.dprintf "ltl" "%a --> %a@,@," AST.pLLProp prop AST.pLLProp chiofprop)
    chimap;

  Debug.dprintf "ltl" "Chi of tester = %a@," AST.pLLProp chiofform;
  (p2vmap, v2pmap, chimap, chiofform, transrel, jlist)
