(* routines for encoding automata into BDDs *)
(* also performs rudimentary checks on determinism of incomplete automata *)

open MusynthTypes
open Format

module AST = MusynthAST
module Utils = MusynthUtils
module Debug = MusynthDebug
module LTL = MusynthLtl
module Trans = MusynthTrans

let encodeStateVariables mgr automaton =
  let name, states = 
    (match automaton with
     | LLCompleteAutomaton (name, states, _, _, _, _, _, _, _) -> name, states
     | LLIncompleteAutomaton (name, states, _, _, _, _) -> name, states)
  in
  let statename = Utils.getStateNameForAutomaton automaton in
  mgr#registerStateVariable statename states

let encodeParamVariables mgr automaton = 
  match automaton with
  | LLCompleteAutomaton _ -> []
  | LLIncompleteAutomaton (_, _, _, _, transitions, _) ->
     List.fold_left 
       (fun acc trans ->
        match trans with
        | TComplete _ -> acc
        | TParametrizedDest (_, _, var) ->
           let name, valset = var in
           let vallist = LLDesigSet.fold (fun valu acc -> valu :: acc) valset [] in
           let paramreg = mgr#registerParamVariable name vallist in
           paramreg :: acc
        | _ -> assert false) [] transitions

(* evaluates to a map of bdds for each message *)
let encodeTransitionRelation mgr allmsgs automata =
  let tpropmap = Trans.makeTransMap allmsgs automata in
  LLDesigMap.map (mgr#prop2BDD) tpropmap

let encodeFairnessSpec mgr fspec =
  match fspec with
  | FairnessSpecNone -> FairnessSpecNone
  | ProcessJustice (pname, enprop, rtrans) -> 
     ProcessJustice (pname, mgr#prop2BDD enprop, 
                     List.map mgr#prop2BDD rtrans)
  | ProcessCompassion (pname, enprop, rtrans) ->
     ProcessCompassion (pname, mgr#prop2BDD enprop, 
                        List.map mgr#prop2BDD rtrans)
  | LossCompassion (cname, imname, omname, irtrans, ortrans) ->
     LossCompassion (cname, imname, omname, 
                     List.map mgr#prop2BDD irtrans,
                     List.map mgr#prop2BDD ortrans)
  | DupCompassion (cname, imname, omname, irtrans, ortrans)->
     DupCompassion (cname, imname, omname, 
                    List.map mgr#prop2BDD irtrans,
                    List.map mgr#prop2BDD ortrans)
  | Justice (p1, p2) -> Justice (mgr#prop2BDD p1, mgr#prop2BDD p2)
  | Compassion (p1, p2) -> Compassion (mgr#prop2BDD p1, mgr#prop2BDD p2)
  | LTLJustice (p1, p2) -> LTLJustice (mgr#prop2BDD p1, mgr#prop2BDD p2)

let encodeProg mgr prog =
  (* encode the choose variable for scheduling first *)
  let msgdecls, automata, initconstraints, specs, symProps = prog in
  (* encode the state variables of the automata next *)
  List.iter (fun aut -> ignore (encodeStateVariables mgr aut)) automata;
  (* encode the parameters of the automata *)
  List.iter (fun aut -> ignore (encodeParamVariables mgr aut)) automata;

  let transMap = Trans.makeTransMap msgdecls automata in

  if Debug.debugEnabled () then
    begin
      Debug.dprintf "trans" "Transition Relations:\n";
      LLDesigMap.iter
        (fun name rel ->
         Debug.dprintf "trans" "%a:@," AST.pLLDesignator name;
         Debug.dprintf "trans" "%a@,@," AST.pLLProp rel) transMap;
    end
  else
    ();

  let invariants =
    List.fold_left
      (fun propacc spec ->
       match spec with
       | LLSpecInvar (_, prop) -> LLPropAnd (prop, propacc)
       | LLSpecLTL _ -> propacc) LLPropTrue specs in

  let schedFairnessSpecs = LTL.constructFairnessSpecs transMap prog in
  let universaljlist, universalclist = 
    List.partition
      (fun spec -> 
       match spec with 
       | ProcessJustice _ -> true
       | _ -> false) schedFairnessSpecs in

  let ltltableaumap =
    List.fold_left
      (fun m spec ->
       match spec with
       | LLSpecLTL (name, prop, justicelist, compassionlist) ->
          let actJList = List.map (fun (a, b) -> Justice (a, b)) justicelist in
          let actCList = List.map (fun (a, b) -> Compassion (a, b)) compassionlist in
          let tableaudesc = LTL.constructTableau prop in
          let p2vmap, v2pmap, chimap, chioftester, t, tjlist = tableaudesc in
          let myjlist = universaljlist @ actJList @ tjlist in
          let myclist = universalclist @ actCList in
          StringMap.add name (p2vmap, v2pmap, chimap, chioftester, t, myjlist, myclist) m
       | _ -> m) StringMap.empty specs in

  let boolValDomain = [ Utils.makeFalseDesig (); Utils.makeTrueDesig () ] in
  let encodedTableauMap =
    StringMap.map 
      (fun (p2vmap, v2pmap, chimap, chioftester, t, myjlist, myclist) ->
       LLDesigMap.iter 
         (fun v p -> 
          mgr#registerInternalStateVariable v boolValDomain) 
         v2pmap;
       let enct = mgr#prop2BDD t in
       let encjlist = List.map (encodeFairnessSpec mgr) myjlist in
       let encclist = List.map (encodeFairnessSpec mgr) myclist in
       let encchioftester = mgr#prop2BDD chioftester in
       (p2vmap, v2pmap, chimap, encchioftester, enct, encjlist, encclist))
      ltltableaumap
  in
  
  let badstates = LLPropNot invariants in
  let dlProp = Trans.constructDLProps msgdecls automata in
  let dlBDD = mgr#prop2BDD dlProp in
  let transBDDs = LLDesigMap.map mgr#prop2BDD transMap in
  let badStateBDD = mgr#prop2BDD badstates in
  let initBDD = mgr#prop2BDD initconstraints in
  let symPropBDD = mgr#prop2BDD symProps in
  (transBDDs, initBDD, badStateBDD, dlBDD, encodedTableauMap, symPropBDD)

