(* routines for encoding automata into BDDs *)
(* also performs rudimentary checks on determinism of incomplete automata *)

open MusynthTypes
module AST = MusynthAST
module DD = MusynthBDD
module Utils = MusynthUtils
module Safety = MusynthSafety

let encodeStateVariables automaton =
  let name, states = 
    (match automaton with
     | LLCompleteAutomaton (name, states, _, _, _, _) -> name, states
     | LLIncompleteAutomaton (name, states, _, _, _) -> name, states)
  in
  let statename = Utils.getStateNameForAutomaton automaton in
  DD.registerStateVariable statename states

let encodeParamVariables automaton = 
  match automaton with
  | LLCompleteAutomaton _ -> []
  | LLIncompleteAutomaton (_, _, _, _, transitions) ->
     List.fold_left 
       (fun acc trans ->
        match trans with
        | TComplete _ -> acc
        | TParametrizedDest (_, _, var) ->
           let name, valset = var in
           let vallist = LLDesigSet.fold (fun valu acc -> valu :: acc) valset [] in
           let paramreg = DD.registerParamVariable name vallist in
           paramreg :: acc
        | _ -> assert false) [] transitions


let getNextStatePropForTrans primedstate transition =
  match transition with
  | TComplete (_, _, fstate) -> LLPropEquals (primedstate, fstate)
  | TParametrizedDest (_, _, (paramname, valset)) ->
     LLDesigSet.fold
       (fun valu acc ->
        LLPropOr
            (LLPropAnd (LLPropEquals (paramname, valu),
                        LLPropEquals (primedstate, valu)),
             acc)) valset LLPropFalse
  | _ -> assert false


let getTransitionRelationForAut aut autlist =
  let transitions = Utils.getTransitionsForAut aut in
  let statename = Utils.getStateNameForAutomaton aut in
  let statenamep = Utils.getStateNamePForAutomaton aut in
  List.fold_left 
    (fun accprop trans ->
     match trans with
     | TComplete (sstate, msg, _)
     | TParametrizedDest (sstate, msg, _) ->
        let sender = Utils.getSender msg autlist in
        let receivers = Utils.getReceivers msg autlist in
        let sprop = LLPropEquals (statename, sstate) in
        let csprops = List.fold_left 
                        (fun prop pred -> LLPropAnd (prop, pred))
                        LLPropTrue (List.map (Utils.getCSPredsForMsg msg) (sender :: receivers))
        in
        let chooseprop = LLPropEquals (LLSimpleDesignator "choose", msg) in
        let tsprop = LLPropAnd (sprop, LLPropAnd (csprops, chooseprop)) in
        let nsprop = getNextStatePropForTrans statenamep trans in
        LLPropOr (LLPropAnd (tsprop, nsprop), accprop)
     | _ -> assert false) LLPropFalse transitions
        

let encodeChooseTransitions choose choosep allmsgs =
  List.fold_left 
    (fun acc msg ->
     LLPropOr (LLPropEquals (choosep, msg), acc)) LLPropFalse allmsgs

(* evaluates to a map of transition relations *)
let encodeTransitionRelation automata allmsgs choose choosep =
  let m = 
    List.fold_left 
      (fun mapacc aut -> 
       let t = getTransitionRelationForAut aut automata in
       LLDesigMap.add (Utils.getStateNameForAutomaton aut) t mapacc)
      LLDesigMap.empty automata
  in
  LLDesigMap.add choose (encodeChooseTransitions choose choosep allmsgs) m

let encodeProg prog =
  (* encode the choose variable for scheduling first *)
  let msgdecls, automata, initconstraints, specs = prog in
  let choose = LLSimpleDesignator ("choose") in
  let choosep = getPrimedLLDesig choose in
  let _ = DD.registerVar choose msgdecls in
  let _ = DD.registerVar choosep msgdecls in
  (* encode the state variables of the automata next *)
  List.iter (fun aut -> ignore (encodeStateVariables aut)) automata;
  (* encode the parameters of the automata *)
  List.iter (fun aut -> ignore (encodeParamVariables aut)) automata;
  let tranrelations = encodeTransitionRelation automata msgdecls choose choosep in
  let invariants =
    List.fold_left
      (fun propacc spec ->
       match spec with
       | LLSpecInvar (_, prop) -> LLPropAnd (prop, propacc)
       | LLSpecLTL _ -> propacc) LLPropTrue specs in
  let badstates = LLPropNot invariants in
  let dlfProp = Safety.constructDLFProps msgdecls automata in
  let dlfBDD = DD.prop2BDD dlfProp in
  let transBDDs = 
    LLDesigMap.fold 
      (fun ident prop acc ->
       LLDesigMap.add ident (DD.prop2BDD prop) acc)
      tranrelations LLDesigMap.empty
  in
  let badStateBDD = DD.prop2BDD badstates in
  let initBDD = DD.prop2BDD initconstraints in
  (transBDDs, initBDD, badStateBDD, dlfBDD)
    
