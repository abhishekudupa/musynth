(* routines for encoding automata into BDDs *)
(* also performs rudimentary checks on determinism of incomplete automata *)

open MusynthTypes
open Format

module AST = MusynthAST
module Utils = MusynthUtils
module Safety = MusynthSafety
module Debug = MusynthDebug

let encodeStateVariables mgr automaton =
  let name, states = 
    (match automaton with
     | LLCompleteAutomaton (name, states, _, _, _, _) -> name, states
     | LLIncompleteAutomaton (name, states, _, _, _) -> name, states)
  in
  let statename = Utils.getStateNameForAutomaton automaton in
  mgr#registerStateVariable statename states

let encodeParamVariables mgr automaton = 
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
           let paramreg = mgr#registerParamVariable name vallist in
           paramreg :: acc
        | _ -> assert false) [] transitions


let getNextStatePropForTrans primedstate transition =
  match transition with
  | TComplete (_, _, fstate) -> LLPropEquals (primedstate, fstate)
  | TParametrizedDest (_, _, (paramname, valset)) ->
     LLDesigSet.fold
       (fun valu acc ->
        if (valu = (LLSimpleDesignator "defer")) then
          acc
        else
          LLPropOr
            (LLPropAnd (LLPropEquals (paramname, valu),
                        LLPropEquals (primedstate, valu)),
             acc)) valset LLPropFalse
  | _ -> assert false

let getNextStatePropOnSenderForMsg autlist msg =
  let sender = Utils.getSender msg autlist in
  let reltrans = 
    List.filter 
      (fun trans ->
       match trans with
       | TComplete (_, m, _) 
       | TParametrizedDest (_, m, _) -> msg = m
       | _ -> assert false) (Utils.getTransitionsForAut sender) in
  let statenameP = Utils.getStateNamePForAutomaton sender in
  List.fold_left 
    (fun prop trans ->
     LLPropOr (getNextStatePropForTrans statenameP trans, prop)) LLPropFalse reltrans


let getTransitionRelationForAut aut autlist =
  let transitions = Utils.getTransitionsForAut aut in
  let statename = Utils.getStateNameForAutomaton aut in
  let statenamep = Utils.getStateNamePForAutomaton aut in
  let relationElems = 
    List.fold_left 
      (fun propList trans ->
       match trans with
       | TComplete (sstate, msg, _)
       | TParametrizedDest (sstate, msg, _) ->
          let sprop = LLPropEquals (statename, sstate) in
          let csprop = Utils.getCSPredsForMsgAll msg autlist in
          let sender = Utils.getSender msg autlist in
          let sendername = Utils.getNameForAut sender in
          let otherautprop = 
            if sender = aut then 
              LLPropTrue else 
              getNextStatePropOnSenderForMsg autlist msg 
          in
          let chooseprop = LLPropEquals (LLSimpleDesignator "choose", sendername) in
          let nsprop = getNextStatePropForTrans statenamep trans in
          let tsprop = LLPropAnd (otherautprop, LLPropAnd (sprop, LLPropAnd (csprop, chooseprop))) in
          (tsprop, nsprop) :: propList
       | _ -> assert false) [] transitions 
  in
  (* we have all the relation elements *)
  (* add on one for the case where no transition can occur *)
  let someTransProp = 
    List.fold_left 
      (fun acc (tsprop, nsprop) -> LLPropOr (acc, tsprop))
      LLPropFalse relationElems
  in
  let relationElems = 
    (LLPropNot someTransProp, LLPropEquals (statename, statenamep)) :: relationElems in
  List.fold_left 
    (fun acc (tsprop, nsprop) ->
     LLPropOr (acc, LLPropAnd (tsprop, nsprop))) LLPropFalse relationElems
        

let encodeChooseTransitions choose choosep autlist =
  List.fold_left 
    (fun acc msg ->
     LLPropOr (LLPropEquals (choosep, msg), acc)) LLPropFalse 
    (List.map Utils.getNameForAut autlist)

(* evaluates to a map of transition relations *)
let encodeTransitionRelation automata choose choosep =
  let m = 
    List.fold_left 
      (fun mapacc aut -> 
       let t = getTransitionRelationForAut aut automata in
       LLDesigMap.add (Utils.getStateNameForAutomaton aut) t mapacc)
      LLDesigMap.empty automata
  in
  LLDesigMap.add choose (encodeChooseTransitions choose choosep automata) m

let encodeProg mgr prog =
  (* encode the choose variable for scheduling first *)
  let msgdecls, automata, initconstraints, specs = prog in
  let automataname = List.map Utils.getNameForAut automata in
  let choose = LLSimpleDesignator ("choose") in
  let choosep = getPrimedLLDesig choose in
  let _ = mgr#registerStateVariable choose automataname in
  (* encode the state variables of the automata next *)
  List.iter (fun aut -> ignore (encodeStateVariables mgr aut)) automata;
  (* encode the parameters of the automata *)
  List.iter (fun aut -> ignore (encodeParamVariables mgr aut)) automata;
  let tranrelations = encodeTransitionRelation automata choose choosep in
  (* Debug.dprintf 2 "Transition Relations:\n"; *)
  (* LLDesigMap.iter *)
  (*   (fun name rel -> *)
  (*    Debug.dprintf 2 "%a:@," AST.pLLDesignator name; *)
  (*    Debug.dprintf 2 "%a@,@," AST.pLLProp (Utils.canonicalizePropFP rel)) tranrelations; *)
  let invariants =
    List.fold_left
      (fun propacc spec ->
       match spec with
       | LLSpecInvar (_, prop) -> LLPropAnd (prop, propacc)
       | LLSpecLTL _ -> propacc) LLPropTrue specs in
  let badstates = LLPropNot invariants in
  let dlfProp = Safety.constructDLFProps msgdecls automata in
  (* Debug.dprintf 2 "Deadlock Freedom Property:@,"; *)
  (* Debug.dprintf 2 "%a@,@," AST.pLLProp (Utils.canonicalizePropFP dlfProp); *)
  (* Debug.dprintf 2 "Bad State Property:@,"; *)
  (* Debug.dprintf 2 "%a@,@," AST.pLLProp (Utils.canonicalizePropFP badstates); *)
          
  let dlfBDD = mgr#prop2BDD dlfProp in
  let transBDDs = 
    LLDesigMap.fold 
      (fun ident prop acc ->
       LLDesigMap.add ident (mgr#prop2BDD prop) acc)
      tranrelations LLDesigMap.empty
  in
  let badStateBDD = mgr#prop2BDD badstates in
  (* Debug.dprintf 2 "InitProp:@,%a@," AST.pLLProp (Utils.canonicalizePropFP initconstraints); *)
  let initBDD = mgr#prop2BDD initconstraints in
  (transBDDs, initBDD, badStateBDD, dlfBDD)
    
