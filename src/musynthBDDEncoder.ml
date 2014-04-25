(* routines for encoding automata into BDDs *)
(* also performs rudimentary checks on determinism of incomplete automata *)

open MusynthTypes
open Format

module AST = MusynthAST
module Utils = MusynthUtils
module Safety = MusynthSafety
module Debug = MusynthDebug
module LTL = MusynthLtl

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

let getNextStatePropOnAllForMsg autlist msg =
  let sender = Utils.getSender msg autlist in
  let receivers = Utils.getReceivers msg autlist in
  let relaut = sender :: receivers in

  List.fold_left 
    (fun autprop aut ->
     let reltrans = 
       List.filter 
         (fun trans ->
          match trans with
          | TComplete (_, m, _) 
          | TParametrizedDest (_, m, _) -> msg = m
          | _ -> assert false) (Utils.getTransitionsForAut aut) 
     in
     let statenameP = Utils.getStateNamePForAutomaton aut in
     let myprop = 
       List.fold_left 
         (fun prop trans ->
          LLPropOr (getNextStatePropForTrans statenameP trans, prop)) LLPropFalse reltrans
     in
     LLPropAnd (myprop, autprop)) LLPropTrue relaut


(* builds the transition relation. Also includes part of the *)
(* transition relation for lastchosen that is determined by  *)
(* this automaton                                            *)
let getTransitionRelationForAut lastchosenp aut autlist =
  let transitions = Utils.getTransitionsForAut aut in
  let statename = Utils.getStateNameForAutomaton aut in
  let statenamep = Utils.getStateNamePForAutomaton aut in
  let relationElems, lcPropList = 
    List.fold_left 
      (fun (propList, lcPropList) trans ->
       match trans with
       | TComplete (sstate, msg, _)
       | TParametrizedDest (sstate, msg, _) ->
          let sprop = LLPropEquals (statename, sstate) in
          let csprop = Utils.getCSPredsForMsgAll msg autlist in
          let sender = Utils.getSender msg autlist in
          let sendername = Utils.getNameForAut sender in
          let otherautprop = getNextStatePropOnAllForMsg autlist msg in
          let chooseprop = LLPropEquals (LLSimpleDesignator "choose", sendername) in
          let nsprop = getNextStatePropForTrans statenamep trans in
          let tsprop = LLPropAnd (otherautprop, LLPropAnd (sprop, LLPropAnd (csprop, chooseprop))) in
          let newLCPropList = 
            if sender = aut then
              (LLPropAnd (tsprop, nsprop), 
               LLPropEquals (lastchosenp, msg)) :: lcPropList
            else
              lcPropList
          in
          ((tsprop, nsprop) :: propList, newLCPropList)
       | _ -> assert false) ([], []) transitions 
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
  (List.fold_left 
     (fun acc (tsprop, nsprop) ->
      LLPropOr (acc, LLPropAnd (tsprop, nsprop))) LLPropFalse relationElems,
   lcPropList)
        

let encodeChooseTransitions choose choosep autlist =
  List.fold_left 
    (fun acc msg ->
     LLPropOr (LLPropEquals (choosep, msg), acc)) LLPropFalse 
    (List.map Utils.getNameForAut autlist)

(* evaluates to a map of transition relations *)
let encodeTransitionRelation automata choose choosep lastchosen lastchosenp =
  let m, lcprops = 
    List.fold_left 
      (fun (mapacc, lcproplist) aut -> 
       let t, lcprops = getTransitionRelationForAut lastchosenp aut automata in
       (LLDesigMap.add (Utils.getStateNameForAutomaton aut) t mapacc,
        lcproplist @ lcprops))
      (LLDesigMap.empty, []) automata
  in
  let m = LLDesigMap.add choose (encodeChooseTransitions choose choosep automata) m in
  (* add the transition for the lastchosen variable *)
  let lpropsall = 
    List.fold_left 
      (fun propacc (ant, con) ->
       LLPropOr (LLPropAnd (ant, con), propacc)) LLPropFalse lcprops
  in
  (* add the transition for the case where no transition can occur *)
  let notransprop = 
    List.fold_left 
      (fun propacc (ant, con) ->
       LLPropOr (ant, propacc)) LLPropFalse lcprops
  in
  let notransprop = LLPropNot notransprop in
  let lpropsall = LLPropOr (LLPropAnd (notransprop, 
                                       LLPropEquals (lastchosenp, 
                                                     LLSimpleDesignator ("error"))),
                            lpropsall) 
  in
  LLDesigMap.add lastchosen lpropsall m

let encodeProg mgr prog =
  (* encode the choose variable for scheduling first *)
  let msgdecls, automata, initconstraints, specs = prog in
  let automataname = List.map Utils.getNameForAut automata in
  let choose = LLSimpleDesignator ("choose") in
  let choosep = getPrimedLLDesig choose in
  let lastchosen = LLSimpleDesignator ("lastchosen") in
  let lastchosenp = getPrimedLLDesig lastchosen in
  let _ = mgr#registerStateVariable choose automataname in
  let _ = mgr#registerStateVariable lastchosen ((LLSimpleDesignator ("error")) :: msgdecls) in
  (* encode the state variables of the automata next *)
  List.iter (fun aut -> ignore (encodeStateVariables mgr aut)) automata;
  (* encode the parameters of the automata *)
  List.iter (fun aut -> ignore (encodeParamVariables mgr aut)) automata;
  let tranrelations = encodeTransitionRelation automata choose choosep lastchosen lastchosenp in

  if Debug.debugEnabled () then
    begin
      Debug.dprintf "trans" "Transition Relations:\n";
      LLDesigMap.iter
        (fun name rel ->
         Debug.dprintf "trans" "%a:@," AST.pLLDesignator name;
         Debug.dprintf "trans" "%a@,@," AST.pLLProp (Utils.canonicalizePropFP rel)) tranrelations;
    end
  else
    ();

  let invariants =
    List.fold_left
      (fun propacc spec ->
       match spec with
       | LLSpecInvar (_, prop) -> LLPropAnd (prop, propacc)
       | LLSpecLTL _ -> propacc) LLPropTrue specs in

  let schedFairnessSpecs = LTL.constructFairnessSpecs prog in
  let universaljlist = 
    List.fold_left 
      (fun acc spec -> 
       match spec with
       | Justice prop -> prop :: acc
       | _ -> acc) [] schedFairnessSpecs in
  let universalclist = 
    List.fold_left
      (fun acc spec ->
       match spec with
       | Compassion (prop1, prop2) -> (prop1, prop2) :: acc
       | _ -> acc) [] schedFairnessSpecs in

  let ltltableaulist =
    List.fold_left
      (fun lst spec ->
       match spec with
       | LLSpecLTL (name, prop, justicelist, compassionlist) ->
          let tableaudesc = LTL.constructTableau prop in
          let p2vmap, v2pmap, chimap, t, tjlist = tableaudesc in
          let myjlist = universaljlist @ justicelist @ tjlist in
          let myclist = universalclist @ compassionlist in
          (* Debug.dprintf 1 "Tableau:@,%a@,@," AST.pLLProp transrel; *)
          (p2vmap, v2pmap, chimap, t, myjlist, myclist) :: lst
       | _ -> lst) [] specs in

  let boolValDomain = [ Utils.makeFalseDesig (); Utils.makeTrueDesig () ] in
  let encodedTableauList =
    List.map 
      (fun (p2vmap, v2pmap, chimap, t, myjlist, myclist) ->
       LLDesigMap.iter 
         (fun v p -> 
          mgr#registerStateVariable v boolValDomain) 
         v2pmap;
       let enct = mgr#prop2BDD t in
       let encjlist = List.map mgr#prop2BDD myjlist in
       let encclist = List.map (fun (a, b) -> (mgr#prop2BDD a, mgr#prop2BDD b)) myclist in
       (p2vmap, v2pmap, chimap, enct, encjlist, encclist)) 
      ltltableaulist
  in
        
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
  (transBDDs, initBDD, badStateBDD, dlfBDD, encodedTableauList)
    
