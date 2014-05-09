(* routines for encoding automata into BDDs *)
(* also performs rudimentary checks on determinism of incomplete automata *)

open MusynthTypes
open Format

module AST = MusynthAST
module Utils = MusynthUtils
module Debug = MusynthDebug
module LTL = MusynthLtl

(* deadlock freedom prop. constructed in terms of available transitions *)
let constructDLProps msglist automata =
  List.fold_left 
    (fun prop msg ->
     let sprop = Utils.getCSPredsForMsgAll msg automata in
     LLPropAnd (LLPropNot sprop, prop)) LLPropTrue msglist

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
        if (valu = (Utils.makeDeferDesig ())) then
          acc
        else
          LLPropOr
            (LLPropAnd (LLPropEquals (paramname, valu),
                        LLPropEquals (primedstate, valu)),
             acc)) valset LLPropFalse
  | _ -> assert false

let getTransPropOnAllForMsg autlist msg =
  let sender = Utils.getSender msg autlist in
  let receivers = Utils.getReceivers msg autlist in
  let relaut = sender :: receivers in
  let otheraut = 
    List.filter 
      (fun aut -> 
       (not (List.mem aut relaut))) 
      autlist
  in
  let otherunchangedprop = 
    List.fold_left
      (fun prop aut ->
       let statename = Utils.getStateNameForAutomaton aut in
       let statenamep = Utils.getStateNamePForAutomaton aut in
       LLPropAnd (LLPropEquals (statename, statenamep), prop))
      LLPropTrue otheraut
  in
  
  let transprop = 
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
  in
  List.fold_left 
    (fun prop aut ->
     let statename = Utils.getStateNameForAutomaton aut in
     let statenameP = Utils.getStateNamePForAutomaton aut in
     LLPropAnd
       (LLPropEquals (statename, statenamep), prop)) 
    transprop otheraut

(* builds the transition relation. Also includes part of the *)
(* transition relation for lastchosen that is determined by  *)
(* this automaton                                            *)
let getTransRelForMsg msg autlist =
  let csprop = Utils.getCSPredsForMsgAll msg autlist in
  

  let transitions = Utils.getTransitionsForAut aut in
  let statename = Utils.getStateNameForAutomaton aut in
  let statenamep = Utils.getStateNamePForAutomaton aut in
  let lcmesgp = Utils.makeLCMesgDesigPrime () in
  let relationElems = 
    List.fold_left 
      (fun propList trans ->
       match trans with
       | TComplete (sstate, msg, _)
       | TParametrizedDest (sstate, msg, _) ->
          let sprop = LLPropEquals (statename, sstate) in
          let chooseprop = LLPropEquals (lcmesgp, msg) in
          let nsprop = getNextStatePropForTrans statenamep trans in
          let tsprop = LLPropAnd (sprop, chooseprop) in
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
    (fun prop (tsprop, nsprop) ->
     LLPropOr (LLPropAnd (tsprop, nsprop), prop)) LLPropFalse relationElems

(* evaluates to a map of transition relations *)
let encodeTransitionRelation allmsgs automata =
  (* encode the transition relation for the lcmesg and lcproc vars first *)
  let lcmesgProps =
    List.fold_left
      (fun propnspairs msg ->
       let myprop = Utils.getCSPredsForMsgAll msg automata in
       let mynsprop = LLPropEquals (Utils.makeLCMesgDesigPrime (), msg) in
       (myprop, mynsprop) :: propnspairs) [] allmsgs
  in
  (* add a transition for when nothing is enabled *)
  let sometransprop = 
    List.fold_left
      (fun acc (prop, nsprop) ->
       LLPropOr (prop, acc)) LLPropFalse lcmesgProps
  in
  let notransprop = LLPropNot sometransprop in
  let lcmesgProps = (notransprop, LLPropEquals 
                                    (Utils.makeLCMesgDesigPrime (), 
                                     Utils.makeDeadlockDesig ())) :: lcmesgProps
  in
  let lcmesgTrans =
    List.fold_left
      (fun acc (tsprop, nsprop) ->
       LLPropOr (LLPropAnd (tsprop, nsprop), acc)) LLPropFalse lcmesgProps
  in
  let m = LLDesigMap.add (Utils.makeLCMesgDesig ()) lcmesgTrans LLDesigMap.empty in
  (* encode the transition relation for the lcproc var *)
  let lcprocProps =
    List.fold_left
      (fun propnspairs msg ->
       let myprop = LLPropEquals (Utils.makeLCMesgDesigPrime (), msg) in
       let nsprop = LLPropEquals (Utils.makeLCProcDesigPrime (),
                                  Utils.getNameForAut (Utils.getSender msg automata)) in
       (myprop, nsprop) :: propnspairs) [] allmsgs in
  let lcprocProps = (LLPropEquals (Utils.makeLCMesgDesigPrime (), 
                                   Utils.makeDeadlockDesig ()),
                     LLPropEquals (Utils.makeLCProcDesigPrime (),
                                   Utils.makeDeadlockDesig ())) :: lcprocProps in
  let lcprocTrans =
    List.fold_left
      (fun acc (tsprop, nsprop) ->
       LLPropOr (LLPropAnd (tsprop, nsprop), acc)) LLPropFalse lcprocProps 
  in
  let m = LLDesigMap.add (Utils.makeLCProcDesig ()) lcprocTrans m in
  List.fold_left 
    (fun mapacc aut -> 
     let t = getTransitionRelationForAut aut automata in
     LLDesigMap.add (Utils.getStateNameForAutomaton aut) t mapacc)
    m automata

let encodeProg mgr prog =
  (* encode the choose variable for scheduling first *)
  let msgdecls, automata, initconstraints, specs, symProps = prog in
  let automataname = List.map Utils.getNameForAut automata in
  let lcmesg = Utils.makeLCMesgDesig () in
  let lcproc = Utils.makeLCProcDesig () in
  let _ = mgr#registerStateVariable lcmesg ((Utils.makeDeadlockDesig ()) :: msgdecls) in
  let _ = mgr#registerStateVariable lcproc ((Utils.makeDeadlockDesig ()) :: automataname) in
  (* encode the state variables of the automata next *)
  List.iter (fun aut -> ignore (encodeStateVariables mgr aut)) automata;
  (* encode the parameters of the automata *)
  List.iter (fun aut -> ignore (encodeParamVariables mgr aut)) automata;
  let tranrelations = encodeTransitionRelation msgdecls automata in

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
       | Justice _ -> spec :: acc
       | _ -> acc) [] schedFairnessSpecs in
  let universalclist = 
    List.fold_left
      (fun acc spec ->
       match spec with
       | Compassion _ -> spec :: acc
       | _ -> acc) [] schedFairnessSpecs in

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

          if (Debug.debugEnabled ()) then
            begin
              Debug.dprintf "trans" "Tableau:@,%a@,@," AST.pLLProp t;
              Debug.dprintf "ltl" "Justices for prop %a:@,@,"
                            AST.pLLProp prop;
              List.iter 
                (fun j -> 
                 let prop1, prop2 = 
                   match j with
                   | LTLJustice (prop1, prop2) -> Debug.dprintf "ltl" "(LTL) "; prop1, prop2
                   | Justice (prop1, prop2) -> prop1, prop2
                   | _ -> assert false
                 in
                 Debug.dprintf "ltl" "%a -->@,%a@," AST.pLLProp prop1 AST.pLLProp prop2) myjlist;
              Debug.dprintf "ltl" "Compassions for prop %a:@,@," AST.pLLProp prop;
              List.iter 
                (fun j -> 
                 let p, q = 
                   match j with
                   | LossDupCompassion (p, q) -> Debug.dprintf "ltl" "(LD) "; p, q
                   | Compassion (p, q) -> p, q
                   | _ -> assert false
                 in
                 Debug.dprintf "ltl" "%a -->@,%a@," AST.pLLProp p AST.pLLProp q)
                myclist;
              Debug.dflush ()
            end
          else
            ();

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
       let encjlist = List.map 
                        (fun j -> 
                         let prop1, prop2 = 
                           match j with
                           | Justice (prop1, prop2)
                           | LTLJustice (prop1, prop2) -> prop1, prop2
                           | _ -> assert false
                         in
                         mgr#prop2BDD (LLPropOr (LLPropNot prop1, prop2)))
                        myjlist in

       let encclist = List.map 
                        (fun j -> 
                         let prop1, prop2 =
                           match j with
                           | Compassion (prop1, prop2)
                           | LossDupCompassion (prop1, prop2) -> prop1, prop2
                           | _ -> assert false
                         in
                         (mgr#prop2BDD prop1, mgr#prop2BDD prop2)) myclist in
       let encchioftester = mgr#prop2BDD chioftester in
       (p2vmap, v2pmap, chimap, encchioftester, enct, encjlist, encclist))
      ltltableaumap
  in
  
  let badstates = LLPropNot invariants in
  let dlProp = constructDLProps msgdecls automata in
  let dlBDD = mgr#prop2BDD dlProp in
  let transBDDs = 
    LLDesigMap.fold 
      (fun ident prop acc ->
       LLDesigMap.add ident (mgr#prop2BDD prop) acc)
      tranrelations LLDesigMap.empty
  in
  let badStateBDD = mgr#prop2BDD badstates in
  (* add the deadlock freedom to the init constraints *)
  let initconstraints = LLPropAnd 
                          (LLPropAnd 
                             (LLPropNot (LLPropEquals (Utils.makeLCMesgDesig (),
                                                       Utils.makeDeadlockDesig ())),
                              LLPropNot (LLPropEquals (Utils.makeLCProcDesig (),
                                                       Utils.makeDeadlockDesig ()))),
                           initconstraints)
  in
  let initBDD = mgr#prop2BDD initconstraints in
  let symPropBDD = mgr#prop2BDD symProps in
  (transBDDs, initBDD, badStateBDD, dlBDD, encodedTableauMap, symPropBDD)

