(* routines for encoding automata into BDDs *)
(* also performs rudimentary checks on determinism of incomplete automata *)

open MusynthTypes
open Format

module AST = MusynthAST
module Utils = MusynthUtils
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

  

(* evaluates to a map of bdds for each message *)
let encodeTransitionRelation mgr allmsgs automata =
  let tpropmap = makeTransProp allmsgs automata in
  LLDesigMap.map (mgr#prop2BDD) tpropmap

let encodeProg mgr prog =
  (* encode the choose variable for scheduling first *)
  let msgdecls, automata, initconstraints, specs, symProps = prog in
  let automataname = List.map Utils.getNameForAut automata in
  (* encode the state variables of the automata next *)
  List.iter (fun aut -> ignore (encodeStateVariables mgr aut)) automata;
  (* encode the parameters of the automata *)
  List.iter (fun aut -> ignore (encodeParamVariables mgr aut)) automata;

  let transMap = makeTransProp msgdecls automata in

  if Debug.debugEnabled () then
    begin
      Debug.dprintf "trans" "Transition Relations:\n";
      LLDesigMap.iter
        (fun name rel ->
         Debug.dprintf "trans" "%a:@," AST.pLLDesignator name;
         Debug.dprintf "trans" "%a@,@," AST.pLLProp rel) tranrelations;
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

