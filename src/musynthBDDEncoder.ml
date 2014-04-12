(* routines for encoding automata into BDDs *)
(* also performs rudimentary checks on determinism of incomplete automata *)

open MusynthTypes
module AST = MusynthAST
module DD = MusynthBDD

let encodeStateVariables automaton =
  let name, states = 
    (match automaton with
     | LLCompleteAutomaton (name, states, _, _, _, _) -> name, states
     | LLIncompleteAutomaton (name, states, _, _, _) -> name, states)
  in
  let statename = LLFieldDesignator (name, "state") in
  let statenameprime = LLFieldDesignator (name, "state'") in
  let statereg = DD.registerVar statename states in
  let statenameprimereg = DD.registerVar statenameprime states in
  (statereg, statenameprimereg)

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
           let paramreg = DD.registerVar name vallist in
           paramreg :: acc) [] transitions

let getmsgsForAut aut =
  match aut with
  | LLCompleteAutomaton (_, _, inmsgs, outmsgs, _, _)
  | LLIncompleteAutomaton (_, _, inmsgs, outmsgs, _) -> (inmsgs, outmsgs)

let getnameforaut aut =
  match aut with
  | LLCompleteAutomaton (name, _, _, _, _, _)
  | LLIncompleteAutomaton (name, _, _, _, _) -> name

let getTransitionsForAut aut =
  match aut with
  | LLCompleteAutomaton (_, _, _, _, transitions, _)
  | LLIncompleteAutomaton (_, _, _, _, transitions) -> transitions

let getCSPredsForMsg msg transitions statename =
  List.fold_left
    (fun acc trans ->
     match trans with
     | TComplete (start, m, final) ->
        if m = msg then
          LLPropOr (LLPropEquals (statename, start), acc)
        else
          acc
     | TParametrizedDest (start, m, (paramname, dset)) ->
        if m = msg then
          LLPropOr (LLPropAnd (LLPropEquals (statename, start),
                               (LLPropNot (LLPropEquals (paramname, "defer")))),
                    acc)
        else
          acc) LLPropFalse transitions

let getTransitionRelationForAut aut lsmap cspredmap =
  let name = getnameforaut aut in
  let transitions = getTransitionsForAut aut in
  let statename = LLFieldDesignator (name, "state") in
  let pstatename = LLFieldDesignator (name, "state'") in
  let disjList = 
    List.fold_left 
      (fun acc trans ->
       match trans with
       | TComplete (startstate, msg, _)
       | TParametrizedDest (startstate, msg, _, _) ->
          let propState = LLPropEquals (statename, startstate) in
          let listeners, senders = LLDesigMap.find msg lsmap in
          if (LLDesigSet.cardinal senders) <> 1 then
            raise (SemanticError ("Message \"" ^ (lldesigToString msg) ^ "\" has multiple senders", 
                                  None))
          else
            begin
              let cspreds = [ LLDesigLLDesigMap.find (LLDesigSet.min_elt senders, msg) ] in
              let cspreds = 
                LLDesigSet.fold 
                  (fun listener acc ->
                   let mypred = LLDesigLLDesigMap.find (listener, msg) in
                   LLPropAnd (mypred, acc)) cspreds listeners 
              in
              let choose = LLSimpleDesignator "choose" in
              let choosepred = LLPropEquals (choose, msg) in
              
              let nspred = 
                (match trans with
                 | TComplete (_, _, endstate) -> LLPropEquals (pstatename, endstate)
                 | TParametrizedDest (_, _, (paramname, destset)) ->
                    LLDesigSet.fold 
                      (fun dest acc ->
                       LLPropAnd (LLPropEquals (paramname, dest), 
                                  LLPropEquals (pstatename, dest))) destset LLPropTrue)
              in
              (LLPropAnd (propState, LLPropAnd (cspreds, LLPropAnd (choosepred, nspred)))) :: acc
            end)
  in
  List.fold_left 
    (fun acc disj -> LLPropOr (Disj, acc)) LLPropFalse disjList

let encodeTransitionRelation automata allmsgs =
  (* build the list of listeners and senders *)
  let lsmap, cspredmap = 
    List.fold_left
      (fun (mapacc, predacc) msg ->
       let listeners, senders, cspreds = 
         (List.fold_left
            (fun (inacc, outacc, csacc) aut ->
             let name = getnameforaut aut in
             let statename = LLFieldDesignator (name, "state") in
             let transitions = getTransitionsForAut aut in
             let inmsgs, outmsgs = getmsgsforaut aut in
             let newinacc, newcsacc = 
               if List.mem msg inmsgs then
                 let csPreds = getCSPredsForMsg msg transitions statename in
                 (LLDesigLLDesigMap.add (name, msg) csPreds,
                  LLDesigSet.add name inacc)
               else
                 (inacc, csacc)
             in
             let newoutacc, newcsacc = 
               if List.mem msg outmsgs then
                 let csPreds = getCSPredsForMsg msg transition statename in
                 (LLDesigSet.add name outacc, 
                  LLDesigLLDesigMap.add (name, msg) csPreds)
               else
                 (outacc, csacc))
           (LLDesigSet.empty, LLDesigSet.empty, LLDesigLLDesigMap.empty)
           automata)
       in
       (LLDesigMap.add msg (listeners, senders) mapacc,
        LLDesigLLDesigMap.fold_left (fun k v acc -> LLDesigLLDesigMap.add k v acc)
                                    cspreds predacc))
      (LLDesigMap.empty, LLDesigLLDesigMap.empty) allmsgs
  in
  (* we're now ready to encode the transition relation *)
  (* we do it one automaton at a time, aka conjunctive *)
  (* partitioning. We can merge them together later if *)
  (* this proves to be non performant.                 *)
  


      
let encodeProg prog =
  (* encode the choose variable for scheduling first *)
  let msgdecls, automata, _, _ = prog in
  let choose = LLSimpleDesignator ("Choose") in
  let chooseprime = getPrimedLLDesig choose in
  let choosereg = DD.registerVar choose msgdecls in
  let chooseprimereg = DD.registerVar chooseprime msgdecls in
  (* encode the state variables of the automata next *)
  List.iter (fun aut -> ignore (encodeStateVariables aut)) automata;
  (* encode the parameters of the automata *)
  List.iter (fun aut -> ignore (encodeParamVariables aut)) automata;
  (* encode the transition relation *)
            
