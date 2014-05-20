(* utility/helper functions for building up transition relations *)

open MusynthTypes
module Utils = MusynthUtils
module Opts = MusynthOptions

let constructEnabledProp autlist automaton = 
  let _, outmsgs = Utils.getMsgsForAut automaton in
  List.fold_left 
    (fun prop msg ->
     LLPropOr (Utils.getCSPredsForMsgAll msg autlist, prop)) LLPropFalse outmsgs

(* deadlock freedom prop. constructed in terms of available transitions *)
let constructDLProps msglist automata =
  List.fold_left 
    (fun prop msg ->
     let sprop = Utils.getCSPredsForMsgAll msg automata in
     LLPropAnd (LLPropNot sprop, prop)) LLPropTrue msglist

let makeTransPropOnMsgForAut msg aut =
  let transitions = Utils.getTransitionsForAut aut in
  let statename = Utils.getStateNameForAutomaton aut in
  let statenameP = Utils.getStateNamePForAutomaton aut in
  List.fold_left 
    (fun prop trans ->
     match trans with
     | TComplete (start, m, final) ->
        if m = msg then
          LLPropOr
            (LLPropAnd (LLPropEquals (statename, start),
                        LLPropEquals (statenameP, final)),
             prop)
        else
          prop
     | TParametrizedDest (start, m, (paramname, destset)) ->
        if m = msg then
          let nsprop = 
            LLDesigSet.fold
              (fun s prop ->
               if (s = (Utils.makeDeferDesig ())) then
                 prop
               else
                 LLPropOr (LLPropAnd (LLPropEquals (paramname, s),
                                      LLPropEquals (statenameP, s)),
                           prop)) destset LLPropFalse
          in
          LLPropOr
            (LLPropAnd (LLPropEquals (statename, start), nsprop), 
             prop)
        else
          prop
     | _ -> assert false) LLPropFalse transitions

(* Make the transition prop for one message *)
let makeTransPropForMsg msg autlist =
  let sender = Utils.getSender msg autlist in
  let receivers = Utils.getReceivers msg autlist in
  let relaut = sender :: receivers in
  let otheraut = 
    List.filter
      (fun aut -> (not (List.mem aut relaut)))
      autlist
  in
  let otherunchangedprop = 
    List.fold_left
      (fun prop aut ->
       let statename = Utils.getStateNamePForAutomaton aut in
       let statenameP = Utils.getStateNamePForAutomaton aut in
       LLPropAnd (LLPropEquals (statename, statenameP), prop))
      LLPropTrue otheraut
  in
  let cspred = Utils.getCSPredsForMsgAll msg autlist in
  let tprop = 
    List.fold_left 
      (fun prop aut ->
       LLPropAnd (makeTransPropOnMsgForAut msg aut, prop)) cspred relaut
  in
  LLPropAnd (tprop, otherunchangedprop)

(* builds the transition relation as a map *)
(* Also builds the enabled and taken props *)
let makeTransMap msglist autlist =
  List.fold_left 
    (fun m msg ->
     let tprop = makeTransPropForMsg msg autlist in
     let ctprop = Utils.canonicalizePropFP tprop in
     LLDesigMap.add msg ctprop m)
    LLDesigMap.empty msglist

let makeRestrictedTransProp transMap msglist =
  let msgset = 
    List.fold_left 
      (fun set msg -> LLDesigSet.add msg set) LLDesigSet.empty msglist 
  in
  if !Opts.disjunctivePart then
    LLDesigSet.fold 
      (fun msg lst ->
       (LLDesigMap.find msg transMap) :: lst)
      msgset []
  else
    [ LLDesigSet.fold 
        (fun msg prop ->
         LLPropOr (LLDesigMap.find msg transMap, prop))
        msgset LLPropFalse ]

