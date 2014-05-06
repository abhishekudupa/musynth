(* Utility functions common to all model checking routines *)

open MusynthTypes
open Cudd
open Format

module AST = MusynthAST
module Debug = MusynthDebug
module Opts = MusynthOptions
module MGR = MusynthBDDManager

let post mgr transRel states =
  let ucube = mgr#getCubeForUnprimedVars () in
  let substTable = mgr#getSubstTableP2U () in
  let postimage = Bdd.existand ucube states transRel in
  Bdd.vectorcompose substTable postimage

let pre mgr transRel states =
  let pcube = mgr#getCubeForPrimedVars () in
  let substTable = mgr#getSubstTableU2P () in
  let substBdd = Bdd.vectorcompose substTable states in
  Bdd.existand pcube substBdd transRel

let inclusionFixPointTester oldpred newpred =
  Bdd.is_leq newpred oldpred

let eqFixPointTester oldpred newpred = 
  Bdd.is_equal oldpred newpred

let postAndTransformer mgr transRel pred =
  Bdd.dand pred (post mgr transRel pred)

let postOrTransformer mgr transRel pred =
  Bdd.dor pred (post mgr transRel pred)

let preAndTransformer mgr transRel pred =
  Bdd.dand pred (pre mgr transRel pred)

let preOrTransformer mgr transRel pred =
  Bdd.dor pred (pre mgr transRel pred)

(* compute the post image k times *)
(* and return the set of states reachable in k or fewer steps *)
(* also indicates if a fixpoint was reached *)
let postK k mgr transRel states = 
  let rec postKRec k reach frontier =
    match k with
    | 0 -> ExecNonConverged reach
    | _ -> 
       let newReach = Bdd.dor reach (post mgr transRel frontier) in
       if (Bdd.is_leq newReach reach) then
         ExecFixpoint newReach
       else
         let newFrontier = Bdd.dand newReach (Bdd.dnot reach) in
         postKRec (k - 1) newReach newFrontier
  in
  postKRec k states states

(* compute the pre image k times *)
(* and return the set of states that can reach "states" *)
(* in k or fewer steps *)
(* also indicates if a fixpoint was reached *)
let preK k mgr transRel states =
  let rec preKRec k reach frontier = 
    match k with
    | 0 -> ExecNonConverged reach
    | _ -> 
       let newReach = Bdd.dor reach (post mgr transRel frontier) in
       if (Bdd.is_leq newReach reach) then
         ExecFixpoint newReach
       else
         let newFrontier = Bdd.dand newReach (Bdd.dnot reach) in
         preKRec (k - 1) newReach newFrontier
  in
  preKRec k states states

(* fix point function *)
let rec computeFixPoint pTransFormer fpCondition pred =
  let newPred = pTransFormer pred in
  if fpCondition pred newPred then
    newPred
  else
    computeFixPoint pTransFormer fpCondition newPred

(* construct a path from some initial state to *)
(* the specified error state. Assumption is that *)
(* such a path exists! *)

let findPathCube mgr initStates transRel errstate = 
  (* construct the set of states reachable in 1, 2, ... k steps *)
  (* stop when we hit the error state *)
  (* evaluates to a list that terminates with the error state reachable *)
  let rec forwardStates reachable frontier =
    if (not (Bdd.is_inter_empty reachable errstate)) then
      [ reachable ]
    else
      begin
        let newReach = Bdd.dor reachable (post mgr transRel frontier) in
        let newFrontier = Bdd.dand newReach (Bdd.dnot reachable) in
        if (Bdd.is_false newFrontier) then
          raise (Failure ("MusynthMC.findPathCube: Reached fixpoint before hitting target!"))
        else
          reachable :: (forwardStates newReach newFrontier)
      end
  in
  (* now extract a sequence of pres for the error state in the list *)
  let rec foldStateList stateK myState =
    match stateK with
    | [] -> assert false
    | [ head ] ->
       let minTerm = mgr#pickMinTermOnStates (Bdd.dand myState head) in
       [ mgr#cubeOfMinTerm minTerm ]
    | head :: rest ->
       let traceSoFar = foldStateList rest myState in
       let firstInTrace = List.hd traceSoFar in
       let preImage = pre mgr transRel firstInTrace in
       let preImageRestricted = Bdd.dand preImage head in
       let minTerm = mgr#pickMinTermOnStates preImageRestricted in
       let cube = mgr#cubeOfMinTerm minTerm in
       cube :: traceSoFar
  in
  let stateList = forwardStates initStates initStates in
  foldStateList stateList errstate
                
let findPath mgr initStates transRel errstate =
  List.map 
    mgr#getStateVars 
    (findPathCube mgr initStates transRel errstate)

let findLoopCube mgr initStates transRel finalStates jlist clist =
  if (Bdd.is_false finalStates) then
    ([], [])
  else
    begin
      let pFinalStates = 
        Bdd.vectorcompose 
          (mgr#getSubstTableU2P ()) finalStates in
      let newTrans = Bdd.dand (Bdd.dand finalStates pFinalStates) transRel in
      let s = mgr#cubeOfMinTerm (Bdd.pick_minterm finalStates) in

      let rec refineS s =
        let pres = computeFixPoint (preOrTransformer mgr newTrans) 
                                   inclusionFixPointTester s 
        in
        let posts = computeFixPoint (postOrTransformer mgr newTrans) 
                                    inclusionFixPointTester s 
        in
        let news = Bdd.dand posts (Bdd.dnot pres) in
        if Bdd.is_false news then
          s
        else
          let s = mgr#cubeOfMinTerm (Bdd.pick_minterm news) in
          refineS s
      in
      
      let s = refineS s in
      let final = computeFixPoint (postOrTransformer mgr newTrans) 
                                  inclusionFixPointTester s 
      in
      let pfinal = Bdd.vectorcompose (mgr#getSubstTableU2P ()) final in
      let newTrans = Bdd.dand (Bdd.dand final pfinal) newTrans in
      let prefix = findPathCube mgr initStates transRel final in
      let lastInPrefix = List.hd (List.rev prefix) in
      let postOfLastInPrefix = post mgr newTrans lastInPrefix in
      let period = [ mgr#cubeOfMinTerm (Bdd.pick_minterm postOfLastInPrefix) ] in
      let period = 
        List.fold_left
          (fun period justiceSpec ->
           let satJusticeSpec prop = (not (Bdd.is_inter_empty prop justiceSpec)) in
           if (List.exists satJusticeSpec period) then
             period
           else
             let lastInPeriod = List.nth period ((List.length period) - 1) in
             let newPath = findPathCube mgr lastInPeriod newTrans (Bdd.dand final justiceSpec) in
             period @ (List.tl newPath))
          period jlist
      in
      let period = 
        List.fold_left
          (fun period compassionSpec ->
           let p, q = compassionSpec in
           let satCompassionSpec prop = (not (Bdd.is_inter_empty prop q)) in
           if ((not (List.exists satCompassionSpec period)) && 
                 (not (Bdd.is_inter_empty final p))) then
             begin
               let lastInPeriod = List.nth period ((List.length period) - 1) in
               let newPath = findPathCube mgr lastInPeriod newTrans (Bdd.dand final q) in
               period @ (List.tl newPath)
             end
           else
             period) period clist
      in
      let lastInPeriod = List.nth period ((List.length period) - 1) in
      let newPath = findPathCube mgr lastInPeriod newTrans lastInPrefix in
      let period = period @ (List.tl newPath) in
      (prefix, period)
    end

let findLoop mgr initStates transRel finalStates jlist clist =
  let prefix, period = findLoopCube mgr initStates transRel finalStates jlist clist in
  (List.map mgr#getStateVars prefix,
   List.map mgr#getStateVars period)

