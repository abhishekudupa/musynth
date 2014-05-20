(* Utility functions common to all model checking routines *)

open MusynthTypes
open Cudd
open Format

module AST = MusynthAST
module Debug = MusynthDebug
module Opts = MusynthOptions
module MGR = MusynthBDDManager

let getTransRel mgr transBDDs =
  if !Opts.disjunctivePart then
    LLDesigMap.fold 
      (fun k v lst -> v :: lst)
      transBDDs []
  else
    [ LLDesigMap.fold
        (fun k v prop -> Bdd.dor v prop)
        transBDDs (mgr#makeFalse ()) ]

(* Distribute the conjunction over the disjunction if needed *)
let addTableauTransRel transRel tableauTransRel =
  List.map (Bdd.dand tableauTransRel) transRel

let restrictTransRelToStates transRel states =
  List.map (Bdd.dand states) transRel

let composeTransRel mgr transRel =
  List.fold_left
    (fun prop trans ->
     Bdd.dor trans prop)
    (mgr#makeFalse ()) transRel

(* Post without the renaming *)
let postPrime mgr transRel states =
  let ucube = mgr#getCubeForUnprimedVars () in
  List.fold_left 
    (fun acc msgTransRel -> 
     Bdd.dor (Bdd.existand ucube states msgTransRel) acc)
    (mgr#makeFalse ()) transRel

(* Pre of a set of primed states *)
let prePrime mgr transRel states =
  let pcube = mgr#getCubeForPrimedVars () in
  List.fold_left
    (fun acc msgTransRel ->
     Bdd.dor (Bdd.existand pcube states msgTransRel) acc)
    (mgr#makeFalse ()) transRel

let primeSet mgr states =
  let substTable = mgr#getSubstTableU2P () in
  Bdd.vectorcompose substTable states

let unprimeSet mgr states =
  let substTable = mgr#getSubstTableP2U () in
  Bdd.vectorcompose substTable states

let pre mgr transRel states =
  let pstates = primeSet mgr states in
  prePrime mgr transRel pstates

let post mgr transRel states =
  let postImagePrimed = postPrime mgr transRel states in
  unprimeSet mgr postImagePrimed

(* finds the subset s of states such that pre(s) |= restriction *)
(* both states and restriction are in terms of unprimed vars    *)
(* The result s \subseteq states is in terms of unprimed vars   *)
let restrictedPre mgr transRel states restriction =
  let ucube = mgr#getCubeForUnprimedVars () in
  let statesprimed = primeSet mgr states in
  let sandrest = Bdd.dand statesprimed restriction in
  let primedresult = 
    List.fold_left
      (fun acc trans ->
       Bdd.dor (Bdd.existand ucube trans sandrest) acc)
      (mgr#makeFalse ()) transRel
  in
  unprimeSet mgr primedresult

(* the dual of restrictedPre *)
let restrictedPost mgr transRel states restriction =
  let pcube = mgr#getCubeForPrimedVars () in
  let restrictionprimed = primeSet mgr restriction in
  let sandrest = Bdd.dand states restrictionprimed in
  List.fold_left
    (fun acc trans ->
     Bdd.dor (Bdd.existand pcube trans sandrest) acc)
    (mgr#makeFalse ()) transRel

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

let findLoopCube mgr initStates transRel tableauTransRel finalStates jlist clist =
  if (Bdd.is_false finalStates) then
    ([], [])
  else
    begin
      let pFinalStates = 
        Bdd.vectorcompose 
          (mgr#getSubstTableU2P ()) finalStates in
      let newTrans = 
        restrictTransRelToStates 
          transRel 
          (Bdd.dand finalStates pFinalStates)
      in
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
      let newTrans = 
        restrictTransRelToStates
          newTrans (Bdd.dand final pfinal)
      in
      let prefix = findPathCube mgr initStates transRel final in
      let lastInPrefix = List.hd (List.rev prefix) in
      let postOfLastInPrefix = post mgr newTrans lastInPrefix in

      let period = [ mgr#cubeOfMinTerm (Bdd.pick_minterm postOfLastInPrefix) ] in

      (* Helper functions for connecting period with fairness requirements *)
      let rec isTransTaken period rtrans =
        match period with
        | [] -> false
        | [ head ] -> false
        | fst :: snd :: rest ->
           if (not (Bdd.is_inter_empty (post mgr rtrans fst) snd)) then
             true
           else
             isTransTaken (snd :: rest) rtrans
      in
      
      (* similar to findPathCube, except that it also detects when *)
      (* a particular transition is taken                          *)
      let findPathStateOrTrans initState statePred transPred = 
        let rec forwardStates reachable frontier =
          if (not (Bdd.is_inter_empty reachable statePred)) then
            [ reachable ]
          else
            let postReach = post mgr newTrans frontier in
            let postReachTrans = post mgr transPred frontier in
            if (not (Bdd.is_inter_empty postReach postReachTrans)) then
              [ reachable; Bdd.dand postReach postReachTrans ]
            else
              let newReach = Bdd.dor postReach reachable in
              if (Bdd.is_leq newReach reachable) then
                raise (Failure ("MusynthMCUtils.findPathStateOrTrans: Reached fixpoint!"))
              else
                let newFrontier = Bdd.dand newReach (Bdd.dnot reachable) in
                reachable :: (forwardStates newReach newFrontier)
        in
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
        let stateList = forwardStates initState initState in
        let lastState = List.nth stateList ((List.length stateList) - 1) in
        let lastCube = mgr#cubeOfMinTerm (mgr#pickMinTermOnStates lastState) in
        foldStateList stateList lastCube
      in

      let addFairnessToPeriod period fairness =
        match fairness with
        | FairnessSpecNone -> period
        | ProcessJustice (pname, enabled, rtrans) ->
           let rtrans = addTableauTransRel rtrans tableauTransRel in
           let rtrans = restrictTransRelToStates rtrans (Bdd.dand final pfinal) in
           let notenabled = List.exists (Bdd.is_inter_empty enabled) period in
           let taken = isTransTaken period rtrans in
           if (notenabled || taken) then
             period
           else
             let lastInPeriod = List.nth period ((List.length period) - 1) in
             period @ (List.tl (findPathStateOrTrans lastInPeriod (Bdd.dnot enabled) rtrans))
                        
        | ProcessCompassion (pname, enabled, rtrans) ->
           let rtrans = addTableauTransRel rtrans tableauTransRel in
           let rtrans = restrictTransRelToStates rtrans (Bdd.dand final pfinal) in
           let gnotenabled = Bdd.is_inter_empty final enabled in
           let taken = isTransTaken period rtrans in
           if (gnotenabled || taken) then
             period
           else
             let lastInPeriod = List.nth period ((List.length period) - 1) in
             period @ (List.tl (findPathStateOrTrans lastInPeriod (mgr#makeFalse ()) rtrans))

        | LossCompassion (cname, imname, omname, irtrans, ortrans) ->
           let irtrans = addTableauTransRel irtrans tableauTransRel in
           let irtrans = restrictTransRelToStates irtrans (Bdd.dand final pfinal) in
           let ortrans = addTableauTransRel ortrans tableauTransRel in
           let ortrans = restrictTransRelToStates ortrans (Bdd.dand final pfinal) in
           let gnotrecvd = Bdd.is_false (restrictedPre mgr irtrans final final) in
           let sent = isTransTaken period ortrans in
           if (gnotrecvd || sent) then
             period
           else
             let lastInPeriod = List.nth period ((List.length period) - 1) in
             period @ (List.tl (findPathStateOrTrans lastInPeriod (mgr#makeFalse ()) ortrans))

        | DupCompassion (cname, imname, omname, irtrans, ortrans) ->
           let irtrans = addTableauTransRel irtrans tableauTransRel in
           let irtrans = restrictTransRelToStates irtrans (Bdd.dand final pfinal) in
           let ortrans = addTableauTransRel ortrans tableauTransRel in
           let ortrans = restrictTransRelToStates ortrans (Bdd.dand final pfinal) in
           let gnotsent = Bdd.is_false (restrictedPre mgr ortrans final final) in
           let recvd = isTransTaken period irtrans in
           if (gnotsent || recvd) then
             period
           else
             let lastInPeriod = List.nth period ((List.length period) - 1) in
             period @ (List.tl (findPathStateOrTrans lastInPeriod (mgr#makeFalse ()) irtrans))

        | Justice (p, q)
        | LTLJustice (p, q) ->
           let jprop = Bdd.dor (Bdd.dnot p) q in
           if (List.exists (fun cube -> not (Bdd.is_inter_empty jprop cube)) period) then
             period
           else
             let lastInPeriod = List.nth period ((List.length period) - 1) in
             period @ List.tl (findPathCube mgr lastInPeriod newTrans (Bdd.dand final jprop))
                              
        | Compassion (p, q) ->
           if ((List.exists (fun cube -> not (Bdd.is_inter_empty q cube)) period) ||
                 (Bdd.is_inter_empty final q)) then
             period
           else
             let lastInPeriod = List.nth period ((List.length period) - 1) in
             period @ List.tl (findPathCube mgr lastInPeriod newTrans (Bdd.dand final q))
      in
      (* end of helper functions *)
      
      let period = 
        List.fold_left
          (fun period fspec ->
           addFairnessToPeriod period fspec) period (jlist @ clist)
      in
      (prefix, period)
    end (* else *)

let findLoop mgr initStates transRel tableauTransRel finalStates jlist clist =
  let prefix, period = 
    findLoopCube 
      mgr initStates transRel tableauTransRel finalStates jlist clist 
  in
  (List.map mgr#getStateVars prefix,
   List.map mgr#getStateVars period)

