(* model checking and synthesis routines *)

open MusynthTypes
open Cudd
open Format

module AST = MusynthAST
module Debug = MusynthDebug
module Opts = MusynthOptions
module Trace = MusynthTrace
module MGR = MusynthBDDManager

type bddType = (Man.d Bdd.t)

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
  if fpCondition newPred pred then
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
        let pres = computeFixPoint (fun s -> Bdd.dor s (pre mgr newTrans s)) Bdd.is_equal s in
        let posts = computeFixPoint (fun s -> Bdd.dor s (post mgr newTrans s)) Bdd.is_equal s in
        let newS = Bdd.dand posts (Bdd.dnot pres) in
        if Bdd.is_false newS then
          s
        else
          let s = mgr#cubeOfMinTerm (Bdd.pick_minterm newS) in
          refineS s
      in
      
      let s = refineS s in
      let final = computeFixPoint (fun s -> Bdd.dor s (post mgr newTrans s)) Bdd.is_equal s in
      let pfinal = Bdd.vectorcompose (mgr#getSubstTableU2P ()) final in
      let newTrans = Bdd.dand (Bdd.dand final pfinal) newTrans in
      let prefix = findPathCube mgr initStates transRel final in
      let lastInPrefix = List.hd (List.rev prefix) in
      let postOfLastInPrefix = post mgr newTrans lastInPrefix in
      let period = [ mgr#cubeOfMinTerm (Bdd.pick_minterm postOfLastInPrefix) ] in
      Debug.dprintf "mc" "Satisfying Justice Requirements..."; Debug.dflush ();
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
      Debug.dprintf "mc" "Done!@,Satisfying Compassion Requirements..."; Debug.dflush ();
      Debug.dprintf "mc" "Path so far:@,";
      Trace.printTraceLiveness (Debug.getDebugFmt ()) (List.map mgr#getStateVars prefix)
                               (List.map mgr#getStateVars period);
      Debug.dflush ();
      let period = 
        List.fold_left
          (fun period compassionSpec ->
           Debug.dprintf "mc" "Trying to satisfy compassion spec@,"; Debug.dflush ();
           let p, q = compassionSpec in
           let satCompassionSpec prop = (not (Bdd.is_inter_empty prop q)) in
           if List.exists satCompassionSpec period then
             Debug.dprintf "mc" "Found!@,"
           else
             Debug.dprintf "mc" "Not Found!@,";
           Debug.dflush ();
           if ((not (List.exists satCompassionSpec period)) && 
                 (not (Bdd.is_inter_empty final p))) then
             begin
               let lastInPeriod = List.nth period ((List.length period) - 1) in
               let fmt = Debug.getDebugFmt () in
               Debug.dprintf "mc" "Finding path@,Start:@,";
               let state = mgr#getStateVars lastInPeriod in
               Trace.printState fmt state;
               Debug.dprintf "mc" "@,End:@,";
               let f = mgr#getStateVars q in
               Trace.printState fmt f;
               assert (not (Bdd.is_false (Bdd.dand final q)));
               Bdd.print (mgr#getBitPrinter ()) fmt q;
               Debug.dflush ();
               let newPath = findPathCube mgr lastInPeriod newTrans (Bdd.dand final q) in
               period @ (List.tl newPath)
             end
           else
             period) period clist
      in
      Debug.dprintf "mc" "Done!@,Closing path..."; Debug.dflush ();
      let lastInPeriod = List.nth period ((List.length period) - 1) in
      let newPath = findPathCube mgr lastInPeriod newTrans lastInPrefix in
      let period = period @ (List.tl newPath) in
      Debug.dprintf "mc" "Done!@,"; Debug.dflush ();
      (prefix, period)
    end

let findLoop mgr initStates transRel finalStates jlist clist =
  let prefix, period = findLoopCube mgr initStates transRel finalStates jlist clist in
  (List.map mgr#getStateVars prefix,
   List.map mgr#getStateVars period)

let getSafetyParams mgr initStates reachStates transRel badStates =
  let reachableBadStates = Bdd.dand reachStates badStates in
  if (Bdd.is_false reachableBadStates) then
    mgr#makeTrue ()
  else
    begin
      if (Debug.debugEnabled ()) then
        let fmt = Debug.getDebugFmt () in
        begin
          Debug.dprintf "mc" "Found a safety violation:@,";
          let trace = findPath mgr initStates transRel 
                               (mgr#cubeOfMinTerm (mgr#pickMinTermOnStates reachableBadStates))
          in
          if (Debug.debugOptEnabled "trace") then
            Trace.printTraceSafety fmt trace
          else
            ()
        end
      else
        ();
      let badParams = Bdd.existand (mgr#getAllButParamCube ()) reachStates badStates in
      Bdd.dnot badParams
    end

let getFeasible mgr initStates reach chioftester origTransRel jlist clist =
  let transRel = Bdd.dand reach origTransRel in
  
  let rec elimCycles transRel states = 
    let newStates = computeFixPoint
                      (fun states ->
                       Bdd.dand states (pre mgr transRel states))
                      Bdd.is_equal
                      states
    in
    (* Filter based on justices *)
    let newStates = 
      List.fold_left
        (fun states justiceSpec ->
         let justStates = Bdd.dand states justiceSpec in
         let newTrans = Bdd.dand transRel states in
         computeFixPoint 
           (fun states ->
            Bdd.dor states (pre mgr newTrans states))
           Bdd.is_equal
             justStates) newStates jlist
    in
    (* filter based on compassion *)
    let newStates = 
      List.fold_left
        (fun states compassionSpec ->
         let newTrans = Bdd.dand transRel states in
         let p, q = compassionSpec in
         let cStates1 = Bdd.dand states (Bdd.dnot p) in
         let cStates2 = Bdd.dand states q in
         let cStates2 = 
           computeFixPoint
             (fun states ->
              Bdd.dor states (pre mgr newTrans states))
             Bdd.is_equal
             cStates2
         in
         Bdd.dor cStates1 cStates2) newStates clist
    in
    if (Bdd.is_equal newStates states) then
      newStates
    else
      elimCycles transRel newStates
  in
  let newStates = elimCycles transRel reach in
  (* compute backward fixpoint of newStates *)
  computeFixPoint
    (fun states ->
     Bdd.dor states (pre mgr transRel states))
    Bdd.is_equal
    newStates

let getParamsForInfeasible mgr initStates reach chioftester origTransRel jlist clist =
  (* restrict transrel to only reachable states *)
  let feasibleStates = getFeasible mgr initStates reach chioftester origTransRel jlist clist in
  let feasibleBadStates = Bdd.dand (Bdd.dand feasibleStates chioftester) initStates in
  (* we should not have any feasible cycles! *)
  (* eliminate params which are responsible for a feasible cycle *)
  if (Bdd.is_false feasibleBadStates) then
    mgr#makeTrue ()
  else
    begin
      if (Debug.debugEnabled ()) then
        begin
          let fmt = Debug.getDebugFmt () in
          Debug.dprintf "mc" "Found a liveness violation:@,"; Debug.dflush ();
          let prefix, period = findLoop mgr initStates origTransRel feasibleStates jlist clist in
          if (Debug.debugOptEnabled "trace") then
            Trace.printTraceLiveness fmt prefix period
          else
            ()
        end
      else
        ();
      let badParams = Bdd.exist (mgr#getAllButParamCube ()) feasibleBadStates in
      Bdd.dnot badParams
    end
      
(* computes a restriction on the initial states (parameters really) *)
(* such that the restriction ensures the we're good for at least k steps *)
(* badstates is the states where an invariant is blown *)
(* tableau is the list of ltl tableaus *)
let getParamsForKSteps k paramConstraints mgr transRel initstates badstates tableau =
  let actInitStates = Bdd.dand initstates paramConstraints in
  assert (Bdd.is_inter_empty actInitStates badstates);
  let kReachStat = postK k mgr transRel actInitStates in
  let kReach = 
    (match kReachStat with
     | ExecNonConverged s -> s
     | ExecFixpoint s -> s) 
  in
  (* get params for safety *)
  let sparams = getSafetyParams mgr actInitStates kReach transRel badstates in
  let newParamConstraints = Bdd.dand paramConstraints sparams in

  (* Now for each tableau, construct the set of k reachable states for THAT tableau *)
  (* check if there exist cycles for THAT tableau, and refine params accordingly    *)
  (* and check subsequent tableau with the refined parameters                       *)
  
  let sparams = 
    StringMap.fold
      (fun propname tableau pconstraints ->
       let actInitStates = Bdd.dand pconstraints initstates in
       let _, _, _, chioftester, tableautrans, jlist, clist = tableau in
       let transRel = Bdd.dand transRel tableautrans in
       let kReachStat = postK k mgr transRel actInitStates in
       let kReach =
         (match kReachStat with
          | ExecNonConverged s -> s
          | ExecFixpoint s -> s) 
       in
       let sparams = getParamsForInfeasible mgr actInitStates kReach chioftester 
                                            transRel jlist clist 
       in
       Bdd.dand pconstraints sparams) tableau newParamConstraints
  in
  match kReachStat with
  | ExecNonConverged _ -> ExecNonConverged sparams
  | ExecFixpoint _ -> ExecFixpoint sparams

let conjoinTransitionRels mgr transBDDs =
  LLDesigMap.fold 
    (fun name bdd accbdd ->
     Bdd.dand bdd accbdd) transBDDs (mgr#makeTrue ())

(* TODO: Currently hardwired to use monolithic transition *)
(*       Change this to be based on command line option   *)
let synthFrontEnd mgr transBDDs initBDD badStateBDD dlfBDD ltltableaulist =
  let transrel = conjoinTransitionRels mgr transBDDs in
  let badstates = Bdd.dor badStateBDD (Bdd.dnot dlfBDD) in
  (* iteratively synthesize for greater and greater k until we hit fixpoint *)
  let rec synthesize k paramConstraints =
    Debug.dprintf "mc" "Synthesizing upto %d steps...@," k; Debug.dflush ();
    let result = 
      getParamsForKSteps k paramConstraints mgr transrel 
        initBDD badstates ltltableaulist 
    in
    match result with
    | ExecFixpoint params -> params
    | ExecNonConverged params ->
      synthesize (k + 1) params
  in
  let solbdd = synthesize 0 (mgr#getConstraintsOnParams ()) in
  if (Debug.debugEnabled ()) then
    Debug.dprintf "mc" "Found %e solutions@," (mgr#getNumMinTermsParam solbdd)
  else
    ();
  solbdd

let check mgr transBDDs initBDD badStateBDD dlfbdd ltltableaulist =
  let transrel = conjoinTransitionRels mgr transBDDs in
  let badstates = Bdd.dor badStateBDD (Bdd.dnot dlfbdd) in

  let rec computeReachable states frontier =
    let newStates = Bdd.dor states (post mgr transrel states) in
    if (Bdd.is_leq newStates states) then
      MCSuccess newStates
    else
      (* check if we have a violation *)
      if (not (Bdd.is_inter_empty newStates badstates)) then
        let reachableBadStates = Bdd.dand newStates badstates in
        let trace = findPath mgr initBDD transrel
                             (mgr#cubeOfMinTerm (mgr#pickMinTermOnStates reachableBadStates))
        in
        MCFailureSafety trace
      else
        let newFrontier = Bdd.dand newStates (Bdd.dnot states) in
        computeReachable newStates newFrontier
  in
  Debug.dprintf "mc" "No safety violation found. Proceeding to liveness checks.@,@,";
  Debug.dflush ();

  let reachStat = computeReachable initBDD initBDD in
  match reachStat with 
  | MCFailureSafety trace -> reachStat
  | MCSuccess reachStates ->
     StringMap.fold
       (fun propname tableau acc ->
        let _, _, _, chioftester, tableautrans, jlist, clist = tableau in
        match acc with
        | MCSuccess _ ->
           let ltransrel = Bdd.dand transrel tableautrans in
           let lreachStates = computeFixPoint
                                (fun states -> Bdd.dor states (post mgr ltransrel states))
                                Bdd.is_equal
                                initBDD
           in
           let feasible = getFeasible mgr initBDD lreachStates chioftester ltransrel jlist clist in
           if (not (Bdd.is_false (Bdd.dand (Bdd.dand feasible chioftester) initBDD))) then
             begin
               let badReachableStates = Bdd.dand (Bdd.dand feasible chioftester) initBDD in
               Debug.dprintf "mc" "Violation of property \"%s\" found.@," propname;
               Debug.dprintf "mc" "Initial state which violates property:@,%a@,"
                             Trace.printState (mgr#getStateVars badReachableStates);
               Debug.dflush ();
               let prefix, loop = findLoop mgr initBDD ltransrel feasible jlist clist in
               MCFailureLiveness (propname, prefix, loop)
             end
           else
             MCSuccess feasible
        | MCFailureLiveness _ -> 
           acc
        | _ -> assert false) ltltableaulist reachStat
  | _ -> assert false
