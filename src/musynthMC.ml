(* model checking and synthesis routines *)

open MusynthTypes
open Cudd
open Format

module AST = MusynthAST
module Debug = MusynthDebug
module Opts = MusynthOptions
module Trace = MusynthTrace
module MGR = MusynthBDDManager
module MCU = MusynthMCUtils
module Utils = MusynthUtils

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
          let trace = MCU.findPath mgr initStates transRel 
                                   (mgr#cubeOfMinTerm (mgr#pickMinTermOnStates 
                                                         reachableBadStates))
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
  let iteration = ref 0 in
  
  let rec elimCycles transRel states = 
    Debug.dprintf "mc" "Elim Cycles: iteration %d@," !iteration;
    iteration := !iteration + 1;
    Debug.dprintf "mc" "Elim Cycles: Finding Cycles@,";
    Debug.dflush (); 
    let newStates = MCU.computeFixPoint (MCU.preAndTransformer mgr transRel)
                                        MCU.inclusionFixPointTester states
    in
    (* Filter based on justices *)
    Debug.dprintf "mc" ("Elim Cycles: Filtering on %d justice requirements. " ^^ 
                          "newStates : %d nodes@,")
                  (List.length jlist) (Bdd.size newStates);
    Debug.dflush ();

    let newStates = 
      List.fold_left
        (fun states justiceSpec ->
         let justStates = Bdd.dand states justiceSpec in
         let newTrans = Bdd.dand transRel states in
         MCU.computeFixPoint (MCU.preOrTransformer mgr newTrans)
                             MCU.inclusionFixPointTester justStates)
        newStates jlist
    in
    (* filter based on compassion *)
    Debug.dprintf "mc" ("Elim Cycles: Filtering on %d compassion requirements. " ^^ 
                          "newStates : %d nodes@,")
                  (List.length clist) (Bdd.size newStates);
    Debug.dflush ();

    let newStates = 
      List.fold_left
        (fun states compassionSpec ->
         let newTrans = Bdd.dand transRel states in
         let p, q = compassionSpec in
         let cStates1 = Bdd.dand states (Bdd.dnot p) in
         let cStates2 = Bdd.dand states q in
         let cStates2 = MCU.computeFixPoint (MCU.preOrTransformer mgr newTrans)
                                            MCU.inclusionFixPointTester cStates2
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
  Debug.dprintf "mc" "Computing backward fixpoint on newStates: %d nodes@," (Bdd.size newStates);
  Debug.dflush ();
  MCU.computeFixPoint (MCU.preOrTransformer mgr transRel) MCU.inclusionFixPointTester newStates

let getParamsForInfeasible mgr initStates reach chioftester origTransRel jlist clist propName =
  let feasibleStates = getFeasible mgr initStates reach chioftester 
                                   origTransRel jlist clist 
  in
  Debug.dprintf "mc" "Feasible states (liveness) BDD has %d nodes@," 
                (Bdd.size feasibleStates);
  Debug.dflush ();
  
  if (not (Bdd.is_false (Bdd.dand feasibleStates chioftester))) then
    Debug.dprintf "mc" "Feasible and chi of tester is not empty@,"
  else
    Debug.dprintf "mc" "Feasible and chi of tester is empty@,";
  
  let feasibleBadStates = Bdd.dand (Bdd.dand feasibleStates chioftester) initStates in
  Debug.dprintf "mc" "Feasible BAD states (liveness) BDD has %d nodes@," 
                (Bdd.size feasibleStates);
  Debug.dflush ();
  (* we should not have any feasible cycles! *)
  (* eliminate params which are responsible for a feasible cycle *)
  if (Bdd.is_false feasibleBadStates) then
    mgr#makeTrue ()
  else
    begin

      if (Debug.debugEnabled ()) then
        begin
          let fmt = Debug.getDebugFmt () in
          Debug.dprintf "mc" "Found a liveness violation of property \"%s\":@," 
                        propName; 
          Debug.dflush ();
          if (Debug.debugOptEnabled "trace") then
            let prefix, period = MCU.findLoop mgr initStates origTransRel 
                                              feasibleStates jlist clist in
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
  Debug.dprintf "mc" "Synthesizing completions safe upto %d steps with %e candidates@,"
                k (mgr#getNumMinTermsParam actInitStates); 
  Debug.dflush ();
  let kReachStat = MCU.postK k mgr transRel actInitStates in
  let kReach = 
    (match kReachStat with
     | ExecNonConverged s -> s
     | ExecFixpoint s -> s) 
  in
  Debug.dprintf "mc" "Reachable (safety) BDD has %d nodes@," (Bdd.size kReach);
  Debug.dflush ();
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
       Debug.dprintf "mc" ("Synthesizing completions with no liveness violation " ^^ 
                             "on property \"%s\"" ^^ " upto %d steps with %e candidates@,") 
                     propname k (mgr#getNumMinTermsParam actInitStates);
       Debug.dflush ();
       let _, _, _, chioftester, tableautrans, jlist, clist = tableau in
       let ltransRel = Bdd.dand transRel tableautrans in
       let kReachStat = MCU.postK k mgr transRel actInitStates in
       let kReach =
         (match kReachStat with
          | ExecNonConverged s -> s
          | ExecFixpoint s -> s) 
       in
       Debug.dprintf "mc" "Reachable (liveness) BDD has %d nodes@," (Bdd.size kReach);
       Debug.dflush ();
       let sparams = getParamsForInfeasible mgr actInitStates kReach chioftester 
                                            ltransRel jlist clist propname
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
let synthFrontEndInternal mgr paramConstraints transBDDs initBDD badStateBDD dlBDD ltltableaulist =
  let transrel = conjoinTransitionRels mgr transBDDs in
  let badstates = Bdd.dor badStateBDD dlBDD in
  (* iteratively synthesize for greater and greater k until we hit fixpoint *)
  let rec synthesize k paramConstraints =
    mgr#minimize ();
    Debug.dprintf "mc" "Synthesizing upto %d steps...@," k; Debug.dflush ();
    let result =
      getParamsForKSteps k paramConstraints mgr transrel
        initBDD badstates ltltableaulist
    in
    match result with
    | ExecFixpoint params -> params
    | ExecNonConverged params ->
      synthesize (k + !Opts.jumpStep) params
  in
  let solbdd = synthesize 0 paramConstraints in
  if (Debug.debugEnabled ()) then
    Debug.dprintf "mc" "Found %e solutions@," (mgr#getNumMinTermsParam solbdd)
  else
    ();
  solbdd

let synthFrontEnd mgr transBDDs initBDD badStateBDD dlBDD ltltableaulist =
  let transRel = conjoinTransitionRels mgr transBDDs in
  (* set initial params to false *)
  let paramNames = mgr#getParamVarNames () in
  let paramConstProp = 
    List.fold_left 
      (fun prop name -> LLPropAnd (LLPropEquals (name, Utils.makeDeferDesig ()), prop))
      LLPropTrue paramNames
  in
  let paramConstraints = mgr#prop2BDD paramConstProp in
  let reachableStates = MCU.computeFixPoint 
                          (MCU.postOrTransformer mgr transRel)
                          MCU.inclusionFixPointTester (Bdd.dand initBDD paramConstraints)
  in
  let deadlockedReachable = Bdd.dand reachableStates dlBDD in
  if (Bdd.is_false deadlockedReachable) then
    begin
      Debug.dprintf "mc" "No Deadlock Found@,";
      mgr#makeFalse ()
    end
  else
    begin
      let rstates = Bdd.exist (mgr#getCubeForParamVars ()) reachableStates in
      Debug.dprintf "mc" "%e Deadlocked states.@," (Bdd.nbminterms (mgr#getNumStateBits ())
                                                                   (Bdd.existand (mgr#getCubeForParamVars ())
                                                                                 rstates dlBDD));

      let dlParams = Bdd.existand (mgr#getCubeForUnprimedVars ()) rstates dlBDD in
      let dlsupport = Bdd.support dlParams in
      let dlSupportSize = Bdd.supportsize dlParams in
      Debug.dprintf "mc" "Support of deadlock has size %d:@," dlSupportSize;
      Debug.dprintf "mc" "%a@," (Bdd.print (mgr#getBitPrinter ())) (Bdd.support dlParams);
      Debug.dprintf "mc" "DL on support has %e cubes.@," (Bdd.nbminterms dlSupportSize dlParams);
      let newConstraints = Bdd.exist dlsupport paramConstraints in
      let newConstraints = Bdd.dand newConstraints (Bdd.dnot dlParams) in
      let newConstraints = Bdd.dand newConstraints (mgr#getConstraintsOnParams ()) in
      Debug.dprintf "mc" "New param constraints have %e cubes.@," 
                    (Bdd.nbminterms (mgr#getNumParamBits ()) newConstraints);
      synthFrontEndInternal mgr newConstraints transBDDs initBDD badStateBDD dlBDD ltltableaulist
    end


let check mgr transBDDs initBDD badStateBDD dlfbdd ltltableaulist =
  let transrel = conjoinTransitionRels mgr transBDDs in
  let badstates = Bdd.dor badStateBDD (Bdd.dnot dlfbdd) in
  let reachIter = ref 0 in

  let rec computeReachable states frontier =
    reachIter := !reachIter + 1;
    let newStates = Bdd.dor states (MCU.post mgr transrel states) in
    if (Bdd.is_leq newStates states) then
      MCSuccess newStates
    else
      (* check if we have a violation *)
      if (not (Bdd.is_inter_empty newStates badstates)) then
        let reachableBadStates = Bdd.dand newStates badstates in
        let trace = MCU.findPath mgr initBDD transrel
                                 (mgr#cubeOfMinTerm (mgr#pickMinTermOnStates reachableBadStates))
        in
        MCFailureSafety trace
      else
        let newFrontier = Bdd.dand newStates (Bdd.dnot states) in
        computeReachable newStates newFrontier
  in

  let reachStat = computeReachable initBDD initBDD in
  match reachStat with
  | MCFailureSafety trace -> reachStat
  | MCSuccess reachStates ->
     Debug.dprintf "mc" ("No safety violation found. Fixpoint in %d iterations. " ^^
                           "Proceeding to liveness checks.@,@,") !reachIter;
     StringMap.fold
       (fun propname tableau acc ->
        let _, _, _, chioftester, tableautrans, jlist, clist = tableau in
        match acc with
        | MCSuccess _ ->
           let ltransrel = Bdd.dand transrel tableautrans in
           let lreachStates = 
             MCU.computeFixPoint (MCU.postOrTransformer mgr ltransrel) 
                                 MCU.inclusionFixPointTester initBDD
           in
           let feasible = getFeasible mgr initBDD lreachStates chioftester ltransrel jlist clist in
           if (not (Bdd.is_false (Bdd.dand (Bdd.dand feasible chioftester) initBDD))) then
             begin
               let badReachableStates = Bdd.dand (Bdd.dand feasible chioftester) initBDD in
               Debug.dprintf "mc" "Violation of property \"%s\" found.@," propname;
               Debug.dprintf "mc" "Initial state which violates property:@,%a@,"
                             Trace.printState (mgr#getStateVars badReachableStates);
               Debug.dflush ();
               let prefix, loop = MCU.findLoop mgr initBDD ltransrel feasible jlist clist in
               MCFailureLiveness (propname, prefix, loop)
             end
           else
             MCSuccess feasible
        | MCFailureLiveness _ -> 
           acc
        | _ -> assert false) ltltableaulist reachStat
  | _ -> assert false
