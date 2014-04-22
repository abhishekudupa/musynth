(* model checking and synthesis routines *)

open MusynthTypes
open Cudd
open Format

module AST = MusynthAST
module Debug = MusynthDebug
module Opts = MusynthOptions

type bddType = (Man.d Bdd.t)

(* utility functions for model checking *)
let rec fixPoint pTransformer init =
  let newPred = pTransformer init in
  if newPred = init then 
    newPred
  else
    fixPoint pTransformer newPred

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

(* construct a trace such that the error state is reachable *)
let explain mgr initstates transRel errstate =
  (* construct the set of states reachable in 1, 2, ... k steps *)
  let rec forwardStates stateList reachable frontier =
    let reachPlusFrontier = Bdd.dor reachable frontier in
    if (not (Bdd.is_inter_empty reachPlusFrontier errstate)) then
      stateList
    else
      begin
        let actFrontier = Bdd.dand frontier (Bdd.dnot reachable) in
        let newStateList = actFrontier :: stateList in
        let newPost = post mgr transRel actFrontier in
        forwardStates newStateList reachPlusFrontier newPost
      end
  in
  
  let stateList = forwardStates [] (mgr#makeFalse ()) initstates in
  (* now extract a sequence of pres for the error state in the list *)
  (* Not tail recursive, but WTF? we're already fucked anyway with  *)
  (* a fucking counterexample. Fuck efficiency at this point!       *)
  let k = ref 0 in
  let printer = mgr#getStateVarPrinter () in
  let rec foldStateList myState stateK =
    match stateK with
    | [] -> ()
    | head :: rest ->
       assert (not (Bdd.is_false myState));
       let preMyState = pre mgr transRel myState in
       assert (not (Bdd.is_false preMyState));
       let preMyState = Bdd.dand preMyState head in
       assert (not (Bdd.is_false preMyState));
       let minTerm = mgr#pickMinTermOnStates preMyState in
       foldStateList (mgr#cubeOfMinTerm minTerm) rest;
       Debug.dprintf 3 "Cube %d:@," !k;
       Debug.dprintf 3 "----------------------------------------------------------------------@,";
       Debug.dprintf 3 "%a" (mgr#getCubePrinter ()) minTerm;
       Debug.dprintf 3 "----------------------------------------------------------------------@,@,";
       Debug.dprintf 2 "State %d:@," !k;
       k := !k + 1;
       Debug.dprintf 2 "----------------------------------------------------------------------@,";
       Debug.dprintf 2 "%a" printer minTerm;
       Debug.dprintf 2 "----------------------------------------------------------------------@,@,";
  in
  foldStateList errstate stateList;
  Debug.dprintf 2 "Ending with erroneous state:@,";
  Debug.dprintf 2 "----------------------------------------------------------------------@,";
  let minTerm = mgr#pickMinTermOnStates errstate in
  Debug.dprintf 2 "%a" printer minTerm;
  Debug.dprintf 2 "----------------------------------------------------------------------@,@,"

let countCycles mgr states transRel =
  let rec countCyclesRec mgr states transRel =
    let next = post mgr transRel states in
    let refinedStates = (Bdd.dand next states) in
    if (Bdd.is_equal refinedStates states) then
      mgr#getNumMinTermsState states
    else if (Bdd.is_false refinedStates) then
      0.0
    else
      countCyclesRec mgr refinedStates transRel
  in
  countCyclesRec mgr states transRel

let getParamsForFeasible mgr transRel tableau reachStates =
  ()

let rec synthForwardSafety mgr transrel initStates badstates =
  let itercount = ref 0 in

  let rec computeNextOrCEX reach frontier =
    if (not (Bdd.is_inter_empty frontier badstates)) then
      begin
        let badReachStates = Bdd.dand frontier badstates in
        let errstate = mgr#pickMinTermOnStates badReachStates in
        Debug.dprintf 1 "@,@,Found counter example:@,@,";
        Debug.dprintf 1 "%a@,@," (mgr#getStateVarPrinter ()) errstate;
        Debug.dprintf 1 "With parameter values:@,@,";
        Debug.dprintf 1 "%a@,@," (mgr#getParamVarPrinter ()) errstate;
        Debug.dprintf 2 "Trail to counterexample from initial state:@,@,";
        if (!Opts.debugLevel >= 2) then
          explain mgr initStates transrel (mgr#cubeOfMinTerm errstate)
        else
          ();
        SynthCEX badReachStates
      end
    else
      begin
        let newReach = Bdd.dor reach frontier in
        
        Debug.dprintf 1 "@,@,Iteration %d:@," !itercount;
        Debug.dprintf 1 "Reach has %e states@," 
                      (mgr#getNumMinTermsState reach);
        Debug.dprintf 1 "Frontier has %e states@,"
                      (mgr#getNumMinTermsState frontier);
        Debug.dprintf 1 "newReach has %e states@," 
                      (mgr#getNumMinTermsState newReach);
        Debug.dprintf 1 "BDD size for newReach = %d nodes@," (Bdd.size newReach);
        Debug.dprintf 1 "BDD size for newReach abstracted on params = %d nodes@," 
                      (Bdd.size (Bdd.exist (mgr#getCubeForParamVars ()) newReach));

        (* let ncycles = countCycles mgr newReach transrel in *)
        (* Debug.dprintf 1 "%e states form cycles in newReach@," ncycles; *)
        (* Debug.dflush (); *)

        itercount := !itercount + 1;
        
        if (Bdd.is_leq newReach reach) then
          SynthSafe
        else
          let newStates = Bdd.dand frontier (Bdd.dnot reach) in
          Debug.dprintf 1 "Computing post with %e states@," (mgr#getNumMinTermsState newStates);
          let postNew = post mgr transrel newStates in
          Debug.dprintf 1 "Post has %e states@," (mgr#getNumMinTermsState postNew);
          Debug.dflush ();
          computeNextOrCEX newReach postNew
      end
  in
  computeNextOrCEX (mgr#makeFalse ()) initStates
                   
(* returns the bdd corresponding to the parameter values which work *)
let synthesize mgr transrel initstates badstates tableaulist =

  let iteration = ref 0 in
  
  let rec synthesizeRec refinedInit =
    Debug.dprintf 1 "CEGIS iteration %d...@," !iteration;
    iteration := !iteration + 1;
    Debug.dprintf 1 "Attempting synthesis with %e candidates@,"
                  (mgr#getNumMinTermsParam refinedInit);
    let synthStat = synthForwardSafety mgr transrel refinedInit badstates in
    match synthStat with
    | SynthSafe ->
       Debug.dprintf 1 "Successfully synthesized %e solutions!@," 
                     (mgr#getNumMinTermsParam refinedInit);
       let r = Bdd.exist (mgr#getAllButParamCube ()) refinedInit in
       assert (not (Bdd.is_false r));
       Debug.dprintf 1 "Returning Solutions!@,";
       r
    | SynthCEX cex ->
       let badParamValues = Bdd.exist (mgr#getAllButParamCube ()) cex in
       Debug.dprintf 1 "%e param values eliminated@," (mgr#getNumMinTermsParam badParamValues);
       let newInit = 
         Bdd.dand refinedInit 
                  (Bdd.dnot (Bdd.existand 
                               (mgr#getAllButParamCube ()) 
                               cex badstates))
       in
       assert (not (Bdd.is_false newInit));
       Debug.dprintf 1 "newInit has %e states@," (mgr#getNumMinTermsState newInit);
       if (not (Bdd.is_leq (Bdd.exist (mgr#getCubeForParamVars ()) newInit) 
                           (Bdd.exist (Bdd.dand (mgr#getCubeForPrimedVars ())
                                                (mgr#getCubeForParamVars ())) initstates))) then
         begin
           Debug.dprintf 1 "Synthesis eliminated one or more initial states!@,";
           Debug.dprintf 1 "No solution possible.@,";
           mgr#makeFalse ()
         end
       else
         synthesizeRec newInit
  in
  Debug.dprintf 1 "initStates has %e states@," (mgr#getNumMinTermsState initstates);
  synthesizeRec initstates

(* TODO: Currently hardwired to use monolithic transition *)
(*       Change this to be based on command line option   *)
let synthFrontEnd mgr transBDDs initBDD badStateBDD dlfBDD ltltableaulist =
  let transrel = 
    LLDesigMap.fold 
      (fun name bdd accbdd ->
       Bdd.dand bdd accbdd)
      transBDDs (mgr#makeTrue ()) in
  let badstates = Bdd.dor badStateBDD (Bdd.dnot dlfBDD) in
  synthesize mgr transrel (Bdd.dand initBDD (mgr#getConstraintsOnParams ())) 
             badstates ltltableaulist
