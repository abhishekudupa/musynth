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
       Debug.dprintf 2 "Constructing pre...@,";
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


let rec synthForwardSafety mgr transrel initStates badstates =
  let itercount = ref 0 in
  let rec computeNextOrCEX reach frontier =
    let newReach = Bdd.dor reach frontier in

    Debug.dprintf 1 "@,@,Iteration %d:@," !itercount;
    Debug.dprintf 1 "Reach has %e states@," 
                  (mgr#getNumMinTerms reach);
    Debug.dprintf 1 "Frontier has %e states@,"
                  (mgr#getNumMinTerms frontier);
    Debug.dprintf 1 "newReach has %e states@," 
                  (mgr#getNumMinTerms newReach);
    Debug.dprintf 1 "BDD size for newReach = %d nodes@," (Bdd.size newReach);

    itercount := !itercount + 1;

    if (Bdd.is_leq newReach reach) then
      SynthSafe
    else if (not (Bdd.is_inter_empty newReach badstates)) then
      begin
        let badReachStates = Bdd.dand newReach badstates in
        let errstate = mgr#pickMinTermOnStates badReachStates in
        Debug.dprintf 1 "@,@,Found counter example:@,@,";
        Debug.dprintf 1 "%a@,@," (mgr#getStateVarPrinter ()) errstate;
        Debug.dprintf 2 "Trail to counterexample from initial state:@,@,";
        if (!Opts.debugLevel >= 2) then
          explain mgr initStates transrel (mgr#cubeOfMinTerm errstate)
        else
          ();
        SynthCEX newReach
      end
    else
      let newStates = Bdd.dand frontier (Bdd.dnot reach) in
      Debug.dprintf 1 "Computing post with %e states@," (mgr#getNumMinTerms newStates);
      let postNew = post mgr transrel newStates in
      Debug.dprintf 1 "Post has %e states@," (mgr#getNumMinTerms postNew);
      computeNextOrCEX newReach postNew
  in
  computeNextOrCEX (mgr#makeFalse ()) initStates

(* returns the bdd corresponding to the parameter values which work *)
let synthesize mgr transrel initstates badstates =
  let ucube = mgr#getCubeForUnprimedVars () in
  let iteration = ref 0 in

  let rec synthesizeSafetyRec refinedInit =
    Debug.dprintf 1 "CEGIS iteration %d...@," !iteration;
    iteration := !iteration + 1;
    Debug.dprintf 1 "Attempting synthesis with %e candidates@,"
                  (mgr#getNumMinTerms refinedInit);
    let synthStat = synthForwardSafety mgr transrel refinedInit badstates in
    match synthStat with
    | SynthSafe ->
       Debug.dprintf 1 "Successfully synthesized solution!@,";
       let r = Bdd.exist (mgr#getAllButParamCube ()) refinedInit in
       assert (not (Bdd.is_false r));
       Debug.dprintf 1 "Returning Solution!@,";
       r
    | SynthCEX cex -> 
       let newInit = Bdd.dand refinedInit (Bdd.dnot (Bdd.existand ucube cex badstates)) in
       synthesizeSafetyRec newInit
  in
  Debug.dprintf 1 "initStates has %e states@," (mgr#getNumMinTerms initstates);
  synthesizeSafetyRec initstates

(* TODO: Currently hardwired to use monolithic transition *)
(*       Change this to be based on command line option   *)
let synthFrontEnd mgr transBDDs initBDD badStateBDD dlfBDD =
  let transrel = 
    LLDesigMap.fold 
      (fun name bdd accbdd ->
       Bdd.dand bdd accbdd)
      transBDDs (mgr#makeTrue ()) in
  let badstates = Bdd.dor badStateBDD (Bdd.dnot dlfBDD) in
  synthesize mgr transrel (Bdd.dand initBDD (mgr#getConstraintsOnParams ())) badstates
