(* model checking and synthesis routines *)

open MusynthTypes
open Cudd
open Format

module AST = MusynthAST
module Debug = MusynthDebug
module Opts = MusynthOptions
module Trace = MusynthTrace

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
  let rec foldStateList myState stateK =
    match stateK with
    | [] -> []
    | head :: rest ->
       let preMyState = pre mgr transRel myState in
       let preMyState = Bdd.dand preMyState head in
       let minTerm = mgr#pickMinTermOnStates preMyState in
       let cube = mgr#cubeOfMinTerm minTerm in
       let lst = foldStateList cube rest in
       lst @ [ (mgr#getStateVars cube) ]
  in
  let lst = foldStateList errstate stateList in
  let minTerm = mgr#pickMinTermOnStates errstate in
  let cube = mgr#cubeOfMinTerm minTerm in
  lst @ [ (mgr#getStateVars cube) ]

let getParamsForFeasible mgr transRel tableau initstates k =
  ()

let rec synthForwardSafety mgr transrel initStates badstates =
  let itercount = ref 0 in

  let rec computeNextOrCEX reach frontier =
    if (not (Bdd.is_inter_empty frontier badstates)) then
      begin
        let badReachStates = Bdd.dand frontier badstates in
        let errstate = mgr#pickMinTermOnStates badReachStates in

        if (Debug.debugEnabled ()) then
          begin
            Debug.dprintf "mc" "@,@,Found counter example:@,@,";
            Debug.dprintf "mc" "%a@,@," (mgr#getStateVarPrinter ()) errstate;
            Debug.dprintf "mc" "With parameter values:@,@,";
            Debug.dprintf "mc" "%a@,@," (mgr#getParamVarPrinter ()) errstate;
            Debug.dprintf "trace" "Trail to counterexample from initial state:@,@,";
            if (Debug.debugOptEnabled "trace") then
              let trace = explain mgr initStates transrel (mgr#cubeOfMinTerm errstate) in
              Trace.printTraceSafety trace
            else
              ();
          end
        else
          ();

        SynthCEX badReachStates
      end
    else
      begin
        let newReach = Bdd.dor reach frontier in
        
        if (Debug.debugEnabled ()) then
          begin
            Debug.dprintf "mc" "@,@,Iteration %d:@," !itercount;
            Debug.dprintf "mc" "Reach has %e states@," 
                          (mgr#getNumMinTermsState reach);
            Debug.dprintf "mc" "Frontier has %e states@,"
                          (mgr#getNumMinTermsState frontier);
            Debug.dprintf "mc" "newReach has %e states@," 
                          (mgr#getNumMinTermsState newReach);
            Debug.dprintf "mc" "BDD size for newReach = %d nodes@," (Bdd.size newReach);
            Debug.dprintf "mc" "BDD size for newReach abstracted on params = %d nodes@," 
                          (Bdd.size (Bdd.exist (mgr#getCubeForParamVars ()) newReach))

            (* let ncycles = countCycles mgr newReach transrel in *)
            (* Debug.dprintf 1 "%e states form cycles in newReach@," ncycles; *)
            (* Debug.dflush (); *)
          end
        else
          ();

        itercount := !itercount + 1;
        
        if (Bdd.is_leq newReach reach) then
          SynthSafe
        else
          let newStates = Bdd.dand frontier (Bdd.dnot reach) in
          let postNew = post mgr transrel newStates in
          computeNextOrCEX newReach postNew
      end
  in
  computeNextOrCEX (mgr#makeFalse ()) initStates
                   
(* returns the bdd corresponding to the parameter values which work *)
let synthesize mgr transrel initstates badstates tableaulist =

  let iteration = ref 0 in
  
  let rec synthesizeRec refinedInit =

    if (Debug.debugEnabled ()) then
      begin
        Debug.dprintf "mc" "CEGIS iteration %d...@," !iteration;
        Debug.dprintf "mc" "Attempting synthesis with %e candidates@,"
                      (mgr#getNumMinTermsParam refinedInit)
      end
    else
      ();

    iteration := !iteration + 1;
    let synthStat = synthForwardSafety mgr transrel refinedInit badstates in
    match synthStat with
    | SynthSafe ->
       Debug.dprintf "mc" "Successfully synthesized %e solutions!@," 
                     (mgr#getNumMinTermsParam refinedInit);
       let r = Bdd.exist (mgr#getAllButParamCube ()) refinedInit in
       assert (not (Bdd.is_false r));
       Debug.dprintf "mc" "Returning Solutions!@,";
       r
    | SynthCEX cex ->
       let badParamValues = Bdd.exist (mgr#getAllButParamCube ()) cex in
       Debug.dprintf "mc" "%e param values eliminated@," (mgr#getNumMinTermsParam badParamValues);
       let newInit = 
         Bdd.dand refinedInit 
                  (Bdd.dnot (Bdd.existand 
                               (mgr#getAllButParamCube ()) 
                               cex badstates))
       in
       assert (not (Bdd.is_false newInit));
       Debug.dprintf "mc" "newInit has %e states@," (mgr#getNumMinTermsState newInit);
       if (not (Bdd.is_leq (Bdd.exist (mgr#getCubeForParamVars ()) newInit) 
                           (Bdd.exist (Bdd.dand (mgr#getCubeForPrimedVars ())
                                                (mgr#getCubeForParamVars ())) initstates))) then
         begin
           Debug.dprintf "mc" "Synthesis eliminated one or more initial states!@,";
           Debug.dprintf "mc" "No solution possible.@,";
           mgr#makeFalse ()
         end
       else
         synthesizeRec newInit
  in
  Debug.dprintf "mc" "initStates has %e states@," (mgr#getNumMinTermsState initstates);
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
