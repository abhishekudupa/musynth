(* model checking and synthesis routines *)

open MusynthTypes
open Cudd
open Format

module AST = MusynthAST

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
  (* fprintf std_formatter "Postimage has %e states\n" (mgr#getNumMinTerms postimage); *)
  (* pp_print_flush std_formatter (); *)
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
  let rec foldStateList myState stateK =
    match stateK with
    | [] -> ()
    | head :: rest ->
       let preMyState = pre mgr transRel myState in
       let preMyState = Bdd.dand preMyState head in
       let minTerm = Bdd.pick_minterm preMyState in
       foldStateList (Bdd.cube_of_minterm (mgr#getManager ()) minTerm) rest;
       fprintf std_formatter "State:\n";
       fprintf std_formatter "----------------------------\n";
       mgr#printStateVarsInCube std_formatter minTerm;
       fprintf std_formatter "\n----------------------------\n\n\n";
  in
  foldStateList errstate stateList


let rec synthForwardSafety mgr transrel initStates badstates =

  let rec computeNextOrCEX reach frontier =
    fprintf std_formatter "Reach has %e states\n" 
            (mgr#getNumMinTerms reach);
    fprintf std_formatter "Frontier has %e states\n"
            (mgr#getNumMinTerms frontier);
    pp_print_flush std_formatter ();
    let newReach = Bdd.dor reach frontier in
    fprintf std_formatter "newReach has %e states\n" 
            (mgr#getNumMinTerms newReach);
    pp_print_flush std_formatter ();

    if (Bdd.is_leq newReach reach) then
      SynthSafe
    else if (not (Bdd.is_inter_empty newReach badstates)) then
      begin
        fprintf std_formatter "Found counter example\n";
        fprintf std_formatter "Counter example has %e states\n" 
                (mgr#getNumMinTerms (Bdd.dand newReach badstates));
        mgr#printCubes 1 std_formatter (Bdd.dand newReach badstates);
        fprintf std_formatter "Counterexample state:\n";
        let errstate = Bdd.pick_minterm (Bdd.dand newReach badstates) in
        mgr#printStateVarsInCube std_formatter errstate;
        fprintf std_formatter "Backwards trail from initial state:\n\n";
        explain mgr initStates transrel (Bdd.cube_of_minterm (mgr#getManager ()) errstate);
        pp_print_flush std_formatter ();
        SynthCEX newReach
      end
    else
      let newStates = Bdd.dand frontier (Bdd.dnot reach) in
      fprintf std_formatter "Computing post with %e states\n" (mgr#getNumMinTerms newStates);
      pp_print_flush std_formatter ();
      let postNew = post mgr transrel newStates in
      fprintf std_formatter "Post has %e states\n" (mgr#getNumMinTerms postNew);
      pp_print_flush std_formatter ();
      computeNextOrCEX newReach postNew
  in
  
  (* fprintf std_formatter "Transition bdd:\n"; *)
  (* Bdd.print (mgr#getVarPrinter ()) std_formatter transrel; *)
  (* fprintf std_formatter "\n"; *)
  (* pp_print_flush std_formatter (); *)
  computeNextOrCEX (mgr#makeFalse ()) initStates

(* returns the bdd corresponding to the parameter values which work *)
let synthesize mgr transrel initstates badstates =
  let ucube = mgr#getCubeForUnprimedVars () in

  let rec synthesizeSafetyRec refinedInit =
    fprintf std_formatter "Attempting synthesis with %e candidates\n"
            (mgr#getNumMinTerms refinedInit);
    pp_print_flush std_formatter ();
    let synthStat = synthForwardSafety mgr transrel refinedInit badstates in
    match synthStat with
    | SynthSafe -> 
       Bdd.exist (mgr#getAllButParamCube ()) refinedInit
    | SynthCEX cex -> 
       let newInit = Bdd.dand refinedInit (Bdd.dnot (Bdd.existand ucube cex badstates)) in
       synthesizeSafetyRec newInit
  in
  fprintf std_formatter "initStates has %e states\n" (mgr#getNumMinTerms initstates);
  pp_print_flush std_formatter ();
  synthesizeSafetyRec initstates

(* TODO: Currently hardwired to use monolithic transition *)
(*       Change this to be based on command line option   *)
let synthFrontEnd mgr transBDDs initBDD badStateBDD dlfBDD =
  Bdd.print (mgr#getVarPrinter ()) std_formatter initBDD;
  fprintf std_formatter "\n";
  let transrel = 
    LLDesigMap.fold 
      (fun name bdd accbdd ->
       Bdd.dand bdd accbdd)
      transBDDs (mgr#makeTrue ()) in
  let badstates = Bdd.dor badStateBDD (Bdd.dnot dlfBDD) in
  synthesize mgr transrel initBDD badstates
