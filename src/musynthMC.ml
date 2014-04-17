(* model checking and synthesis routines *)

open MusynthTypes
open Cudd
open Format

type bddType = (Man.d Bdd.t)

(* utility functions for model checking *)
let rec fixPoint pTransformer init =
  let newPred = pTransformer init in
  if newPred = init then 
    newPred
  else
    fixPoint pTransformer newPred

let post ucube substTable transRel states =
  let postimage = Bdd.existand ucube states transRel in
  Bdd.vectorcompose substTable postimage


let rec synthForwardSafety mgr transrel initStates badstates =
  let ucube = mgr#getCubeForUnprimedVars () in
  let substTable = mgr#getSubstTableP2U () in

  let rec computeNextOrCEX reach frontier =
    fprintf std_formatter "Reach has %e states\n" 
            (mgr#getNumMinTerms reach);
    pp_print_flush std_formatter ();
    let newReach = Bdd.dor reach frontier in
    fprintf std_formatter "newReach has %e states\n" 
            (mgr#getNumMinTerms reach);
    pp_print_flush std_formatter ();

    if (Bdd.is_leq newReach reach) then
      SynthSafe
    else if (not (Bdd.is_inter_empty newReach badstates)) then
      SynthCEX newReach
    else
      let newStates = Bdd.dand frontier (Bdd.dnot reach) in
      let postNew = post ucube substTable transrel newStates in
      computeNextOrCEX (Bdd.dor reach frontier) postNew
  in
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
       Bdd.exist ucube refinedInit
    | SynthCEX cex -> 
       let newInit = Bdd.dand refinedInit (Bdd.dnot (Bdd.exist ucube cex)) in
       synthesizeSafetyRec newInit
  in
  synthesizeSafetyRec initstates

(* TODO: Currently hardwired to use monolithic transition *)
(*       Change this to be based on command line option   *)
let synthFrontEnd mgr transBDDs initBDD badStateBDD dlfBDD =
  let transrel = 
    LLDesigMap.fold 
      (fun name bdd accbdd ->
       Bdd.dand bdd accbdd) transBDDs (mgr#makeTrue ()) in
  let badstates = Bdd.dor badStateBDD (Bdd.dnot dlfBDD) in
  synthesize mgr transrel initBDD badstates
