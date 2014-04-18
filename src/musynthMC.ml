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
  fprintf std_formatter "Postimage has %e states\n" (mgr#getNumMinTerms postimage);
  pp_print_flush std_formatter ();
  Bdd.vectorcompose substTable postimage

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
        Bdd.print (mgr#getVarPrinter ()) std_formatter newReach;
        fprintf std_formatter "Found counter example\n"; pp_print_flush std_formatter ();
        SynthCEX newReach
      end
    else
      let newStates = Bdd.dand frontier (Bdd.dnot reach) in
      fprintf std_formatter "Computing post with %e states\n" (mgr#getNumMinTerms newStates);
      pp_print_flush std_formatter ();
      let postNew = post mgr transrel newStates in
      fprintf std_formatter "Post has %e states\n" (mgr#getNumMinTerms postNew);
      pp_print_flush std_formatter ();
      computeNextOrCEX (Bdd.dor reach frontier) postNew
  in
  
  fprintf std_formatter "Transition bdd:\n";
  Bdd.print (mgr#getVarPrinter ()) std_formatter transrel;
  fprintf std_formatter "\n";
  pp_print_flush std_formatter ();
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
       fprintf std_formatter "Conjoining BDD for var %a\n" AST.pLLDesignator name;
       let res = Bdd.dand bdd accbdd in
       fprintf std_formatter "Result\n";
       Bdd.print (mgr#getVarPrinter ()) std_formatter res;
       fprintf std_formatter "\n";
       pp_print_flush std_formatter ();
       res) transBDDs (mgr#makeTrue ()) in
  let badstates = Bdd.dor badStateBDD (Bdd.dnot dlfBDD) in
  synthesize mgr transrel initBDD badstates
