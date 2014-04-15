(* model checking and synthesis routines *)

open MusynthTypes
module DD = MusynthBDD
open Cudd

(* utility functions for model checking *)
let rec fixPoint pTransformer init =
  let newPred = pTransformer init in
  if newPred = init then 
    newPred
  else
    fixPoint pTransformer newPred

let post transrel states =
  ()

type bddType = (Man.d Bdd.t)


let rec synthForwardSafety transrel initStates badstates =
  let rec computeNext reach frontier =
    let newReach = Bdd.dor reach frontier in
    if (Bdd.is_leq newReach reach) then
      SynthSafe
    else
      begin
        let actFrontier = Bdd.dand frontier (Bdd.dnot reach) in
        SynthSafe
      end
  in
  ()

let synthesize prog transrel badstates =
  ()
