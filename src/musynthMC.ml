(* model checking and synthesis routines *)

open MusynthTypes
module DD = MusynthBDD

let rec fixPoint pTransformer init =
  let newPred = pTransformer init in
  if newPred = init then 
    newPred
  else
    fixPoint pTransformer newPred

let rec synthForwardSafety transrel badstates =
  let rec computeNext reach frontier =
    if (Bdd.eq (Bdd.dor reach frontier) reach) then
      

let synthesize prog transrel badstates =
  ()
