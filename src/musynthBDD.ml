open MusynthTypes
module CK = MusynthASTChecker
module AST = MusynthAST
open Cudd

(* routines for converting to BDDs *)
let rec lg num =
  if num <= 0 then
    raise (Invalid_argument ("lg called with <= 0 arg"))
  else
    match num with
    | 1 -> 1
    | 2 -> 1
    | _ -> 1 + (lg (num lsr 1))

let numBitsForValues values =
  let numvalues = List.length values in
  lg numvalues

module IntMap = Map.Make
    (struct
      type t = int
      let compare = Pervasives.compare
    end)

(* string -> (int * int) * IntMap, where first arg is the low index, second is numbits *)
let varMap = ref (StringMap.empty)

let numTotalBits = ref 0

let bddMan = Man.make_d ()

let registerVar name (valDomain : string list) = 
  let numBits = numBitsForValues valDomain in
  let curIndex = ref 0 in
  let valmap = 
    List.fold_left 
      (fun acc valu ->
        let retval = IntMap.add !curIndex valu acc in
        curIndex := !curIndex + 1;
        retval) IntMap.empty valDomain
  in
  let rec registerVars numVars =
    match numVars with
    | 1 -> 
        let bdd = Bdd.newvar bddMan in
        [ Bdd.topvar bdd ]
    | _ ->
        let lst = registerVars (numVars - 1) in
        let bdd = Bdd.newvar bddMan in
        [ Bdd.topvar bdd ] @ lst
  in
  let indexList = registerVars numBits in
  let low = List.hd indexList in
  let size = List.length indexList in 
  numTotalBits := !numTotalBits + size;
  Man.group bddMan low size Man.MTR_DEFAULT;
  varMap := StringMap.add name (low, size, valmap) !varMap
  
let registerVarAndPrimed name (valDomain : string list) =
  registerVar name valDomain;
  registerVar (name ^ "'") valDomain
