open MusynthTypes
module CK = MusynthASTChecker
module AST = MusynthAST
module Utils = MusynthUtils
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
  let val2SMap = 
    List.fold_left 
      (fun acc valu ->
       let retval = IntMap.add !curIndex valu acc in
       curIndex := !curIndex + 1;
       retval) IntMap.empty valDomain
  in
  curIndex := 0;
  let s2ValMap = 
    List.fold_left 
      (fun acc valu ->
       let retval = StringMap.add valu !curIndex acc in
       curIndex := !curIndex + 1;
       retval) StringMap.empty valDomain
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
  varMap := StringMap.add name (low, size, val2SMap, s2ValMap) !varMap

let lookupVar name =
  StringMap.find name !varMap
                 
let registerVarAndPrimed name (valDomain : string list) =
  registerVar name valDomain;
  registerVar (name ^ "'") valDomain

let rec mkBddForVal low size valrep = 
  match size with
  | 1 -> 
     Bdd.nxor 
       (Bdd.ithvar bddMan low) 
       (if ((valrep land 1) <> 0) then Bdd.dtrue bddMan else Bdd.dfalse bddMan)
  | _ -> 
     Bdd.dand 
       (mkBddForVal low (size - 1) valrep)
       (Bdd.nxor
          (Bdd.ithvar bddMan (low + size - 1))
          (if ((valrep lsr (size - 1)) <> 0) then 
             Bdd.dtrue bddMan 
           else 
             Bdd.dfalse bddMan))

let rec prop2BDD prop =
  match prop with
  | PropTrue _ -> Bdd.dtrue bddMan
  | PropFalse _ -> Bdd.dfalse bddMan
  | PropEquals (desig1, desig2, _) ->
     let s1 = AST.astToString AST.pDesignator desig1 in
     let s2 = AST.astToString AST.pDesignator desig2 in
     (* one of these must be a var *)
     (try
         let var, valu = 
           (try (lookupVar s1, s2)
            with Not_found -> 
              lookupVar s2, s1)
         in
         let low, size, val2SMap, s2valMap = var in
         let valrep = StringMap.find valu s2valMap in
         mkBddForVal low size valrep
       with Not_found -> if s1 = s2 then Bdd.dtrue bddMan else Bdd.dfalse bddMan)
  | PropNEquals (desig1, desig2, _) ->
     prop2BDD (PropNot (PropEquals (desig1, desig2, None), None))
  | PropNot (prop1, _) ->
     Bdd.dnot (prop2BDD prop1)
  | PropAnd (prop1, prop2, _) ->
     Bdd.dand (prop2BDD prop1) (prop2BDD prop2)
  | PropOr (prop1, prop2, _) ->
     Bdd.dor (prop2BDD prop1) (prop2BDD prop2)
  | PropImplies (prop1, prop2, _) ->
     Bdd.dor (Bdd.dnot (prop2BDD prop1)) (prop2BDD prop2)
  | PropIff (prop1, prop2, _) ->
     Bdd.nxor (prop2BDD prop1) (prop2BDD prop2)
  | _ -> assert false
