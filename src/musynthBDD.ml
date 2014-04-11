open MusynthTypes
module CK = MusynthASTChecker
module AST = MusynthAST
module Utils = MusynthUtils
open Cudd

exception BddException of string

(* routines for converting to BDDs *)
let rec lg num =
  if num <= 0 then
    raise (Invalid_argument ("lg called with arg <= 0"))
  else
    match num with
    | 1 -> 1
    | 2 -> 1
    | _ -> 1 + (lg (num lsr 1))

let numBitsForValues values =
  let numvalues = List.length values in
  lg numvalues

(* string -> (int * int) * IntMap, where first arg is the low index, second is numbits *)
let varMap = ref (LLDesigMap.empty)

let numTotalBits = ref 0

let bddMan = ref (Man.make_d ())

let resetbddMan () =
  bddMan := Man.make_d ()

let registerVar name (valDomain : llDesignatorT list) = 
  let numBits = numBitsForValues valDomain in
  let curIndex = ref 0 in
  let rep2ValMap, val2RepMap = 
    List.fold_left 
      (fun (acc1, acc2) valu ->
       let retval1 = IntMap.add !curIndex valu acc1 in
       let retval2 = LLDesigMap.add valu !curIndex acc2 in
       curIndex := !curIndex + 1;
       (retval1, retval2)) (IntMap.empty, LLDesigMap.empty) valDomain
  in
  let rec registerVars numVars =
    match numVars with
    | 1 -> 
       let bdd = Bdd.newvar !bddMan in
       [ Bdd.topvar bdd ]
    | _ ->
       let lst = registerVars (numVars - 1) in
       let bdd = Bdd.newvar !bddMan in
       lst @ [ (Bdd.topvar bdd) ]
  in
  let indexList = registerVars numBits in
  let low = List.hd indexList in
  let size = List.length indexList in 
  numTotalBits := !numTotalBits + size;
  Man.group !bddMan low size Man.MTR_DEFAULT;
  varMap := LLDesigMap.add name (low, size, rep2ValMap, val2RepMap) !varMap

let lookupVar name =
  try Some (LLDesigMap.find name !varMap) with Not_found -> None
                 
let registerVarAndPrimed name (valDomain : llDesignatorT list) =
  registerVar name valDomain;
  registerVar (getPrimedLLDesig name) valDomain

let rec mkBddForVal low size valrep = 
  match size with
  | 1 -> 
     if ((valrep land 1) <> 0) then 
       Bdd.ithvar !bddMan low
     else
       Bdd.dnot (Bdd.ithvar !bddMan low)
  | _ -> 
     Bdd.dand 
       (mkBddForVal low (size - 1) valrep)
       (if (((valrep lsr (size - 1)) land 1) <> 0) then
          Bdd.ithvar !bddMan (low + size - 1)
        else
          Bdd.dnot (Bdd.ithvar !bddMan (low + size - 1)))

let rec mkBDDForEqual low1 size1 low2 size2 =
  if size1 <> size2 then
    raise (BddException "Unequal sizes in mkBDDForEqual")
  else
    match size1 with
    | 1 -> Bdd.nxor (Bdd.ithvar !bddMan low1) (Bdd.ithvar !bddMan low2)
    | _ -> Bdd.dand (mkBDDForEqual low1 (size1 - 1) low2 (size2 - 1))
                    (Bdd.nxor (Bdd.ithvar !bddMan (low1 - size1 + 1))
                              (Bdd.ithvar !bddMan (low2 - size2 + 1)))
                    
let rec prop2BDD prop =
  match prop with
  | LLPropTrue -> Bdd.dtrue !bddMan
  | LLPropFalse -> Bdd.dfalse !bddMan
  | LLPropEquals (desig1, desig2) ->
     let l1 = lookupVar desig1 in
     let l2 = lookupVar desig2 in
     begin
       match l1, l2 with
       | Some (low1, size1, _, _), Some (low2, size2, _, _) ->
          mkBDDForEqual low1 size1 low2 size2
       | Some (low, size, r2V, v2R), None ->
          let valrep = 
            (try LLDesigMap.find desig2 v2R
             with Not_found -> 
               raise (BddException ("Invalid value while making BDD: " ^
                                      (lldesigToString desig2))))
          in
          mkBddForVal low size valrep
       | None, Some (low, size, r2V, v2R) ->
          let valrep = 
            (try LLDesigMap.find desig1 v2R
             with Not_found -> 
               raise (BddException ("Invalid value while making BDD: " ^
                                      (lldesigToString desig1))))
          in
          mkBddForVal low size valrep
       | None, None ->
          if desig1 = desig2 then
            Bdd.dtrue !bddMan
          else
            Bdd.dfalse !bddMan
     end
  | LLPropNot prop1 ->
     Bdd.dnot (prop2BDD prop1)
  | LLPropAnd (prop1, prop2) ->
     Bdd.dand (prop2BDD prop1) (prop2BDD prop2)
  | LLPropOr (prop1, prop2) ->
     Bdd.dor (prop2BDD prop1) (prop2BDD prop2)
  | _ -> raise (BddException ("Invalid prop while making BDD: " ^
                                (AST.astToString AST.pLLProp prop)))

          
