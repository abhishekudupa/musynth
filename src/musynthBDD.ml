open MusynthTypes
module CK = MusynthASTChecker
module AST = MusynthAST
module Utils = MusynthUtils
open Cudd
module Opts = MusynthOptions
open Format

exception BddException of string

class bddEncoder =
  object (self)
    val mutable manager = Man.make_d ()
    val mutable numTotalBits = 0
    val mutable var2DescMap = LLDesigMap.empty
    val mutable bit2NameMap = IntMap.empty
    val mutable stateVars = LLDesigMap.empty
    val mutable paramVars = LLDesigSet.empty
    val mutable varConstraints = LLDesigMap.empty
                              
    (* caches *)
    val mutable cachedP2UCube = None
    val mutable cachedU2PCube = None
    val mutable cachedP2USubstTable = None
    val mutable cachedU2PSubstTable = None

    method reset () =
      manager <- Man.make_d ();
      numTotalBits <- 0;
      var2DescMap <- LLDesigMap.empty;
      bit2NameMap <- IntMap.empty;
      stateVars <- LLDesigMap.empty;
      paramVars <- LLDesigSet.empty

    method private lg num =
      let rec lgInternal num =
        if num <= 0 then
          raise (Invalid_argument ("bddEncoder#lg called with argument <= 0"))
        else
          match num with
          | 1 -> 1
          | 2 -> 1
          | _ -> 1 + (lgInternal (num lsr 1))
      in
      lgInternal num

    method private numBitsForValues values =
      self#lg (List.length values)

    method registerVar name valDomain =
      try 
        let _ = LLDesigMap.find name var2DescMap in
        raise (BddException ("Variable \"" ^ (lldesigToString name) ^ 
                               "\" has already been registered"))
      with 
      | Not_found ->
         begin
           let nameAsStr = lldesigToString name in
           let numBits = self#numBitsForValues valDomain in
           let r2vMap, v2rMap, _ =
             List.fold_left 
               (fun (ar2vMap, av2rMap, count) v ->
                let r1 = IntMap.add count v ar2vMap in
                let r2 = LLDesigMap.add v count av2rMap in
                (r1, r2, count + 1))
               (IntMap.empty, LLDesigMap.empty, 0)
               valDomain
           in
           
           let rec registerBits numBits =
             match numBits with
             | 1 -> 
                let _ = Bdd.ithvar bddMan numTotalBits in
                let retval = [ numTotalBits ] in
                numTotalBits <- numTotalBits + 1;
                retval
             | _ ->
                let lst = registerBits (numBits - 1) in
                let _ = Bdd.ithvar bddMan numTotalBits in
                let retval = lst @ numTotalBits in
                numTotalBits <- numTotalBits + 1;
                retval
           in
           let indexList = registerBits numBits in
           List.iter 
             (fun idx ->
              bit2NameMap <- IntMap.add idx 
                                        (nameAsStr ^ "." ^ (string_of_int idx)) 
                                        bit2NameMap) indexList;
           let low = List.hd indexList in
           let size = List.length indexList in
           Man.group bddMan low size Man.MTR_DEFAULT;
           var2DescMap <- LLDesigMap.add name (low, size, r2vMap, v2rMap);

           if !Opts.debugLevel >= 1 then
             begin
               fprintf std_formatter
                       "bddEncoder: Created group with %d variables for %s, total bits used = %d\n"
                       size (lldesigToString name) numTotalBits;
               pp_print_flush std_formatter ()
             end
           else
             ()
         end
           
    method mkBDDForVal low size valrep = 
      ()
               
  end

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

(* global variables for state of bdd encoding *)
(* string -> (int * int) * IntMap, where first arg is the low index, second is numbits *)
let varMap = ref (LLDesigMap.empty)
let numTotalBits = ref 0
let bddMan = ref (Man.make_d ())
let stateVars = ref (LLDesigMap.empty)
let paramVars = ref (LLDesigSet.empty)

let reset () =
  bddMan := Man.make_d ();
  varMap := LLDesigMap.empty;
  numTotalBits := 0;
  stateVars := LLDesigMap.empty;
  paramVars := LLDesigSet.empty

let makeMaps valDomain =
  let acc1, acc2, _ = 
    List.fold_left
      (fun (acc1, acc2, count) valu ->
       let r1 = IntMap.add count valu acc1 in
       let r2 = LLDesigMap.add valu count acc2 in
       (r1, r2, count + 1)) (IntMap.empty, LLDesigMap.empty, 0) valDomain
  in
  (acc1, acc2)
     

let registerVar name (valDomain : llDesignatorT list) = 
  let numBits = numBitsForValues valDomain in
  let rep2ValMap, val2RepMap = makeMaps valDomain in
  let rec registerVars numVars =
    match numVars with
    | 1 -> 
       let _ = Bdd.ithvar !bddMan !numTotalBits in
       numTotalBits := !numTotalBits + 1;
       [ !numTotalBits - 1 ]
    | _ ->
       let lst = registerVars (numVars - 1) in
       let _ = Bdd.ithvar  !bddMan !numTotalBits in
       numTotalBits := !numTotalBits + 1;
       lst @ [ !numTotalBits - 1 ]
  in
  let indexList = registerVars numBits in
  let low = List.hd indexList in
  let size = List.length indexList in 
  Man.group !bddMan low size Man.MTR_DEFAULT;
  varMap := LLDesigMap.add name (low, size, rep2ValMap, val2RepMap) !varMap;


  (low, size, rep2ValMap, val2RepMap)

let lookupVar name =
  try Some (LLDesigMap.find name !varMap) with Not_found -> None
                 
let registerVarAndPrimed name (valDomain : llDesignatorT list) =
  let ret1 = registerVar name valDomain in
  let ret2 = registerVar (getPrimedLLDesig name) valDomain in
  (ret1, ret2)
                         

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


let registerStateVariable name valdomain =
  try
    let (low, size, r2vMap, v2rMap)  = LLDesigMap.find name !varMap in
    let (r1, r2) = makeMaps valdomain in
    let s = numBitsForValues valdomain in
      
    if (size <> s || (not (IntMap.equal (fun a b -> a = b) r2vMap r1)) || 
          (not (LLDesigMap.equal (fun a b -> a = b) v2rMap r2))) then
      raise (BddException ("State variable \"" ^ (lldesigToString name) ^ "\" already registered!"))
    else
      let r2 = LLDesigMap.find (getPrimedLLDesig name) !varMap in
      ((low, size, r2vMap, v2rMap), r2)
  with Not_found ->
    begin
      let rv = registerVarAndPrimed name valdomain in
      stateVars := LLDesigMap.add name (getPrimedLLDesig name) !stateVars;
      rv
    end
  
let registerParamVariable name valdomain =
  try
    let (low, size, r2vMap, v2rMap) = LLDesigMap.find name !varMap in
    let r1, r2 = makeMaps valdomain in 
    let s = numBitsForValues valdomain in

    if (size <> s || (not (IntMap.equal (fun a b -> a = b) r2vMap r1)) || 
          (not (LLDesigMap.equal (fun a b -> a = b) v2rMap r2))) then
      raise (BddException ("Parameter \"" ^ (lldesigToString name) ^ "\" already registered!"))
    else
      (low, size, r2vMap, v2rMap)
  with Not_found ->
    begin
      let rv = registerVar name valdomain in
      paramVars := LLDesigSet.add name !paramVars;
      rv
    end

let getCubeForOneVar varname =
  let (low, size, _, _) = LLDesigMap.find varname !varMap in
  let rec mkCube low size =
    match size with
    | 0 -> assert false
    | 1 -> Bdd.ithvar !bddMan low
    | _ -> Bdd.dand (Bdd.ithvar !bddMan (low + (size - 1))) (mkCube low (size - 1))
  in
  mkCube low size

(* get a cube representing all the unprimed state variables *)
let getCubeForUnprimedVars () =
  LLDesigMap.fold 
    (fun vname pname bdd ->
     Bdd.dand (getCubeForOneVar vname) bdd) 
    !stateVars 
    (Bdd.dtrue !bddMan)

(* get a cube representing all the primed state variables *)
let getCubeForPrimedVars () =
  LLDesigMap.fold 
    (fun vname pname bdd ->
     Bdd.dand (getCubeForOneVar pname) bdd) 
    !stateVars 
    (Bdd.dtrue !bddMan)

let substOneVarInTable table varname svarname =
  let (low1, size1, _, _) = LLDesigMap.find varname !varMap in
  let (low2, size2, _, _) = LLDesigMap.find svarname !varMap in
  if size1 <> size2 then
    raise (Invalid_argument "Sizes not equal in substOneVarInTable ()")
  else
    let rec subst fromidx toidx size =
      match size with
      | 0 -> ()
      | _ -> 
         table.(fromidx + (size -1)) <- Bdd.ithvar !bddMan (toidx + (size - 1));
         subst fromidx toidx (size - 1)
    in
    subst low1 low2 size1

(* get a table that substitutes primed |--> unprimed *)
let getSubstTableP2U () =
  let table = Array.make (Man.get_bddvar_nb !bddMan) (Bdd.dtrue !bddMan) in
  (* set to the identity mapping first! *)
  let table = Array.mapi (fun i elem -> Bdd.ithvar !bddMan i) table in
  (* set the primed |--> unprimed mappings *)
  LLDesigMap.iter 
    (fun vname pname ->
     substOneVarInTable table pname vname) !stateVars;
  table

(* get a table that substitutes unprimed |--> primed *)
let getSubstTableU2P () =
  let table = Array.make (Man.get_bddvar_nb !bddMan) (Bdd.dtrue !bddMan) in
  (* set to the identity mapping first! *)
  let table = Array.mapi (fun i elem -> Bdd.ithvar !bddMan i) table in
  (* set the primed |--> unprimed mappings *)
  LLDesigMap.iter 
    (fun vname pname ->
     substOneVarInTable table vname pname) !stateVars;
  table

let prop2BDD prop =
  let rec prop2BDDInt prop = 
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
       Bdd.dnot (prop2BDDInt prop1)
    | LLPropAnd (prop1, prop2) ->
       Bdd.dand (prop2BDDInt prop1) (prop2BDDInt prop2)
    | LLPropOr (prop1, prop2) ->
       Bdd.dor (prop2BDDInt prop1) (prop2BDDInt prop2)
    | _ -> raise (BddException ("Invalid prop while making BDD: " ^
                                  (AST.astToString AST.pLLProp prop)))
  in
  let cprop = Utils.canonicalizePropFP prop in
  prop2BDDInt cprop
