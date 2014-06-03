open MusynthTypes
open Cudd
open Format

module CK = MusynthASTChecker
module AST = MusynthAST
module Utils = MusynthUtils
module Opts = MusynthOptions
module Debug = MusynthDebug

type bddType = Cudd.Man.d Cudd.Bdd.t

class bddManager =
object (self)
  val mutable manager = Man.make_d ()
  val mutable numTotalBits = 0
  val mutable numStateBits = 0
  val mutable numInternalStateBits = 0
  val mutable numParamBits = 0
  val mutable bitNameToBddMap = StringMap.empty
  val mutable indexToBitNameMap = IntMap.empty
  val mutable varMap = LLDesigMap.empty
  val mutable stateBitSet = IntSet.empty
  val mutable pStateBitSet = IntSet.empty
  val mutable dPStateBitSet = IntSet.empty
  val mutable paramBitSet = IntSet.empty
  val mutable stateVars = LLDesigMap.empty
  val mutable paramVars = LLDesigSet.empty
  val mutable internalStateVars = LLDesigSet.empty
                                    
  (* caches *)
  val mutable cachedPrimedVarCube = None
  val mutable cachedDPrimedVarCube = None
  val mutable cachedUnprimedVarCube = None
  val mutable cachedParamVarCube = None
  val mutable cachedAllVarCube = None
  val mutable cachedP2USubstTable = None
  val mutable cachedU2PSubstTable = None
  val mutable cachedU2DPSubstTable = None
  val mutable cachedP2DPSubstTable = None
  val mutable cachedAllButParamCube = None
  val mutable cachedConstraintsOnAllVars = None
  val mutable cachedConstraintsOnParams = None
  (* We also cache some functions to avoid *)
  (* recomputing them each time            *)
  val mutable cachedStateVarPrinter = None
  val mutable cachedParamVarPrinter = None
  val mutable cachedCubePrinter = None
  val mutable cachedAllVarPrinter = None
  val mutable cachedVarCubes = LLDesigSetMap.empty

  initializer
    if !Opts.reorderEnabled then
      Man.enable_autodyn manager !Opts.reorderMethod
    else
      Man.disable_autodyn manager

  method reset () =
    manager <- Man.make_d ();
    if !Opts.reorderEnabled then
      Man.enable_autodyn manager !Opts.reorderMethod
    else
      Man.disable_autodyn manager;
    numTotalBits <- 0;
    numStateBits <- 0;
    numInternalStateBits <- 0;
    numParamBits <- 0;
    bitNameToBddMap <- StringMap.empty;
    indexToBitNameMap <- IntMap.empty;
    varMap <- LLDesigMap.empty;
    stateBitSet <- IntSet.empty;
    pStateBitSet <- IntSet.empty;
    dPStateBitSet <- IntSet.empty;
    paramBitSet <- IntSet.empty;
    stateVars <- LLDesigMap.empty;
    paramVars <- LLDesigSet.empty;
    internalStateVars <- LLDesigSet.empty;

    self#invalidateCaches ()
                          
  method private invalidateCaches () =
    cachedPrimedVarCube <- None;
    cachedDPrimedVarCube <- None;
    cachedUnprimedVarCube <- None;
    cachedParamVarCube <- None;
    cachedAllVarCube <- None;
    cachedP2USubstTable <- None;
    cachedU2PSubstTable <- None;
    cachedU2DPSubstTable <- None;
    cachedP2DPSubstTable <- None;
    cachedAllButParamCube <- None;
    cachedStateVarPrinter <- None;
    cachedParamVarPrinter <- None;
    cachedCubePrinter <- None;
    cachedAllVarPrinter <- None;
    cachedConstraintsOnAllVars <- None;
    cachedConstraintsOnParams <- None;
    cachedVarCubes <- LLDesigSetMap.empty

  method makeTrue () = 
    Bdd.dtrue manager

  method makeFalse () = 
    Bdd.dfalse manager

  method private lg num =
    let rec lgInternal num =
      if num <= 0 then
        raise (Invalid_argument ("bddEncoder#lg called with argument <= 0"))
      else
        match num with
        | 1 -> 1
        | _ -> 1 + (lgInternal (num lsr 1))
    in
    if (num = 1) then 1 else lgInternal (num - 1)

  method private registerBits name numBits =
    let low = numTotalBits in

    let rec registerBitsRec numBits = 
      let bitName = (lldesigToString name) ^ "." ^ (string_of_int (numBits - 1)) in
      let curBit = low + (numBits - 1) in
      match numBits with
      | 0 -> assert false
      | _ -> 
         let bdd = Bdd.ithvar manager curBit in
         bitNameToBddMap <- StringMap.add bitName bdd bitNameToBddMap;
         indexToBitNameMap <- IntMap.add curBit bitName indexToBitNameMap;
         if numBits = 1 then 
           [ bitName ]
         else
           (registerBitsRec (numBits - 1)) @ [ bitName ]
    in
    let rv = (low, registerBitsRec numBits) in
    Man.group manager low numBits Man.MTR_FIXED;
    Debug.dprintf "bdd" "Registered %d bits for group %a, total bits so far = %d\n" 
                  numBits AST.pLLDesignator name (numTotalBits + numBits);
    numTotalBits <- numTotalBits + numBits;
    rv

  method private checkVarReregister name valDomain =
    let dset = List.fold_left (fun acc v -> LLDesigSet.add v acc) LLDesigSet.empty valDomain in
    let sDom = LLDesigSet.elements dset in
    try
      let pDom, _, _, _, _, _, _, _ = LLDesigMap.find name varMap in
      if pDom <> sDom then
        raise (BddException ("Variable \"" ^ (lldesigToString name) ^ "\" already registered, " ^ 
                               " with a different domain"))
      else
        None
    with Not_found ->
      Some sDom

  method private makeBDDForRepr low size repr =
    let mkBddForBit bitNum = 
      let mask = if bitNum = 1 then 1 else (1 lsl (bitNum - 1)) in
      if ((repr land mask) <> 0) then
        Bdd.ithvar manager (low + (bitNum - 1))
      else
        Bdd.dnot (Bdd.ithvar manager (low + (bitNum - 1)))
    in
    let rec mkBDDforReprRec size =
      match size with
      | 0 -> assert false
      | _ -> 
         let bdd = mkBddForBit size in
         if size = 1 then 
           bdd
         else
           Bdd.dand bdd (mkBDDforReprRec (size - 1))
    in
    mkBDDforReprRec size

  method registerVar name valDomain =
    let sortedValDomain = self#checkVarReregister name valDomain in
    match sortedValDomain with
    | None -> ()
    | Some s ->
       begin
         let sortedValDomain = s in
         let domainSize = List.length sortedValDomain in
         let numBits = self#lg domainSize in
         let low, bitNameList = self#registerBits name numBits in           
         let _, valreppairs = 
           List.fold_left 
             (fun (cnt, acclst) v -> (cnt + 1, (v, cnt) :: acclst)) (0, []) sortedValDomain
         in
         let domValToBDDMap, cubeToDomValMap, varConstraints = 
           List.fold_left
             (fun (map1, map2, constr) (domval, repr) ->
              let bddForRepr = self#makeBDDForRepr low numBits repr in
              (LLDesigMap.add domval bddForRepr map1,
               IntMap.add repr domval map2,
               Bdd.dor constr bddForRepr))
             (LLDesigMap.empty, IntMap.empty, (self#makeFalse ()))
             valreppairs
         in
         
         let cubeToDomValFun cube =
           let curidx = ref 0 in
           let repr =
             Array.fold_left
               (fun repr valu ->
                if ((!curidx < low) || (!curidx > (low + (numBits - 1)))) then
                  begin
                    curidx := !curidx + 1;
                    repr
                  end
                else
                  begin
                    let actval =
                      match valu with
                      | Man.False -> 0
                      | Man.True -> 1
                      | Man.Top -> 0
                    in
                    let shift = !curidx - low in
                    curidx := !curidx + 1;
                    if shift = 0 then
                      repr lor actval
                    else
                      repr lor (actval lsl shift)
                  end) 0 cube
           in
           List.nth sortedValDomain repr
         in
         
         varMap <- LLDesigMap.add name 
                                  (sortedValDomain, low, numBits, bitNameList, 
                                   domValToBDDMap, cubeToDomValMap, 
                                   cubeToDomValFun, varConstraints)
                                  varMap;
         self#invalidateCaches ()
       end

  method lookupVar name =
    try
      Some (LLDesigMap.find name varMap)
    with Not_found ->
      None

  method prop2BDD prop =
    let rec prop2BDDInt prop = 
      match prop with
      | LLPropTrue -> self#makeTrue ()
      | LLPropFalse -> self#makeFalse ()
      | LLPropEquals (desig1, desig2) ->
         let l1 = self#lookupVar desig1 in
         let l2 = self#lookupVar desig2 in
         begin
           match l1, l2 with
           | Some (_, low1, size1, _, _, _, _, constraints1),
             Some (_, low2, size2, _, _, _, _, constraints2) ->
              if (size1 <> size2) then
                raise (BddException ("Sizes of variables not equal in equality"))
              else
                begin
                  let rec mkEqual low1 low2 size =
                    match size with
                    | 0 -> assert false
                    | _ -> 
                       let bdd = 
                         (Bdd.nxor 
                            (Bdd.ithvar manager (low1 + (size - 1)))
                            (Bdd.ithvar manager (low2 + (size - 1)))) 
                       in
                       if size = 1 then 
                         bdd
                       else
                         Bdd.dand bdd (mkEqual low1 low2 (size - 1))
                  in
                  Bdd.dand 
                    (Bdd.dand constraints1 constraints2) 
                    (mkEqual low1 low2 size1)
                end
                  
           | Some (_, _, _, _, dv2Bdd, _, _, _), None ->
              begin
                try 
                  LLDesigMap.find desig2 dv2Bdd
                with 
                | Not_found -> 
                   begin
                     let propString = AST.astToString AST.pLLProp prop in
                     raise (BddException ("Invalid value while making BDD: " ^
                                            (lldesigToString desig2) ^ 
                                              "\nIn subexpression:\n" ^ propString))
                   end
              end
           | None, Some (_, _, _, _, dv2Bdd, _, _, _) ->
              begin
                try 
                  LLDesigMap.find desig1 dv2Bdd
                with 
                | Not_found -> 
                   begin
                     let propString = AST.astToString AST.pLLProp prop in
                     raise (BddException ("Invalid value while making BDD: " ^
                                            (lldesigToString desig2) ^ 
                                              "\nIn subexpression:\n" ^ propString))
                   end
              end
           | None, None ->
              if desig1 = desig2 then
                self#makeTrue ()
              else
                self#makeFalse ()
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

  method private registerBitsForVar low size =
    let rec registerBitsRec size bitSet =
      if size = 1 then
        IntSet.add low bitSet
      else
        registerBitsRec (size - 1) (IntSet.add (low + (size - 1)) bitSet)
    in
    registerBitsRec size IntSet.empty

  method registerStateVariable varName varDomain =
    let varNameP = getPrimedLLDesig varName in
    self#registerVar varName varDomain;
    self#registerVar varNameP varDomain;
    if !Opts.iterativeSq then
      begin
        let varNameDP = getDPrimedLLDesig varName in
        self#registerVar varNameDP varDomain;
        let _, low, size, _, _, _, _, _ = LLDesigMap.find varNameDP varMap in
        dPStateBitSet <- IntSet.union dPStateBitSet (self#registerBitsForVar low size)
      end
    else
      ();
    (* We haven't thrown an exception -> We're good! *)
    stateVars <- LLDesigMap.add varName varNameP stateVars;
    (* register the bits as belonging to state vars and pstatevars *)
    let _, low, size, _, _, _, _, _ = LLDesigMap.find varName varMap in
    stateBitSet <- IntSet.union stateBitSet (self#registerBitsForVar low size);
    numStateBits <- numStateBits + size;
    let _, low, size, _, _, _, _, _ = LLDesigMap.find varNameP varMap in
    pStateBitSet <- IntSet.union pStateBitSet (self#registerBitsForVar low size)

  method registerInternalStateVariable varName varDomain =
    self#registerStateVariable varName varDomain;
    let _, _, size, _, _, _, _, _ = LLDesigMap.find varName varMap in
    numInternalStateBits <- numInternalStateBits + size;
    internalStateVars <- LLDesigSet.add varName internalStateVars

  method registerParamVariable varName varDomain = 
    self#registerVar varName varDomain;
    paramVars <- LLDesigSet.add varName paramVars;
    (* register the bits *)
    let _, low, size, _, _, _, _, _ = LLDesigMap.find varName varMap in
    numParamBits <- numParamBits + size;
    paramBitSet <- IntSet.union paramBitSet (self#registerBitsForVar low size)

  method private getCubeForOneVar varname =
    let (_, _, _, bitNameList, _, _, _, _) = LLDesigMap.find varname varMap in
    List.fold_left 
      (fun acc bitname ->
       let bdd = StringMap.find bitname bitNameToBddMap in
       Bdd.dand bdd acc) (self#makeTrue ()) bitNameList

  method getCubeForVars varNameList =
    let varNameSet = 
      List.fold_left 
        (fun s v -> LLDesigSet.add v s) 
        LLDesigSet.empty varNameList 
    in
    try 
      LLDesigSetMap.find varNameSet cachedVarCubes
    with
    | Not_found ->
       let r = 
         LLDesigSet.fold
           (fun v cube ->
            Bdd.dand cube (self#getCubeForOneVar v)) varNameSet (self#makeTrue ())
       in
       cachedVarCubes <- LLDesigSetMap.add varNameSet r cachedVarCubes;
       r
         
  method getCubeForVar varname =
    self#getCubeForVars [ varname ]

  method getCubeForUnprimedVars () =
    match cachedUnprimedVarCube with
    | Some cube -> cube
    | None ->
       let cube = 
         LLDesigMap.fold 
           (fun vname pname bdd ->
            Bdd.dand (self#getCubeForOneVar vname) bdd)
           stateVars (self#makeTrue ())
       in
       cachedUnprimedVarCube <- Some cube;
       cube

  method getCubeForPrimedVars () = 
    match cachedPrimedVarCube with 
    | Some cube -> cube
    | None ->
       let cube = 
         LLDesigMap.fold 
           (fun vname pname bdd ->
            Bdd.dand (self#getCubeForOneVar pname) bdd)
           stateVars (self#makeTrue ())
       in
       cachedPrimedVarCube <- Some cube;
       cube

  method getCubeForDPrimedVars () =
    match cachedDPrimedVarCube with
    | Some cube -> cube
    | None ->
       let cube = 
         if !Opts.iterativeSq then
           LLDesigMap.fold 
             (fun vname pname bdd ->
              let dpname = getDPrimedLLDesig vname in
              Bdd.dand (self#getCubeForOneVar dpname) bdd)
             stateVars (self#makeTrue ())
         else
           self#makeTrue ()
       in
       cachedDPrimedVarCube <- Some cube;
       cube

  method getCubeForParamVars () =
    match cachedParamVarCube with
    | Some c -> c
    | None ->
       let c = 
         LLDesigSet.fold 
           (fun paramname bdd ->
            Bdd.dand (self#getCubeForOneVar paramname) bdd)
           paramVars (self#makeTrue ())
       in
       cachedParamVarCube <- Some c;
       c

  method getCubeForAllVars () =
    match cachedAllVarCube with
    | Some c -> c
    | None ->
       let c = 
         Bdd.dand (self#getCubeForUnprimedVars ()) 
                  (self#getCubeForParamVars ())
       in
       cachedAllVarCube <- Some c;
       c
         
  method private substOneVarInTable table varName sVarName =
    let _, lowsrc, size1, _, _, _, _, _ = LLDesigMap.find varName varMap in
    let _, lowdst, size2, _, _, _, _, _ = LLDesigMap.find sVarName varMap in
    if (size1 <> size2) then
      raise (Invalid_argument ("Types of variables \"" ^ (lldesigToString varName) ^ 
                                 "\" and \"" ^ (lldesigToString sVarName) ^ "\" are " ^ 
                                   "not compatible for substitution"))
    else
      let rec subst fromidx toidx size =
        match size with
        | 0 -> ()
        | _ ->
           table.(fromidx + (size - 1)) <- Bdd.ithvar manager (toidx + (size - 1));
           subst fromidx toidx (size - 1)
      in
      subst lowsrc lowdst size1

  method getSubstTableP2U () = 
    match cachedP2USubstTable with
    | Some table -> table
    | None ->
       let table = Array.make numTotalBits (self#makeTrue ()) in
       let table = Array.mapi (fun i elem -> Bdd.ithvar manager i) table in
       LLDesigMap.iter 
         (fun vname pname ->
          self#substOneVarInTable table pname vname) stateVars;
       cachedP2USubstTable <- Some table;
       table

  method getSubstTableU2P () = 
    match cachedU2PSubstTable with
    | Some table -> table
    | None ->
       let table = Array.make numTotalBits (self#makeTrue ()) in
       let table = Array.mapi (fun i elem -> Bdd.ithvar manager i) table in
       LLDesigMap.iter 
         (fun vname pname ->
          self#substOneVarInTable table vname pname) stateVars;
       cachedU2PSubstTable <- Some table;
       table

  method getSubstTableU2DP () =
    match cachedU2DPSubstTable with
    | Some t -> t 
    | None ->
       let t = Array.make numTotalBits (self#makeTrue ()) in
       let t = Array.mapi (fun i elem -> Bdd.ithvar manager i) t in
       LLDesigMap.iter 
         (fun vname pname ->
          self#substOneVarInTable t vname (getDPrimedLLDesig vname)) 
         stateVars;
       cachedU2DPSubstTable <- Some t;
       t

  method getSubstTableP2DP () =
    match cachedP2DPSubstTable with
    | Some t -> t 
    | None ->
       let t = Array.make numTotalBits (self#makeTrue ()) in
       let t = Array.mapi (fun i elem -> Bdd.ithvar manager i) t in
       LLDesigMap.iter 
         (fun vname pname ->
          self#substOneVarInTable t pname (getDPrimedLLDesig vname)) 
         stateVars;
       cachedP2DPSubstTable <- Some t;
       t

  method getAllButParamCube () = 
    match cachedAllButParamCube with
    | Some cube -> cube
    | None ->
       let cube1 = self#getCubeForPrimedVars () in
       let cube2 = self#getCubeForUnprimedVars () in
       let cube3 = self#getCubeForDPrimedVars () in
       let cube = Bdd.dand cube1 (Bdd.dand cube2 cube3) in
       cachedAllButParamCube <- Some cube;
       cube

  method getBitPrinter () =
    (fun fmt i -> fprintf fmt "%s" (IntMap.find i indexToBitNameMap))

  method getCubePrinter () =
    match cachedCubePrinter with
    | Some f -> f
    | None ->
       let rval = 
         (fun fmt cube ->
          Array.iteri
            (fun idx valu ->
             let name = IntMap.find idx indexToBitNameMap in
             match valu with
             | Man.True -> fprintf fmt "%s |--> true@," name
             | Man.False -> fprintf fmt "%s |--> false@," name
             | _ -> fprintf fmt "%s |--> dcare@," name) cube)
       in
       cachedCubePrinter <- Some rval;
       rval

  method getNumTotalBits () =
    numTotalBits

  method getNumStateBits () =
    numStateBits

  method getNumInternalStateBits () = 
    numInternalStateBits

  method getNumParamBits () =
    numParamBits

  method getNumMinTerms bdd =
    Bdd.nbminterms numTotalBits (Bdd.dand bdd (self#makeTrue ()))

  method getNumMinTermsState bdd =
    let ebdd = Bdd.dand (self#getCubeForPrimedVars ())
                        (self#getCubeForDPrimedVars ()) in
    let ebdd = Bdd.dand (self#getCubeForPrimedVars ()) ebdd in
    Bdd.nbminterms numStateBits (Bdd.exist ebdd bdd)

  method getNumMinTermsStateNI bdd =
    Bdd.nbminterms (numStateBits - numInternalStateBits) 
                   (Bdd.exist 
                      (Bdd.dand (self#getCubeForPrimedVars ())
                                (Bdd.dand (self#getCubeForDPrimedVars ())
                                          (self#getCubeForParamVars ())))
                      bdd)

  method getNumMinTermsParam bdd =
    Bdd.nbminterms numParamBits (Bdd.exist
                                   (self#getAllButParamCube ())
                                   bdd)

  method printCubes n fmt bdd =
    let n = if n = 0 then max_int else n in
    let bdd = Bdd.dand bdd (self#makeTrue ()) in
    let printer = self#getCubePrinter () in
    let cubes = Bdd.pick_cubes_on_support 
                  bdd (self#getCubeForAllVars ()) n
    in
    Array.iteri
      (fun i cube ->
       let minTerm = Bdd.pick_minterm cube in
       fprintf fmt "Cube %d:@,%a@," i printer minTerm) cubes

  method getAllVarPrinter () =
    match cachedAllVarPrinter with
    | Some f -> f
    | None ->
       let r = 
         (fun fmt cube ->
          LLDesigMap.iter
            (fun name desc ->
             let _, _, _, _, _, _, cubeToDomValFun, _ = desc in
             fprintf fmt "%a |--> %a@," AST.pLLDesignator name
                     AST.pLLDesignator (cubeToDomValFun cube)) varMap) in
       cachedAllVarPrinter <- Some r;
       r
         
  method getStateVarPrinter () =
    match cachedStateVarPrinter with
    | Some f -> f
    | None ->
       let r = 
         (fun fmt cube ->
          LLDesigMap.iter 
            (fun name pname ->
             (* if LLDesigSet.mem name internalStateVars then *)
             (*   () *)
             (* else *)
               let _, _, _, _, _, _, cubeToDomValFun, _ = LLDesigMap.find name varMap in
               fprintf fmt "%a |--> %a@," AST.pLLDesignator name 
                       AST.pLLDesignator (cubeToDomValFun cube)) stateVars)
       in
       cachedStateVarPrinter <- Some r;
       r

  method getParamVarPrinter () = 
    match cachedParamVarPrinter with
    | Some f -> f 
    | None ->
       let r = 
         (fun fmt cube ->
          LLDesigSet.iter 
            (fun name ->
             let _, _, _, _, _, _, cubeToDomValFun, _ = LLDesigMap.find name varMap in
             fprintf fmt "%a |--> %a@," AST.pLLDesignator name 
                     AST.pLLDesignator (cubeToDomValFun cube)) paramVars)
       in
       cachedParamVarPrinter <- Some r;
       r

  method printStateVars n fmt bdd =
    let n = if n = 0 then max_int else n in
    let bdd = Bdd.dand bdd (self#makeTrue ()) in
    let printer = self#getStateVarPrinter () in
    let ebdd = Bdd.exist (Bdd.dand (self#getCubeForParamVars ())
                                   (Bdd.dand
                                      (self#getCubeForPrimedVars ())
                                      (self#getCubeForDPrimedVars ()))) bdd in
    
    let cubes = Bdd.pick_cubes_on_support ebdd (self#getCubeForUnprimedVars ()) n in
    
    Array.iteri
      (fun i cube ->
       let minTerm = self#pickMinTermOnStates cube in
       fprintf fmt "State Values %d:@,%a@,@," i printer minTerm) cubes

  method printParamVars n fmt bdd =
    if ((LLDesigSet.cardinal paramVars) = 0) then
      fprintf fmt "No parameters to synthesize!@,"
    else
      let n = if n = 0 then max_int else n in
      let ebdd = Bdd.exist (self#getAllButParamCube ()) bdd in
      let printer = self#getParamVarPrinter () in
      
      let cubes = Bdd.pick_cubes_on_support ebdd (self#getCubeForParamVars ()) n in
      
      Array.iteri
        (fun i cube ->
         let minTerm = self#pickMinTermOnParams cube in
         fprintf fmt "Param Values %d:@,%a@," i printer minTerm) cubes

  method cubeOfMinTerm minTerm =
    Bdd.cube_of_minterm manager minTerm

  method private determinizeOnSet set cube =
    Array.mapi
      (fun idx v ->
       if (not (IntSet.mem idx set)) then
         v
       else
         match v with
         | Man.True 
         | Man.False -> v
         | _ -> Man.False) cube

  method pickMinTermOnPStates bdd =
    let ecube = Bdd.dand (self#getCubeForParamVars ()) (self#getCubeForUnprimedVars ()) in
    let ecube = Bdd.dand ecube (self#getCubeForDPrimedVars ()) in
    let ebdd = Bdd.exist ecube bdd in
    let minTerm = Bdd.pick_minterm ebdd in
    self#determinizeOnSet pStateBitSet minTerm

  method pickMinTermOnStates bdd =
    let ebdd = Bdd.exist (Bdd.dand (self#getCubeForParamVars ())
                                   (self#getCubeForPrimedVars ())) bdd in
    let minTerm = Bdd.pick_minterm ebdd in
    self#determinizeOnSet stateBitSet minTerm

  method pickMinTermOnParams bdd =
    let ebdd = Bdd.exist (self#getAllButParamCube ()) bdd in
    let minTerm = Bdd.pick_minterm ebdd in
    self#determinizeOnSet paramBitSet minTerm

  method getStateVars bdd =
    let minTerm = self#pickMinTermOnStates bdd in
    LLDesigMap.fold
      (fun name pname map ->
       if LLDesigSet.mem name internalStateVars then
         map
       else
         let _, _, _, _, _, _, cubeToDomValFun, _ = LLDesigMap.find name varMap in
         LLDesigMap.add name (cubeToDomValFun minTerm) map)
      stateVars LLDesigMap.empty

  method getNStateVars n bdd =
    let n = if n = 0 then max_int else n in
    let ebdd = Bdd.exist (Bdd.dand (self#getCubeForPrimedVars ())
                                   (Bdd.dand
                                      (self#getCubeForDPrimedVars ())
                                      (self#getCubeForParamVars ()))) bdd in
    let cubes = Bdd.pick_cubes_on_support (self#getCubeForUnprimedVars ()) ebdd n in

    Array.fold_left 
      (fun lst cube ->
       (self#getStateVars cube) :: lst) [] cubes

  method getParamVars bdd = 
    let minTerm = self#pickMinTermOnStates bdd in
    LLDesigSet.fold
      (fun name map ->
       let _, _, _, _, _, _, cubeToDomValFun, _ = LLDesigMap.find name varMap in
       LLDesigMap.add name (cubeToDomValFun minTerm) map)
      paramVars LLDesigMap.empty

  method getNParamVars n bdd =
    let n = if n = 0 then max_int else n in
    let ebdd = Bdd.exist (self#getAllButParamCube ()) bdd in
    let cubes = Bdd.pick_cubes_on_support (self#getCubeForParamVars ()) ebdd n in

    Array.fold_left
      (fun lst cube ->
       (self#getStateVars cube) :: lst) [] cubes

  method getConstraintsOnAllVars () =
    match cachedConstraintsOnAllVars with
    | Some c -> c
    | None ->
       let c = 
         LLDesigMap.fold
           (fun name desc acc ->
            let _, _, _, _, _, _, _, constr = desc in
            Bdd.dand acc constr) varMap (self#makeTrue ())
       in
       cachedConstraintsOnAllVars <- Some c;
       c

  method getConstraintsOnParams () =
    match cachedConstraintsOnParams with
    | Some c -> c
    | None ->
       let c = 
         LLDesigMap.fold 
           (fun name desc acc ->
            if (LLDesigSet.mem name paramVars) then
              let _, _, _, _, _, _, _, constr = desc in
              Bdd.dand acc constr
            else
              acc) varMap (self#makeTrue ())
       in
       cachedConstraintsOnParams <- Some c;
       c

  method getPeakBDDSize () =
    Man.get_node_count_peak manager

  method isFalse bdd =
    Bdd.is_false (Bdd.dand bdd (self#makeTrue ()))

  method getNumParamVars () =
    LLDesigSet.cardinal paramVars

  method minimize () =
    if (!Opts.reorderEnabled) then
      Man.reduce_heap manager !Opts.reorderMethod 10
    else
      ();
    ignore (Man.garbage_collect manager)

  method reorder bound  =
    if (not !Opts.reorderEnabled) then
      Man.reduce_heap manager !Opts.reorderMethod bound
    else
      ()

  (* accessors for var names *)
  method getParamVarNames () =
    LLDesigSet.elements paramVars
                        
  method getStateVarNames () =
    LLDesigMap.fold (fun name namep lst -> name :: lst) stateVars []

  method getStateVarNamesNI () =
    LLDesigMap.fold 
      (fun name namep lst -> 
       if (not (LLDesigSet.mem name internalStateVars)) then 
         name :: lst
       else
         lst) stateVars []

  method enableAutoReorder () =
    Man.enable_autodyn manager !Opts.reorderMethod

  method disableAutoReorder () =
    Man.disable_autodyn manager

end (* class bddEncoder *)
