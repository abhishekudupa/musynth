open MusynthTypes
module CK = MusynthASTChecker
module AST = MusynthAST
module Utils = MusynthUtils
open Cudd
module Opts = MusynthOptions
open Format

class bddManager =
  object (self)
    val mutable manager = Man.make_d ()
    val mutable numTotalBits = 0
    val mutable bitNameToBddMap = StringMap.empty
    val mutable indexToBitNameMap = IntMap.empty
    val mutable varMap = LLDesigMap.empty
    val mutable stateVars = LLDesigMap.empty
    val mutable paramVars = LLDesigSet.empty
                              
    (* caches *)
    val mutable cachedPrimedVarCube = None
    val mutable cachedUnprimedVarCube = None
    val mutable cachedP2USubstTable = None
    val mutable cachedU2PSubstTable = None

    method reset () =
      manager <- Man.make_d ();
      numTotalBits <- 0;
      bitNameToBddMap <- StringMap.empty;
      indexToBitNameMap <- IntMap.empty;
      varMap <- LLDesigMap.empty;
      stateVars <- LLDesigMap.empty;
      paramVars <- LLDesigSet.empty;

      self#invalidateCaches ()
      
    method private invalidateCaches () =
      cachedPrimedVarCube <- None;
      cachedUnprimedVarCube <- None;
      cachedP2USubstTable <- None;
      cachedU2PSubstTable <- None

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
          | 2 -> 1
          | _ -> 1 + (lgInternal (num lsr 1))
      in
      lgInternal num

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
      fprintf std_formatter "Registered %d bits for group %a, total bits so far = %d\n" 
              numBits AST.pLLDesignator name (numTotalBits + numBits);
      numTotalBits <- numTotalBits + numBits;
      rv

    method private checkVarReregister name valDomain =
      let dset = List.fold_left (fun acc v -> LLDesigSet.add v acc) LLDesigSet.empty valDomain in
      let sDom = LLDesigSet.elements dset in
      try
        let pDom, _, _, _, _, _, _ = LLDesigMap.find name varMap in
        if pDom <> sDom then
          raise (BddException ("Variable \"" ^ (lldesigToString name) ^ "\" already registered, " ^ 
                                 " with a different domain"))
        else
          sDom
      with Not_found ->
        sDom

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
      let valDomain = self#checkVarReregister name valDomain in
      let domainSize = List.length valDomain in
      let numBits = self#lg domainSize in
      
      let low, bitNameList = self#registerBits name numBits in
      let _, valreppairs = 
        List.fold_left 
          (fun (cnt, acclst) v -> (cnt + 1, (v, cnt) :: acclst)) (0, []) valDomain
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
      varMap <- LLDesigMap.add name (valDomain, low, numBits, bitNameList, 
                                     domValToBDDMap, cubeToDomValMap, varConstraints)
                               varMap;
      self#invalidateCaches ();

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
             | Some (_, low1, size1, _, _, _, constraints1),
               Some (_, low2, size2, _, _, _, constraints2) ->
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
                
             | Some (_, _, _, _, dv2Bdd, _, _), None ->
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
             | None, Some (_, _, _, _, dv2Bdd, _, _) ->
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
      fprintf std_formatter "Lowering Prop:\n%a\n" AST.pLLProp cprop;
      pp_print_flush std_formatter ();
      let bdd = prop2BDDInt cprop in
      fprintf std_formatter "BDD:\n";
      Bdd.print (self#getVarPrinter ()) std_formatter bdd;
      fprintf std_formatter "\n";
      pp_print_flush std_formatter ();
      bdd

    method registerStateVariable varName varDomain =
      let varNameP = getPrimedLLDesig varName in
      self#registerVar varName varDomain;
      self#registerVar varNameP varDomain;
      (* We haven't thrown an exception -> We're good! *)
      stateVars <- LLDesigMap.add varName varNameP stateVars

    method registerParamVariable varName varDomain = 
      self#registerVar varName varDomain;
      paramVars <- LLDesigSet.add varName paramVars

    method private getCubeForOneVar varname =
      let (_, _, _, bitNameList, _, _, _) = LLDesigMap.find varname varMap in
      List.fold_left 
        (fun acc bitname ->
         let bdd = StringMap.find bitname bitNameToBddMap in
         Bdd.dand bdd acc) (self#makeTrue ()) bitNameList

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
           
    method private substOneVarInTable table varName sVarName =
      let _, lowsrc, size1, _, _, _, _ = LLDesigMap.find varName varMap in
      let _, lowdst, size2, _, _, _, _ = LLDesigMap.find sVarName varMap in
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

    method getVarPrinter () =
      (fun fmt i -> fprintf fmt "%s" (IntMap.find i indexToBitNameMap))

    method getCubePrinter () =
      (fun fmt cube ->
       Array.iteri
         (fun idx valu ->
          let name = IntMap.find idx indexToBitNameMap in
          match valu with
          | Man.True -> fprintf fmt "%s |--> true\n" name
          | Man.False -> fprintf fmt "%s |--> false\n" name
          | _ -> fprintf fmt "%s |--> dcare\n" name) cube)
                          

    method getNumTotalBits () =
      numTotalBits

    method getNumMinTerms bdd =
      Bdd.nbminterms numTotalBits (Bdd.dand bdd (self#makeTrue ()))

    method printCubes n fmt bdd =
      let bdd = Bdd.dand bdd (self#makeTrue ()) in
      let printer = self#getCubePrinter () in
      let printer = printer fmt in
      let count = ref 0 in
      Bdd.iter_cube 
        (fun cube ->
         if !count >= n then 
           ()
         else
           begin
             fprintf fmt "Cube %d:\n" !count;
             printer cube;
             count := !count + 1
           end) bdd

  end (* class bddEncoder *)
    

