open MusynthTypes
module AST = MusynthAST
(* utility functions *)

module DesigMap = Map.Make
    (struct 
      type t = musDesignatorT
      let compare d1 d2 = 
        let s1 = AST.astToString AST.pDesignator d1 in
        let s2 = AST.astToString AST.pDesignator d2 in
        Pervasives.compare s1 s2
    end)

let crossProduct lstlst =
  let rec crossProductRec lstlst =
    match lstlst with
    | [] -> assert false
    | [ head ] -> List.map (fun elem -> [ elem ]) head
    | head :: rest ->
        let rlists = crossProductRec rest in
        List.concat 
          (List.map (fun i -> List.map (fun r -> i :: r) rlists) head)
  in
  let empty = List.fold_left 
      (fun found lst -> 
        if found then true else ((List.length lst) = 0)) false lstlst in
  let empty = empty || (lstlst = []) in
  if empty then
    []
  else
    crossProductRec lstlst

let identConstPairList2Map idcplist =
  List.fold_left 
    (fun map (ident, const) ->
      IdentMap.add ident const map) IdentMap.empty idcplist

let checkParamCompleteness availQMap reqQMap =
  IdentMap.fold
    (fun ident reqtyp acc ->
      acc && IdentMap.exists (fun ident availtyp -> availtyp = reqtyp) availQMap) reqQMap true

let conjoinPropOpts po1 po2 =
  match po1, po2 with
  | Some prop1, Some prop2 -> Some (PropAnd (prop1, prop2, None))
  | Some prop1, None -> po1
  | None, Some prop2 -> po2
  | None, None -> None

let sDesigToIdent desig =
  match desig with
  | SimpleDesignator ident -> ident
  | _ -> assert false

let rec evalProp prop vMap =
  match prop with
  | PropTrue _ -> true
  | PropFalse _ -> false
  | PropEquals (desig1, desig2, _)
  | PropNEquals (desig1, desig2, _) ->
      let ident1 = sDesigToIdent desig1 in
      let ident2 = sDesigToIdent desig2 in 
      let v1, _ = IdentMap.find ident1 vMap in
      let v2, _ = IdentMap.find ident2 vMap in
      (match prop with
      | PropEquals _ -> v1 = v2
      | PropNEquals _ -> v1 <> v2
      | _ -> assert false)
  | PropAnd (prop1, prop2, _) -> (evalProp prop1 vMap) && (evalProp prop2 vMap)
  | PropOr (prop1, prop2, _) -> (evalProp prop1 vMap) || (evalProp prop2 vMap)
  | PropImplies (prop1, prop2, _) -> (not (evalProp prop1 vMap)) || (evalProp prop2 vMap)
  | PropIff (prop1, prop2, _) -> (evalProp prop1 vMap) = (evalProp prop2 vMap)
  | _ -> assert false

let getMapsForProp paramlist qMap propOpt =
  let vallists = 
    List.map 
      (fun paramname ->
        try
          let typ = IdentMap.find paramname qMap in
          match typ with
          | SymTypeAnon (idlist, _) -> List.map (fun const -> (paramname, const)) idlist
          | _ -> assert false
        with Not_found ->
          [ (paramname, paramname) ])
      paramlist
  in
  match propOpt with
  | None -> List.map identConstPairList2Map (crossProduct vallists)
  | Some prop -> 
      let cp = crossProduct vallists in
      let validcp = List.filter (fun cpelem -> evalProp prop (identConstPairList2Map cpelem)) cp in
      List.map identConstPairList2Map validcp
          
let rec splatList elem count =
  match count with
  | 0 -> []
  | n -> elem :: splatList elem (n - 1)

let makeEmptyMS () =
  DesigMap.empty

let msToList ms =
  DesigMap.fold 
    (fun elem cnt acc ->
      acc @ (splatList elem cnt)) ms []

let msToStr strFun ms =
  if ms = DesigMap.empty || (List.length (msToList ms) = 0) then
    "Empty"
  else
    DesigMap.fold 
      (fun elem cnt str ->
        if (cnt > 0) then
          str ^ (strFun elem) ^ "_" ^ (string_of_int cnt)
        else
          str) ms ""

let addToMs elem ms =
  let count = 
    (try
      DesigMap.find elem ms
    with Not_found -> 0) in
  DesigMap.add elem (count + 1) ms

let delFromMS elem ms =
  let count = DesigMap.find elem ms in
  DesigMap.add elem (count - 1) ms

let makeEmptyMSWithAlphabet alphabet =
  let emptyMS = makeEmptyMS () in
  List.fold_left 
    (fun acc alph ->
      DesigMap.add alph 0 acc) emptyMS alphabet

module MSSet = Set.Make
    (struct
      type t = int DesigMap.t
      let compare = DesigMap.compare Pervasives.compare
    end)

let rec enumerateMS alphabet size =
  let rec enumerateMSInternal alphabet size =
    match size with
    | 0 -> 
        MSSet.add (makeEmptyMSWithAlphabet alphabet) MSSet.empty
    | n ->
        let smallersets = enumerateMSInternal alphabet (size - 1) in
        let largersets = ref (MSSet.empty) in
        MSSet.iter 
          (fun sset ->
            List.iter
              (fun alph ->
                largersets := MSSet.add (addToMs alph sset) !largersets) alphabet) smallersets;
        MSSet.union smallersets !largersets
  in 
  let msset = enumerateMSInternal alphabet size in
  MSSet.elements msset

let rec enumerateLists alphabet size =
  match size with
  | 0 -> [[]]
  | _ ->
      let smalllists = enumerateLists alphabet (size - 1) in
      let newlists = 
        List.map 
          (fun sl ->
            List.concat
              (List.map
                 (fun alph ->
                   alph :: sl) alphabet)) smalllists in
      smalllists @ newlists

let listToStr strFun lst =
  if lst = [] then
    "Empty"
  else
    List.fold_left
      (fun str elem ->
        str ^ "_" ^ (strFun elem)) "" lst

let rec get2Combinations lst =
  match lst with
  | [] -> []
  | [ head ] -> []
  | head :: rest ->
      (List.fold_left (fun acc elem -> (head, elem) :: acc) [] rest) @ (get2Combinations rest)
  
let makePropList ptlist =
  let rec makePropListInt ptlist =
    match ptlist with
    | [] -> []
    | [ head ] -> []
    | head :: rest ->
        let pname, ptype = head in
        let typlist = List.filter (fun (name, typ) -> typ = ptype) ptlist in
        let restlist = List.filter (fun (name, typ) -> typ <> ptype) ptlist in
        let comb = get2Combinations typlist in
        comb @ (makePropListInt restlist)
  in
  makePropListInt ptlist

let nextuid = ref 0

let getuid () =
  let retval = !nextuid in
  nextuid := !nextuid + 1;
  retval

let resetuid () =
  nextuid := 0
