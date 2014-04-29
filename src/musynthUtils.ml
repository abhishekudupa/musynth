open MusynthTypes
module AST = MusynthAST
open Format
(* utility functions *)

module MSSet = 
  Set.Make
    (struct
      type t = int LLDesigMap.t
      let compare = (LLDesigMap.compare Pervasives.compare)
    end)

let rec splatList elem count =
  match count with
  | 0 -> []
  | n -> elem :: splatList elem (n - 1)

(* util functions for multisets and lists *)
(* used to construct channel automata *)
let makeEmptyMS () =
  LLDesigMap.empty

let msToList ms =
  LLDesigMap.fold 
    (fun elem cnt acc ->
     acc @ (splatList elem cnt)) ms []

let msToStr strFun ms =
  if ms = LLDesigMap.empty || (List.length (msToList ms) = 0) then
    "Empty"
  else
    LLDesigMap.fold 
      (fun elem cnt str ->
       if (cnt > 0) then
         str ^ (strFun elem) ^ "_" ^ (string_of_int cnt)
       else
         str) ms ""

let addToMs elem ms =
  let count = 
    (try
        LLDesigMap.find elem ms
      with Not_found -> 0) in
  LLDesigMap.add elem (count + 1) ms

let delFromMS elem ms =
  let count = LLDesigMap.find elem ms in
  LLDesigMap.add elem (count - 1) ms

let makeEmptyMSWithAlphabet alphabet =
  let emptyMS = makeEmptyMS () in
  List.fold_left 
    (fun acc alph ->
     LLDesigMap.add alph 0 acc) emptyMS alphabet


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
       List.concat
         (List.map
            (fun alph ->
             List.map 
               (fun sl ->
                alph :: sl) smalllists) alphabet) in
     smalllists @ newlists

let listToStr strFun lst =
  if lst = [] then
    "Empty"
  else if (List.length lst) = 1 then
    strFun (List.hd lst)
  else
    let str = strFun (List.hd lst) in
    let lst = List.tl lst in
    List.fold_left
      (fun str elem ->
       str ^ "_" ^ (strFun elem)) str lst


(* utils for UID generation *)    
let nextuid = ref 0

let getuid () =
  let retval = !nextuid in
  nextuid := !nextuid + 1;
  retval

let resetuid () =
  nextuid := 0

(* cross products and prop evaluation *)
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

let sDesigToIdent desig =
  match desig with
  | SimpleDesignator ident -> ident
  | _ -> assert false

let conjoinPropOpts popt1 popt2 =
  match popt1, popt2 with
  | None, None -> None
  | Some _, None -> popt1
  | None, Some _ -> popt2
  | Some p1, Some p2 -> Some (PropAnd (p1, p2, None))

let mergeIdentMaps m1 m2 =
  IdentMap.fold
    (fun ident typ acc ->
     try
       let _ = IdentMap.find ident acc in
       raise (Invalid_argument ("INTERNAL: Duplicate found in qMap"))
     with Not_found ->
       IdentMap.add ident typ acc) m2 m1

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


(* utils for dealing with LL automata *)              
let getMsgsForAut aut =
  match aut with
  | LLCompleteAutomaton (_, _, inmsgs, outmsgs, _, _, _, _, _)
  | LLIncompleteAutomaton (_, _, inmsgs, outmsgs, _, _) -> (inmsgs, outmsgs)

let getNameForAut aut =
  match aut with
  | LLCompleteAutomaton (name, _, _, _, _, _, _, _, _)
  | LLIncompleteAutomaton (name, _, _, _, _, _) -> name

let getTransitionsForAut aut =
  match aut with
  | LLCompleteAutomaton (_, _, _, _, transitions, _, _, _, _)
  | LLIncompleteAutomaton (_, _, _, _, transitions, _) -> transitions

let getStatesForAut aut =
  match aut with
  | LLCompleteAutomaton (_, states, _, _, _, _, _, _, _)
  | LLIncompleteAutomaton (_, states, _, _, _, _) -> states

let getAutomatonByName autlist autname =
  List.find (fun aut -> (getNameForAut aut) = autname) autlist

let getFairnessForAutomaton aut =
  match aut with
  | LLCompleteAutomaton (_, _, _, _, _, ftype, _, _, _)
  | LLIncompleteAutomaton (_, _, _, _, _, ftype) -> ftype

let getLFairnessForAutomaton aut =
  match aut with
  | LLCompleteAutomaton (_, _, _, _, _, _, lftype, _, _) -> lftype
  | _ -> LLLossFairnessNone

let getDFairnessForAutomaton aut =
  match aut with
  | LLCompleteAutomaton (_, _, _, _, _, _, _, dftype, _) -> dftype
  | _ -> LLDupFairnessNone

let getSender msg autlist = 
  let lst = 
    List.filter 
      (fun aut ->
       let _, outmsgs = getMsgsForAut aut in
       List.mem msg outmsgs) autlist
  in
  List.hd lst

let getReceivers msg autlist = 
  List.filter 
    (fun aut ->
     let inmsgs, _ = getMsgsForAut aut in
     List.mem msg inmsgs) autlist

let getStateNameForAutomaton aut = 
  let name = getNameForAut aut in
  LLFieldDesignator (name, "state")

let getStateNamePForAutomaton aut = 
  let name = getNameForAut aut in
  LLFieldDesignator (name, "state'")

let getCSPredsForMsg msg aut =
  let transitions = getTransitionsForAut aut in
  let name = getNameForAut aut in
  let statename = LLFieldDesignator (name, "state") in
  List.fold_left
    (fun acc trans ->
     match trans with
     | TComplete (start, m, final) ->
        if m = msg then
          LLPropOr (LLPropEquals (statename, start), acc)
        else
          acc
     | TParametrizedDest (start, m, (paramname, dset)) ->
        if (m = msg) then
          LLPropOr (LLPropAnd (LLPropEquals (statename, start),
                               (LLPropNot (LLPropEquals (paramname, LLSimpleDesignator "defer")))),
                    acc)
        else
          acc
     | _ -> assert false) LLPropFalse transitions

let getCSPredsForMsgAll msg allaut =
  let inaut = (getSender msg allaut) :: (getReceivers msg allaut) in
  List.fold_left
    (fun prop aut ->
     LLPropAnd (getCSPredsForMsg msg aut, prop)) LLPropTrue inaut

let getMsgsToSyncOnFromState aut state =
  let transitions = getTransitionsForAut aut in
  let msgs = 
    List.fold_left 
      (fun accset trans ->
       match trans with
       | TComplete (s, m, _)
       | TParametrizedDest (s, m, _) -> if s = state then LLDesigSet.add m accset else accset
       | _ -> assert false) LLDesigSet.empty transitions
  in
  LLDesigSet.elements msgs

let getStatesFromWhichMsgSync aut msg =
  let transitions = getTransitionsForAut aut in
  let states = 
    List.fold_left 
      (fun accset trans ->
       match trans with
       | TComplete (s, m, _)
       | TParametrizedDest (s, m, _) -> if m = msg then LLDesigSet.add s accset else accset
       | _ -> assert false) LLDesigSet.empty transitions
  in
  LLDesigSet.elements states

(* canonicalize a prop *)
let rec canonicalizeProp prop =
  let sortProps prop1 prop2 =
    if prop1 > prop2 then
      prop2, prop1
    else
      prop1, prop2
  in
  match prop with
  | LLPropTrue -> LLPropTrue
  | LLPropFalse -> LLPropFalse
  | LLPropEquals (d1, d2) ->
     if d1 < d2 then
       prop
     else
       LLPropEquals (d2, d1)
  | LLPropNot (LLPropTrue) -> LLPropFalse
  | LLPropNot (LLPropFalse) -> LLPropTrue
  | LLPropNot (LLPropNot prop1) -> canonicalizeProp prop1
  | LLPropNot prop1 -> LLPropNot (canonicalizeProp prop1)
  | LLPropAnd (prop1, prop2) ->
     let p1, p2 = sortProps prop1 prop2 in
     begin
       match p1, p2 with
       | LLPropFalse, _ -> LLPropFalse
       | _, LLPropFalse -> LLPropFalse
       | LLPropTrue, nprop
       | nprop, LLPropTrue -> canonicalizeProp nprop
       | nprop1, nprop2 -> LLPropAnd (canonicalizeProp nprop1, canonicalizeProp nprop2)
     end
  | LLPropOr (prop1, prop2) ->
     let p1, p2 = sortProps prop1 prop2 in
     begin
       match p1, p2 with
       | LLPropTrue, _ -> LLPropTrue
       | _, LLPropTrue -> LLPropTrue
       | LLPropFalse, nprop
       | nprop, LLPropFalse -> canonicalizeProp nprop
       | nprop1, nprop2 -> LLPropOr (canonicalizeProp nprop1, canonicalizeProp nprop2)
     end
  | LLPropTLX prop1 -> LLPropTLX (canonicalizeProp prop1)
  | LLPropTLU (prop1, prop2) -> LLPropTLU (canonicalizeProp prop1, canonicalizeProp prop2)

let rec canonicalizePropFP prop =
  let cprop = canonicalizeProp prop in
  if cprop = prop then
    cprop
  else
    canonicalizePropFP cprop

let makeFormatterOfName name =
  let oc = open_out name in
  (oc, formatter_of_out_channel oc)

(* utilities for making conjunctions and disjunctions *)
let makeConjunction propList =
  match propList with
  | [] -> LLPropTrue
  | [ head ] -> head
  | head :: rest ->
     let seed = head in
     List.fold_left 
       (fun acc prop ->
        LLPropAnd (acc, prop)) seed rest

let makeDisjunction propList =
  match propList with
  | [] -> LLPropFalse
  | [ head ] -> head 
  | head :: rest ->
     let seed = head in
     List.fold_left 
       (fun acc prop ->
        LLPropOr (acc, prop)) seed rest
     
let makeTrueDesig () =
  LLSimpleDesignator "true"

let makeFalseDesig () = 
  LLSimpleDesignator "false"

let makeLCMesgDesig () =
  LLSimpleDesignator "musynth_last_chosen_mesg"

let makeLCProcDesig () =
  LLSimpleDesignator "musynth_last_chosen_proc"

let makeLCMesgDesigPrime () = 
  LLSimpleDesignator "musynth_last_chosen_mesg'"

let makeLCProcDesigPrime () =
  LLSimpleDesignator "musynth_last_chosen_proc'"

let makeDeadlockDesig () =
  LLSimpleDesignator "deadlock"
