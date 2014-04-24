open MusynthTypes
open Format

module ST = MusynthSymTab
module CK = MusynthASTChecker
module Utils = MusynthUtils
module AST = MusynthAST
module Chan = MusynthChannel

let lowerQMap symtab qMap =
  IdentMap.fold 
    (fun ident typ acc ->
     let rtyp = CK.resolveSymType symtab typ in
     IdentMap.add ident rtyp acc) qMap IdentMap.empty

(* does two things: *)
(* first: recursively instantiates quantifiers *)
(* second: reduces to the low level IR *)
let instantiateDecl symtab qMap propOpt declParamInstantiator decl =
  let qMap = lowerQMap symtab qMap in
  match decl with
  | DeclSimple (declParam, _) -> declParamInstantiator symtab qMap propOpt declParam
  | DeclQuantified (declParam, newQMap, newPropOpt, _) ->
     let lnewQMap = lowerQMap symtab newQMap in
     declParamInstantiator symtab (Utils.mergeIdentMaps qMap lnewQMap)
                           (Utils.conjoinPropOpts propOpt newPropOpt) declParam

let substInDecl substMap declParamSubstitutor decl =
  match decl with
  | DeclSimple (declParam, _) -> DeclSimple (declParamSubstitutor substMap declParam, None)
  | DeclQuantified (declParam, qMap, propOpt, _) -> 
     DeclQuantified (declParamSubstitutor substMap declParam, qMap, propOpt, None)

(* helper routine to instantiate a desig with the given params and eval maps *)
let makeLLInstantiation evalmaps paramlist name =
  List.map
    (fun evalmap ->
     List.fold_left
       (fun acc param ->
        let newname, _ = IdentMap.find param evalmap in
        LLIndexDesignator (acc, newname)) 
       (LLSimpleDesignator name)
       paramlist)
    evalmaps

(* simply lowers a given designator *)
let rec lowerDesignator desig =
  match desig with
  | SimpleDesignator (name, _) -> LLSimpleDesignator name
  | IndexDesignator (ndesig, (name, _), _) ->
     LLIndexDesignator (lowerDesignator ndesig, name)
  | FieldDesignator (ndesig, (name, _), _) ->
     LLFieldDesignator (lowerDesignator ndesig, name)

let desigDeclInstantiator symtab qMap propOpt desig =
  let ident, paramlist = CK.destructDesigDecl desig in
  let name, _ = ident in
  if paramlist = [] then
    [ (LLSimpleDesignator name) ]
  else
    let evalMaps = Utils.getMapsForProp paramlist qMap propOpt in
    makeLLInstantiation evalMaps paramlist name

let transDeclInstantiator symtab qMap propOpt trans =
  let startdesig, msgdesig, finaldesig = trans in
  let sident, sparamlist = CK.destructDesigDecl startdesig in
  let mident, mparamlist = CK.destructDesigDecl msgdesig in
  let fident, fparamlist = CK.destructDesigDecl finaldesig in
  let sname, _ = sident in
  let mname, _ = mident in
  let fname, _ = fident in
  let comblists = sparamlist @ mparamlist @ fparamlist in
  if comblists = [] then
    [ TComplete (LLSimpleDesignator sname, 
                 LLSimpleDesignator mname, 
                 LLSimpleDesignator fname) ]
  else
    let combparamset = 
      List.fold_left 
        (fun acc param -> IdentSet.add param acc) IdentSet.empty comblists in
    let comblists = IdentSet.elements combparamset in
    let evalMaps = Utils.getMapsForProp comblists qMap propOpt in
    let sinst = makeLLInstantiation evalMaps sparamlist sname in
    let minst = makeLLInstantiation evalMaps mparamlist mname in
    let finst = makeLLInstantiation evalMaps fparamlist fname in
    let tlist = List.map2 (fun a b -> (a, b)) sinst minst in
    List.map2 (fun (a, b) c -> TComplete (a, b, c)) tlist finst


let rec desigSubstitutor substMap desig =
  match desig with
  | SimpleDesignator ident -> 
     let newident = (try IdentMap.find ident substMap with Not_found -> ident) in
     SimpleDesignator newident
  | IndexDesignator (ndesig, ident, _) ->
     let newident = (try IdentMap.find ident substMap with Not_found -> ident) in
     IndexDesignator (desigSubstitutor substMap ndesig, newident, None)
  | FieldDesignator (ndesig, ident, _) ->
     let newident = (try IdentMap.find ident substMap with Not_found -> ident) in
     FieldDesignator (desigSubstitutor substMap ndesig, newident, None)

let rec lldesigSubstitutor substMap desig =
  match desig with
  | LLSimpleDesignator name -> 
     let newname = try StringMap.find name substMap with Not_found -> name in
     LLSimpleDesignator newname
  | LLIndexDesignator (ndesig, name) ->
     let newname = try StringMap.find name substMap with Not_found -> name in
     LLIndexDesignator (lldesigSubstitutor substMap ndesig, newname)
  | LLFieldDesignator (ndesig, name) ->
     let newname = try StringMap.find name substMap with Not_found -> name in
     LLFieldDesignator (lldesigSubstitutor substMap ndesig, newname)

let rec lltransSubstitutor substMap trans =
  match trans with
  | TComplete (start, msg, final) -> TComplete (lldesigSubstitutor substMap start,
                                                lldesigSubstitutor substMap msg,
                                                lldesigSubstitutor substMap final)
  | TParametrizedDest (start, msg, (varname, valset)) ->
     TParametrizedDest (lldesigSubstitutor substMap start,
                        lldesigSubstitutor substMap msg,
                        (lldesigSubstitutor substMap varname,
                         LLDesigSet.fold
                           (fun desig acc -> 
                            LLDesigSet.add (lldesigSubstitutor substMap desig) acc)
                           valset LLDesigSet.empty))

  | TParametrizedMsgDest _ -> raise UnimplementedException
     
let transSubstitutor substMap trans =
  let start, msg, final = trans in
  (desigSubstitutor substMap start,
   desigSubstitutor substMap msg,
   desigSubstitutor substMap final)

let instantiateDesigBlock symtab qMap propOpt block =
  List.concat (List.map (instantiateDecl symtab qMap propOpt desigDeclInstantiator) block)

let instantiateTransBlock symtab qMap propOpt block =
  List.concat (List.map (instantiateDecl symtab qMap propOpt transDeclInstantiator) block)

let substituteInDesigBlock substMap block = 
  List.map (substInDecl substMap desigSubstitutor) block

let substituteInTransBlock substMap block =
  List.map (substInDecl substMap transSubstitutor) block

let stateAnnotationInstantiator symtab qMap propOpt allmsgs annot =
  match annot with
  | AnnotIncompleteEventList (msglist, _) ->
     LLAnnotEventList (instantiateDesigBlock symtab qMap propOpt msglist)
  | AnnotIncompleteNumEventList (num, msglist, _) ->
     LLAnnotNumEventList (num, instantiateDesigBlock symtab qMap propOpt msglist)
  | AnnotIncompleteNum (num, _) ->
     LLAnnotNumEventList (num, allmsgs)
  | AnnotIncomplete _ ->
     LLAnnotEventList allmsgs
  | AnnotComplete _
  | AnnotNone _ -> LLAnnotNone

let lowerFairness f =
  match f with
  | FairnessTypeJustice _ -> LLFairnessJustice
  | FairnessTypeCompassion _ -> LLFairnessCompassion
  | FairnessTypeNone -> LLFairnessNone

let lowerLossFairness f =
  match f with
  | LossFairnessNone -> LLLossFairnessNone
  | LossFairnessFinite _ -> LLLossFairnessFinite

let lowerDupFairness f =
  match f with
  | DupFairnessNone -> LLDupFairnessNone
  | DupFairnessFinite _ -> LLDupFairnessFinite

let rec stateAnnotSubstitutor substMap annot =
  match annot with
  | AnnotNone _
  | AnnotComplete _ 
  | AnnotIncomplete _
  | AnnotIncompleteNum _ -> annot
  | AnnotIncompleteEventList (msgdecllist, _) ->
     AnnotIncompleteEventList 
       (List.map (fun mdecl -> substInDecl substMap desigSubstitutor mdecl) msgdecllist, None)
  | AnnotIncompleteNumEventList (num, msgdecllist, _) ->
     AnnotIncompleteNumEventList
       (num, List.map (fun mdecl -> substInDecl substMap desigSubstitutor mdecl) msgdecllist, None)

let instantiateCompleteAutomaton symtab qMap propOpt desig states inmsgs outmsgs transitions ftype = 
  let ident, paramlist = CK.destructDesigDecl desig in 
  let name, _ = ident in
  let sdecls = List.map (fun (decl, annot) -> decl) states in
  let desigInstantiator = instantiateDesigBlock symtab IdentMap.empty None in
  let transInstantiator = instantiateTransBlock symtab IdentMap.empty None in
  let lftype = lowerFairness ftype in
  if paramlist = [] then
    [ LLCompleteAutomaton (LLSimpleDesignator name,
                           desigInstantiator sdecls,
                           desigInstantiator inmsgs,
                           desigInstantiator outmsgs,
                           transInstantiator transitions,
                           lftype, LLLossFairnessNone, 
                           LLDupFairnessNone, false) ]
  else
    let qMap = lowerQMap symtab qMap in
    let evalMaps = Utils.getMapsForProp paramlist qMap propOpt in
    List.map
      (fun evalMap ->
       let lldesig = (List.hd (makeLLInstantiation [ evalMap ] paramlist name)) in
       LLCompleteAutomaton (lldesig,
                            desigInstantiator (substituteInDesigBlock evalMap sdecls),
                            desigInstantiator (substituteInDesigBlock evalMap inmsgs),
                            desigInstantiator (substituteInDesigBlock evalMap outmsgs),
                            transInstantiator (substituteInTransBlock evalMap transitions),
                            lftype, LLLossFairnessNone, LLDupFairnessNone, false)) evalMaps

let rec convertLLDesigToPrimed desig = 
  match desig with
  | LLSimpleDesignator name -> LLSimpleDesignator (name ^ "'")
  | LLIndexDesignator (ndesig, name) -> LLIndexDesignator (convertLLDesigToPrimed ndesig, name)
  | LLFieldDesignator (ndesig, name) -> LLFieldDesignator (convertLLDesigToPrimed ndesig, name)

let instantiateChannelAutomaton symtab qMap propOpt desig chanprops msgs ftype lftype dftype =
  let ident, paramlist = CK.destructDesigDecl desig in 
  let name, _ = ident in
  let desigInstantiator = instantiateDesigBlock symtab IdentMap.empty None in
  let loweredftype = 
    match ftype with
    | FairnessTypeNone _ -> LLFairnessNone
    | _ -> LLFairnessJustice
  in
  let llftype = 
    match chanprops with
    | _, ChanLossy _, _, _, _ -> lowerLossFairness lftype
    | _ -> LLLossFairnessNone
  in
  let ldftype = 
    match chanprops with
    | _, _, ChanDuplicating _, _, _ -> lowerDupFairness dftype
    | _ -> LLDupFairnessNone
  in

  if paramlist = [] then
    let lldesig = LLSimpleDesignator name in
    let linmsgs = desigInstantiator msgs in
    let loutmsgs = List.map convertLLDesigToPrimed linmsgs in
    let states, transitions = Chan.buildChannelAutomaton linmsgs loutmsgs chanprops in
    [ LLCompleteAutomaton (lldesig, states, linmsgs, loutmsgs, transitions, 
                           loweredftype, llftype, ldftype, true) ]
  else
    let qMap = lowerQMap symtab qMap in
    let evalMaps = Utils.getMapsForProp paramlist qMap propOpt in
    List.map
      (fun evalMap ->
       let lldesig = (List.hd (makeLLInstantiation [ evalMap ] paramlist name)) in
       let linmsgs = desigInstantiator (substituteInDesigBlock evalMap msgs) in
       let loutmsgs = List.map convertLLDesigToPrimed linmsgs in
       let states, transitions = Chan.buildChannelAutomaton linmsgs loutmsgs chanprops in
       LLCompleteAutomaton (lldesig, states, linmsgs, loutmsgs, transitions, 
                            loweredftype, llftype, ldftype, true)) evalMaps

let rec checkParamCompatibility lstate paramlist =
  (* check that all the params mentioned in the state are available *)
  match lstate with
  | LLSimpleDesignator _ -> true
  | LLIndexDesignator (ndesig, ident) ->
     if (not (List.mem ident paramlist)) then 
       false
     else
       checkParamCompatibility ndesig paramlist
  | LLFieldDesignator (ndesig, ident) -> assert false

let rec getParamsFromLLIdent param = 
  match param with
  | LLSimpleDesignator ident -> (ident, [])
  | LLIndexDesignator (ndesig, ident) ->
     let nident, lst = getParamsFromLLIdent ndesig in
     (nident, ident :: lst)
  | LLFieldDesignator (ndesig, ident) -> assert false

let checkTransitionDefined ltranslist initstate msg =
  List.exists (fun (start, imsg, next) -> (start = initstate) && (imsg = msg)) ltranslist

let mkLLDesigSet strlst =
  List.fold_left (fun acc elem -> LLDesigSet.add elem acc) LLDesigSet.empty strlst

let identMapToStringMap map =
  IdentMap.fold 
    (fun (name1, _) (name2, _) acc -> StringMap.add name1 name2 acc)
    map StringMap.empty

let instantiateIncompleteAutomaton symtab qMap propOpt desig states inmsgs outmsgs 
                                   transitions ftype =
  let desigInstantiator = instantiateDesigBlock symtab IdentMap.empty None in
  let transInstantiator = instantiateTransBlock symtab IdentMap.empty None in

  let linmsgs = desigInstantiator inmsgs in
  let loutmsgs = desigInstantiator outmsgs in
  let lftype = lowerFairness ftype in

  let lstate2AnnotMap =
    List.fold_left 
      (fun acc1 sdecl ->
       let statedecl, annot = sdecl in
       let lstates = instantiateDecl symtab IdentMap.empty None desigDeclInstantiator statedecl in
       let lannot = stateAnnotationInstantiator symtab IdentMap.empty None linmsgs annot in
       (List.fold_left 
          (fun acc2 lstate ->
           LLDesigMap.add lstate lannot acc2) acc1 lstates))
      LLDesigMap.empty states
  in
  let lstates = LLDesigMap.fold (fun lstate annot acc -> lstate :: acc) lstate2AnnotMap [] in
  let ltrans = transInstantiator transitions in
  let ptrans = 
    LLDesigMap.fold 
      (fun lstate annot acc ->
       match annot with
       | LLAnnotNone -> acc
       | LLAnnotEventList lleventlist ->
          let newtrans = 
            (List.fold_left 
               (fun acc2 levent ->
                let _, availparams1 = getParamsFromLLIdent lstate in
                let _, availparams2 = getParamsFromLLIdent levent in
                let availparams = availparams1 @ availparams2 in
                let goodcands = 
                  List.filter 
                    (fun cand -> checkParamCompatibility cand availparams)
                    lstates
                in
                let goodcands = (LLSimpleDesignator "defer") :: goodcands in
                let newvarname = "synth_t_" ^ (string_of_int (Utils.getuid ())) in
                (TParametrizedDest (lstate, levent, (LLSimpleDesignator newvarname, 
                                                     (mkLLDesigSet goodcands)))) ::acc2)
               [] lleventlist)
          in
          acc @ newtrans
       | LLAnnotNumEventList (num, lleventlist) -> raise UnimplementedException) 
      lstate2AnnotMap []
  in
  (* filter out any duplicates we might have introduced *)
  let ptrans = 
    List.filter 
      (fun trans ->
       let start, msg =
         (match trans with
          | TParametrizedDest (start, msg, _) -> start, msg 
          | _ -> assert false) 
       in
       not (List.exists 
              (fun otrans -> 
               let ostart, omsg = 
                 (match otrans with
                  | TComplete (start, msg, _) -> start, msg
                  | _ -> assert false) in
               (ostart = start) && (omsg = msg)) ltrans)) ptrans 
  in
  (* now instantiate the copies of this automaton *)
  let ident, paramlist = CK.destructDesigDecl desig in
  let name, _ = ident in
  let qMap = lowerQMap symtab qMap in
  let evalMaps = Utils.getMapsForProp paramlist qMap propOpt in
  if paramlist = [] then
    [ (LLIncompleteAutomaton (LLSimpleDesignator name,
                              lstates, linmsgs, loutmsgs, ltrans @ ptrans, lftype)) ]
  else
    List.map 
      (fun evalMap ->
       let lldesig = (List.hd (makeLLInstantiation [ evalMap ] paramlist name)) in
       let sevalMap = identMapToStringMap evalMap in
       LLIncompleteAutomaton (lldesig, 
                              List.map (lldesigSubstitutor sevalMap) lstates, 
                              List.map (lldesigSubstitutor sevalMap) linmsgs, 
                              List.map (lldesigSubstitutor sevalMap) loutmsgs,
                              List.map (lltransSubstitutor sevalMap) (ltrans @ ptrans), lftype))
      evalMaps
    

let autDeclInstantiator symtab qMap propOpt autDecl =
  match autDecl with
  | CompleteAutomaton (desig, states, inmsgs, outmsgs, transitions, ftype, _) ->
     instantiateCompleteAutomaton symtab qMap propOpt desig states inmsgs 
                                  outmsgs transitions ftype
  | ChannelAutomaton (desig, chanprops, msgs, ftype, lftype, dftype, _) ->
     instantiateChannelAutomaton symtab qMap propOpt desig chanprops msgs ftype lftype dftype
  | IncompleteAutomaton (desig, states, inmsgs, outmsgs, transitions, ftype, _) ->
     instantiateIncompleteAutomaton symtab qMap propOpt desig states inmsgs 
                                    outmsgs transitions ftype


let rec substInLProp substMap lprop =
  match lprop with
  | LLPropTrue -> LLPropTrue
  | LLPropFalse -> LLPropFalse
  | LLPropEquals (desig1, desig2) ->
     LLPropEquals (lldesigSubstitutor substMap desig1,
                   lldesigSubstitutor substMap desig2)
  | LLPropNot prop1 -> 
     LLPropNot (substInLProp substMap prop1)
  | LLPropAnd (prop1, prop2) -> 
     LLPropAnd (substInLProp substMap prop1,
                substInLProp substMap prop2)
  | LLPropOr (prop1, prop2) -> 
     LLPropOr (substInLProp substMap prop1,
               substInLProp substMap prop2)
  | LLPropTLX prop1 ->
     LLPropTLX (substInLProp substMap prop1)
  | LLPropTLU (prop1, prop2) ->
     LLPropTLU (substInLProp substMap prop1,
                substInLProp substMap prop2)  
     

let rec lowerProp symtab prop = 
  match prop with
  | PropTrue _ -> LLPropTrue
  | PropFalse _ -> LLPropFalse
  | PropDefine ident -> 
     let entry = ST.lookupOrFail symtab ident in 
     let prop1 =
       (match entry with
        | DeclaredExpr (_, prop1) -> prop1
        | _ -> assert false) in
     lowerProp symtab prop1
  | PropEquals (desig1, desig2, _) -> 
     LLPropEquals (lowerDesignator desig1, lowerDesignator desig2)
  | PropNEquals (desig1, desig2, _) ->
     LLPropNot (LLPropEquals (lowerDesignator desig1, lowerDesignator desig2))
  | PropNot (prop1, _) ->
     LLPropNot (lowerProp symtab prop1)
  | PropAnd (prop1, prop2, _) ->
     LLPropAnd (lowerProp symtab prop1, lowerProp symtab prop2)
  | PropOr (prop1, prop2, _) ->
     LLPropOr (lowerProp symtab prop1, lowerProp symtab prop2)
  | PropImplies (prop1, prop2, _) ->
     LLPropOr (LLPropNot (lowerProp symtab prop1), lowerProp symtab prop2)
  | PropIff (prop1, prop2, _) ->
     lowerProp symtab (PropAnd (PropImplies (prop1, prop2, None), 
                                PropImplies (prop2, prop1, None), None))
  | PropForall (idlist, typ, prop1, _)
  | PropExists (idlist, typ, prop1, _) ->
     let lprop1 = lowerProp symtab prop1 in
     let rtype = CK.resolveSymType symtab typ in
     let constructors = 
       (match rtype with
        | SymTypeAnon (constlist, _) -> constlist
        | _ -> assert false)
     in
     let idcplists = 
       (List.map (fun id -> (List.map (fun const -> (id, const)) constructors)) idlist) in
     let cplists = Utils.crossProduct idcplists in
     let sevalMaps = 
       List.map 
         (fun cplist -> 
          identMapToStringMap (Utils.identConstPairList2Map cplist)) cplists in
     let propLists = (List.map (fun sevalmap -> substInLProp sevalmap lprop1) sevalMaps) in
     (match prop with
      | PropForall _ -> 
         List.fold_left 
           (fun acc prop -> LLPropAnd (prop, acc)) LLPropTrue propLists
      | PropExists _ ->
         List.fold_left
           (fun acc prop -> LLPropOr (prop, acc)) LLPropFalse propLists
      | _ -> assert false)
  | PropTLG (prop1, _) ->
     let lprop1 = lowerProp symtab prop1 in
     LLPropNot (LLPropTLU (LLPropTrue, LLPropNot lprop1))
  | PropTLF (prop1, _) ->
     LLPropTLU (LLPropTrue, lowerProp symtab prop1)
  | PropTLX (prop1, _) ->
     LLPropTLX (lowerProp symtab prop1)
  | PropTLU (prop1, prop2, _) ->
     LLPropTLU (lowerProp symtab prop1, lowerProp symtab prop2)
  | PropTLR (prop1, prop2, _) ->
     LLPropNot (LLPropTLU (LLPropNot (lowerProp symtab prop1), 
                           LLPropNot (lowerProp symtab prop2)))

let lowerAndCanonicalizeProp symtab prop =
  Utils.canonicalizePropFP (lowerProp symtab prop)

let lowerSpec symtab spec = 
  match spec with
  | SpecInvar (name, prop, _) -> 
     LLSpecInvar (name, lowerAndCanonicalizeProp symtab prop)
  | SpecLTL (name, prop, jlist, clist, _) ->
     LLSpecLTL (name, lowerAndCanonicalizeProp symtab prop, 
                List.map (lowerAndCanonicalizeProp symtab) jlist,
                List.map (fun (a, b) -> (lowerAndCanonicalizeProp symtab a, 
                                         lowerAndCanonicalizeProp symtab b)) clist)
  | _ -> assert false
  

let lowerProg symtab prog =
  let stdecls, gmsgdecls, autdecls, isdecls, specs = prog in
  let igmsgdecls = instantiateDesigBlock symtab IdentMap.empty None gmsgdecls in
  let lautdecls = 
    List.concat 
      (List.map (instantiateDecl symtab IdentMap.empty None autDeclInstantiator) autdecls) in
  let lspecs = List.map (lowerSpec symtab) specs in
  let linitstateProp = 
    List.fold_left 
      (fun acc prop -> 
       LLPropAnd (lowerAndCanonicalizeProp symtab prop, acc)) 
      LLPropTrue 
      isdecls
  in
  (* Also constrain all the channels to be empty *)
  let linitstateProp =
    Utils.canonicalizePropFP
      (List.fold_left 
         (fun acc laut ->
          match laut with
          | LLCompleteAutomaton (name, _, _, _, _, _, _, _, true) ->
             LLPropAnd (LLPropEquals (LLFieldDesignator (name, "state"), 
                                      LLSimpleDesignator "Empty"),
                        acc)
          | _ -> acc) linitstateProp lautdecls)
  in
  (igmsgdecls, lautdecls, linitstateProp, lspecs)
