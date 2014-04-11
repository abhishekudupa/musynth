open MusynthTypes
open Format

module ST = MusynthSymTab
module CK = MusynthASTChecker
module Utils = MusynthUtils
module AST = MusynthAST
module Chan = MusynthChannel

let rec substInProp substMap prop =
  match prop with
  | PropEquals (desig1, desig2, _) -> 
      let ident1 = Utils.sDesigToIdent desig1 in
      let ident2 = Utils.sDesigToIdent desig2 in
      PropEquals 
        (SimpleDesignator (try IdentMap.find ident1 substMap with Not_found -> ident1),
         SimpleDesignator (try IdentMap.find ident2 substMap with Not_found -> ident2), None)
  | PropNEquals (desig1, desig2, _) -> 
      let ident1 = Utils.sDesigToIdent desig1 in
      let ident2 = Utils.sDesigToIdent desig2 in
      PropNEquals 
        (SimpleDesignator (try IdentMap.find ident1 substMap with Not_found -> ident1),
         SimpleDesignator (try IdentMap.find ident2 substMap with Not_found -> ident2), None)
  | PropAnd (prop1, prop2, _) -> PropAnd (substInProp substMap prop1,
                                          substInProp substMap prop2, None)
  | PropOr (prop1, prop2, _) -> PropOr (substInProp substMap prop1,
                                        substInProp substMap prop2, None)
  | PropImplies (prop1, prop2, _) -> PropImplies (substInProp substMap prop1,
                                                  substInProp substMap prop2, None)
  | PropIff (prop1, prop2, _) -> PropIff (substInProp substMap prop1,
                                          substInProp substMap prop2, None)
  | _ -> assert false

let substInDecl substMap declParamSubstituter decl =
  match decl with
  | DeclSimple (declparam, _) -> DeclSimple (declParamSubstituter substMap declparam, None)
  | DeclQuantified (declparam, qMap, propOpt, _) -> 
      let newQMap = 
        IdentMap.fold
          (fun ident newident acc ->
            try
              let typ = IdentMap.find ident qMap in
              IdentMap.add newident typ acc
            with Not_found -> acc) substMap IdentMap.empty in
      let newProp = 
        (match propOpt with
        | Some prop -> Some (substInProp substMap prop)
        | None -> None)
      in
      DeclQuantified (declParamSubstituter substMap declparam, newQMap, newProp, None)

let rec desigDeclSubstituter substMap desig =
  match desig with
  | SimpleDesignator ident -> 
      SimpleDesignator 
        (try
          IdentMap.find ident substMap
        with Not_found -> ident)

  | IndexDesignator (ndesig, ident, _) ->
      let newdesig = desigDeclSubstituter substMap ndesig in
      IndexDesignator 
        (newdesig, 
         (try 
           IdentMap.find ident substMap
         with Not_found -> ident), None)
  | FieldDesignator (ndesig, ident, _) ->
      FieldDesignator (desigDeclSubstituter substMap ndesig, ident, None)

let transDeclSubstituter substMap trans =
  match trans with
  | TComplete (start, msg, final) ->
      TComplete
        (desigDeclSubstituter substMap start,
         desigDeclSubstituter substMap msg,
         desigDeclSubstituter substMap final)
  | TParametrized (start, msg, var) ->
      TParametrized 
        (desigDeclSubstituter substMap start,
         desigDeclSubstituter substMap msg,
         var)

let autDeclSubstitutor substMap aut =
  match aut with
  | LLIncompleteAutomaton (desig, states, inmsgs, outmsgs, transitions) ->
      LLIncompleteAutomaton (desigDeclSubstituter substMap desig,
                             List.map (desigDeclSubstituter substMap) states,
                             List.map (desigDeclSubstituter substMap) inmsgs,
                             List.map (desigDeclSubstituter substMap) outmsgs,
                             List.map (transDeclSubstituter substMap) transitions)

  | LLCompleteAutomaton (desig, states, inmsgs, outmsgs, transitions) ->
      LLCompleteAutomaton (desigDeclSubstituter substMap desig,
                           List.map (desigDeclSubstituter substMap) states,
                           List.map (desigDeclSubstituter substMap) inmsgs,
                           List.map (desigDeclSubstituter substMap) outmsgs,
                           List.map (transDeclSubstituter substMap) transitions)


let instDecl symtab declInstantiator decl =
  match decl with 
  | DeclSimple (declparam, _) -> declInstantiator symtab IdentMap.empty None declparam
  | DeclQuantified (declparam, qMap, propOpt, _) -> 
      begin
        ST.push symtab;
        IdentMap.iter 
          (fun ident typ -> 
            let name, _ = ident in
            ST.bind symtab ident (SymVarName (name, typ))) qMap;
        let retval = declInstantiator symtab qMap propOpt declparam in
        ignore (ST.pop symtab);
        retval
      end

let lowerQMap symtab qMap =
  IdentMap.fold 
    (fun ident typ acc ->
      let rtyp = CK.resolveSymType symtab typ in
      IdentMap.add ident rtyp acc) qMap IdentMap.empty

let desigDeclInstantiator symtab qMap propOpt msgDecl = 
  let ident, paramlist = CK.destructDesigDecl msgDecl in
  let qMap = lowerQMap symtab qMap in
  if paramlist = [] then
    [ SimpleDesignator ident ]
  else
      let maps = Utils.getMapsForProp paramlist qMap propOpt in
      List.map 
        (fun map ->
          desigDeclSubstituter map msgDecl) maps

let transDeclInstantiator symtab qMap propOpt transdecl =
  let startDesig, msgDesig, finalDesig = transdecl in
  let sident, sparamlist = CK.destructDesigDecl startDesig in
  let mident, mparamlist = CK.destructDesigDecl msgDesig in
  let fident, fparamlist = CK.destructDesigDecl finalDesig in
  let qMap = lowerQMap symtab qMap in
  let combparamset = List.fold_left (fun set param -> IdentSet.add param set) IdentSet.empty sparamlist in
  let combparamset = List.fold_left (fun set param -> IdentSet.add param set) combparamset mparamlist in
  let combparamset = List.fold_left (fun set param -> IdentSet.add param set) combparamset fparamlist in
  let combparamlist = IdentSet.elements combparamset in
  if combparamlist = [] then
    [ TComplete (SimpleDesignator sident, SimpleDesignator mident, SimpleDesignator fident) ]
  else
    let maps = Utils.getMapsForProp combparamlist qMap propOpt in
    List.map 
      (fun map ->
        TComplete
          (desigDeclSubstituter map startDesig,
           desigDeclSubstituter map msgDesig,
           desigDeclSubstituter map finalDesig)) maps


let reduceChannelAut symtab auttuple qMap propOpt =
  let desig, chanprop, msgblock, _ = auttuple in
  let inmsgblock = msgblock in
  let outmsgblock = List.map (CK.convertDesigDeclToPrimed) msgblock in
  let linmsgs = List.concat (List.map (instDecl symtab desigDeclInstantiator) inmsgblock) in
  let loutmsgs = List.concat (List.map (instDecl symtab desigDeclInstantiator) outmsgblock) in
  let states, transitions = Chan.buildChannelAutomaton linmsgs loutmsgs chanprop in
  LLCompleteAutomaton (desig, states, linmsgs, loutmsgs, transitions)
  
let getDesigFromDecl decl =
  match decl with
  | DeclSimple (param, _) -> param, IdentMap.empty, None
  | DeclQuantified (param, qMap, propOpt, _) -> param, qMap, propOpt

let lowerIncompleteAutomaton symtab autProps qMap propOpt = 
  let desig, states, inmsgs, outmsgs, transitions = autProps in
  (* lower all the predefined transitions into a list *)
  let ltrans = List.concat (List.map (instDecl symtab transDeclInstantiator) transitions) in
  let lstates = 
    List.concat 
      (List.map 
         (fun (statedecl, annot) -> 
           instDecl symtab desigDeclInstantiator statedecl) states) in
  let incompletestates = 
    List.fold_left 
      (fun acc (statedecl, annot) ->
        match annot with
        | AnnotIncomplete _ -> statedecl :: acc
        | _ -> acc) [] states in
  let lincstates = List.concat (List.map (instDecl symtab desigDeclInstantiator) incompletestates) in
  let linmsgs = List.concat (List.map (instDecl symtab desigDeclInstantiator) inmsgs) in
  let loutmsgs = List.concat (List.map (instDecl symtab desigDeclInstantiator) outmsgs) in

  let statestrings = List.map (fun lstate -> AST.astToString AST.pLLIdent lstate) lstates in
  let targetset = List.fold_left (fun acc str -> StringSet.add str acc) StringSet.empty statestrings in
  let targetset = StringSet.add "defer" targetset in
  let newtrans = 
    List.concat 
      (List.map
         (fun incstate ->
           List.fold_left 
             (fun acc msg -> 
               (TParametrized (incstate, 
                              msg, 
                              ("synth_" ^ (string_of_int (Utils.getuid ())), 
                               targetset))) :: acc) [] linmsgs) lincstates)
  in
  let newtrans = 
    List.fold_left
      (fun acc ntrans ->
        let state, msg =
          (match ntrans with
          | TParametrized (state, msg, _) -> state, msg
          | _ -> assert false)
        in
        if (List.exists 
              (fun trans ->
                match trans with
              | TComplete (ostate, omsg, _) ->
                  (AST.astToString AST.pLLIdent ostate) = 
                  (AST.astToString AST.pLLIdent state) && 
                  (AST.astToString AST.pLLIdent omsg) = 
                  (AST.astToString AST.pLLIdent msg)
              | _ -> assert false) ltrans)
      then
          acc
        else
          ntrans :: acc) [] newtrans
  in
  LLIncompleteAutomaton (desig, lstates, linmsgs, loutmsgs, ltrans @ newtrans)

let lowerCompleteAutomaton symtab autProps qMap propOpt =
  let desig, states, inmsgs, outmsgs, transitions = autProps in
  let lstates = 
    List.concat 
      (List.map 
         (fun (statedecl, annot) -> 
           instDecl symtab desigDeclInstantiator statedecl) states) in
  let linmsgs = List.concat (List.map (instDecl symtab desigDeclInstantiator) inmsgs) in
  let loutmsgs = List.concat (List.map (instDecl symtab desigDeclInstantiator) outmsgs) in
  let ltrans = List.concat (List.map (instDecl symtab transDeclInstantiator) transitions) in
  LLCompleteAutomaton (desig, lstates, linmsgs, loutmsgs, ltrans)
  
let autDeclInstantiator symtab qMap propOpt autDecl =
  (* before lowering an automaton we must complete it if incomplete *)
  (* we also need to blow up channel automaton *)
  let desig, low = 
    match autDecl with
    | ChannelAutomaton (desig, chanprop, msgblock, loc) ->
       desig, reduceChannelAut symtab (desig, chanprop, msgblock, loc) qMap propOpt

    | CompleteAutomaton (desig, stateDecls, inmsgs, outmsgs, transitions, loc) -> 
        desig, lowerCompleteAutomaton symtab (desig, stateDecls, inmsgs, outmsgs, transitions) qMap propOpt
    | IncompleteAutomaton (desig, stateDecls, inmsgs, outmsgs, transitions, loc) -> 
        desig, lowerIncompleteAutomaton symtab (desig, stateDecls, inmsgs, outmsgs, transitions) qMap propOpt
  in
  let ident, paramlist = CK.destructDesigDecl desig in
  let qMap = lowerQMap symtab qMap in
  if paramlist = [] then
    [ low ]
  else
    let maps = Utils.getMapsForProp paramlist qMap propOpt in
    List.map 
      (fun map ->
        autDeclSubstitutor map low) maps
      
let instGMsgDecls symtab gmesgDecls =
  List.concat (List.map (instDecl symtab desigDeclInstantiator) gmesgDecls)

let lowerProg symtab prog =
  let stdecls, gmsgdecls, autdecls, isdecls, specs = prog in
  let igmsgdecls = instGMsgDecls symtab gmsgdecls in
  let lautdecls = List.concat (List.map (instDecl symtab autDeclInstantiator) autdecls) in
  List.iter (fun aut -> fprintf std_formatter "%a@," AST.pLLAutomaton aut) lautdecls
