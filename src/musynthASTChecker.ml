(* semantic checks on AST *)

open MusynthAST
open MusynthTypes
open Format
module ST = MusynthSymTab

(* var for the automaton we're processing *)
(* breaks functional programming rules, but wtf *)

let curAutomatonName = ref ""

let rec resolveSymType symtab st =
  match st with
  | SymTypeNamed (stident, _) ->
      let entry = ST.lookupOrFail symtab stident in
      (match entry with
      | SymtypeName (_, ntype) -> resolveSymType symtab ntype
      | _ -> 
          let name, loc = stident in
          raise (SemanticError ("\"" ^ name ^ "\" does not name a type", loc)))
  | SymTypeAnon (idlist, _) ->
      st
      
let checkSymTypeDecl symtab stdecl =
  let ident, symtype = stdecl in
  let name, _ = ident in
  let resst = resolveSymType symtab symtype in
  ST.bind symtab ident (SymtypeName (name, resst))

let rec destructDesigDecl desig = 
  match desig with
  | SimpleDesignator ident -> (ident, [])
  | FieldDesignator (ndesig, ident, _) -> 
      let name, loc = ident in
      raise (SemanticError ("Declarations cannot contain index expressions", loc))
  | IndexDesignator (ndesig, indexid, _) ->
      let ident, paramlist = destructDesigDecl ndesig in
      (ident, paramlist @ [ indexid ])

let getTypeListForIdent symtab ident =
  let entry = ST.lookupOrFail symtab ident in
  match entry with
  | StateName (_, typlist, _, _)
  | MsgName (_, _, typlist, _)
  | AutomatonName (_, _, typlist, _, _) -> typlist
  | _ -> []

let checkTypeLists exptypelist acttypeparamlist =
  List.iter2 
    (fun exptype (acttype, param) ->
      if exptype = acttype then 
        ()
      else
        let paramname, paramloc = param in
        raise (SemanticError ("Expected type \"" ^ (astToString pSymType exptype) ^ 
                              "\" but \"" ^ paramname ^ "\" has type \"" ^ 
                              (astToString pSymType acttype) ^ "\"", paramloc)))
    exptypelist acttypeparamlist

let getRValType symtab rval =
  let rec getRValTypeRec symtab desig paramlist =
    let resolveIdent symtab ident paramlist =
      let acttypelist = 
        List.map 
          (fun param -> 
            let typ = ST.lookupVar symtab param in
            (typ, param)) paramlist
      in
      let resolvedIdent = ST.lookupOrFail symtab ident in
      let exptypelist = getTypeListForIdent symtab ident in
      (try
        checkTypeLists exptypelist acttypelist
      with
        Invalid_argument ("List.iter2") -> 
          let name, loc = ident in
          raise (SemanticError ("Parameter lists for identifier \"" ^ name ^ "\" incorrect", loc)));
      resolvedIdent
    in
        
    match desig with
    | SimpleDesignator ident -> resolveIdent symtab ident paramlist
    | FieldDesignator (ndesig, ident, loc) ->
        (* recurse on the nested designator first *)
        let resolvedAut = getRValTypeRec symtab ndesig [] in
        begin
          match resolvedAut with
          | AutomatonName (_, _, _, _, scope) ->
              ST.pushScope symtab scope;
              let retval = resolveIdent symtab ident paramlist in
              ignore (ST.pop symtab);
              retval
          | _ -> raise (SemanticError ("Invalid index operation", loc))
        end
    | IndexDesignator (ndesig, indexident, _) ->
        getRValTypeRec symtab ndesig (indexident :: paramlist)
  in
  getRValTypeRec symtab rval []

let checkTypeCompatibility entry1 entry2 locopt = 
  match entry1, entry2 with
  | StateName _, StateName _ -> raise (ConstantExpression locopt)
  | StateVar (memlist, autname1), StateName (name, _, _, autname2)
  | StateName (name, _, _, autname2), StateVar (memlist, autname1) ->
      if (autname1 <> autname2) then
        raise (SemanticError ("Statename \"" ^ autname2 ^ "\" is part of automaton \"" ^
                              autname2 ^ "\" and not of \"" ^ autname1 ^ "\"", locopt))
      else 
        ();
      if ((List.mem name memlist) <> true) then
        raise (SemanticError ("Invalid types in comparison", locopt))
      else
        ()
  | SymVarName (_, typ1), SymVarName (_, typ2) ->
      if (typ1 <> typ2) then
        raise (SemanticError ("Invalid types in comparison", locopt))
      else
        ()
  | _ -> raise (SemanticError ("Invalid types in comparison", locopt))

let rec checkPureProp symtab prop = 
  match prop with
  | PropTrue _
  | PropFalse _ -> ()
  | PropEquals (desig1, desig2, locopt)
  | PropNEquals (desig1, desig2, locopt) -> 
      let entry1 = getRValType symtab desig1 in
      let entry2 = getRValType symtab desig2 in
      checkTypeCompatibility entry1 entry2 locopt;
  | PropNot (prop1, _) -> checkPureProp symtab prop1
  | PropAnd (prop1, prop2, _)
  | PropOr (prop1, prop2, _)
  | PropImplies (prop1, prop2, _)
  | PropIff (prop1, prop2, _) ->
      checkPureProp symtab prop1;
      checkPureProp symtab prop2
  | PropForall (idlist, typ, prop1, _)
  | PropExists (idlist, typ, prop1, _) ->
      let acttype = resolveSymType symtab typ in
      ST.push symtab;
      List.iter (fun ident -> 
        let name, _ = ident in
        ST.bind symtab ident (SymVarName (name, acttype))) idlist;
      checkPureProp symtab prop1;
      ignore (ST.pop symtab)
  | _ -> raise (SemanticError ("Expected pure propositional formula, but got:\n" ^ 
                               (astToString pProp prop), None))

let rec checkPureQProp symtab prop = 
  match prop with
  | PropTrue _
  | PropFalse _ -> ()
  | PropEquals (desig1, desig2, locopt)
  | PropNEquals (desig1, desig2, locopt) -> 
      let entry1 = getRValType symtab desig1 in
      let entry2 = getRValType symtab desig2 in
      checkTypeCompatibility entry1 entry2 locopt;
      (match entry1, entry2 with
      | SymVarName _, SymVarName _ -> ()
      | _ -> raise (SemanticError ("Quantifier constraint can only contain locally declared vars", 
                                   locopt)))

  | PropNot (prop1, _) -> checkPureQProp symtab prop1
  | PropAnd (prop1, prop2, _)
  | PropOr (prop1, prop2, _)
  | PropImplies (prop1, prop2, _)
  | PropIff (prop1, prop2, _) ->
      checkPureQProp symtab prop1;
      checkPureQProp symtab prop2
  | PropForall (idlist, typ, prop1, _)
  | PropExists (idlist, typ, prop1, _) ->
      let acttype = resolveSymType symtab typ in
      ST.push symtab;
      List.iter (fun ident -> 
        let name, _ = ident in
        ST.bind symtab ident (SymVarName (name, acttype))) idlist;
      checkPureQProp symtab prop1;
      ignore (ST.pop symtab)
  | _ -> raise (SemanticError ("Expected pure propositional formula, but got:\n" ^ 
                               (astToString pProp prop), None))

(* checker for declarations which involve a designator *)
let checkDesigDecl symtab desig loc =
  let ident, paramlist = destructDesigDecl desig in
  (* check that the paramlist have all been declared *)
  let typelist = List.map (ST.lookupVar symtab) paramlist in
  (ident, typelist)

let checkTransDecl symtab trans locopt =
  let sstate, msg, fstate = trans in
  let sstateentry = getRValType symtab sstate in
  let msgentry = getRValType symtab msg in
  let fstateentry = getRValType symtab fstate in
  match sstateentry, msgentry, fstateentry with
  | StateName _, MsgName _, StateName _ -> ("", [])
  | _ -> raise (SemanticError ("Error in transition", locopt))

let checkDecl symtab declParamChecker decl =
  match decl with 
  | DeclSimple (declParam, loc) ->
      ST.push symtab;
      let ident, typelist = declParamChecker symtab declParam loc in
      (ident, typelist, None, ST.pop symtab)

  | DeclQuantified (declParam, qMap, propOpt, loc) ->
      ST.push symtab;
      IdentMap.iter
        (fun ident typ ->
          let name, loc = ident in
          ST.bind symtab ident (SymVarName (name, resolveSymType symtab typ))) qMap;
      (match propOpt with
      | Some prop -> checkPureProp symtab prop
      | None -> ());
      ST.push symtab;
      let ident, typelist = declParamChecker symtab declParam loc in
      let retval = (ident, typelist, propOpt, ST.pop symtab) in
      ignore (ST.pop symtab);
      retval

let checkStateDecl symtab statedecl =
  let ident, typelist, propOpt, _ = checkDecl symtab checkDesigDecl statedecl in
  let name, _ = ident in
  let entry = (StateName (name, typelist, propOpt, !curAutomatonName)) in
  ST.bind symtab ident entry;
  ST.bindGlobal symtab ident entry

let checkMsgDecl symtab msgtype msgdecl =
  let ident, typelist, propOpt, _ = checkDecl symtab checkDesigDecl msgdecl in
  let name, _ = ident in
  ST.bind symtab ident (MsgName (name, msgtype, typelist, propOpt))

let checkStateDeclBlock symtab block annotallowed =
  List.iter 
    (fun sdecl -> 
      let decl, annot = sdecl in
      checkStateDecl symtab decl;
      if ((not annotallowed) && annot <> AnnotNone) then
        raise (SemanticError ("State Annotations not allowed in complete automata", None))
      else
        ()) block

(* checker for automata *)
let checkAutDef symtab autdef loc =
  let checkAutDefInternal autdesig sblock inblock outblock transblock loc annotallowed =
    let autname, autparamtypelist = checkDesigDecl symtab autdesig loc in
    let actname, _ = autname in
    curAutomatonName := actname;
    checkStateDeclBlock symtab sblock true;
    List.iter (checkMsgDecl symtab InputMsg) inblock;
    List.iter (checkMsgDecl symtab OutputMsg) outblock;
    List.iter (fun tr -> ignore (checkDecl symtab checkTransDecl tr)) transblock;
    (* bind the state variable now *)
    let topscope = ST.peek symtab in
    let snentries = IdentMap.fold 
        (fun key valu lst ->
          match valu with
          | StateName (name, _, _, _) -> name :: lst
          | _ -> lst) !topscope [] in
    ST.bind symtab ("state", None) (StateVar (snentries, !curAutomatonName));
    curAutomatonName := "";
    (autname, autparamtypelist)
  in
  match autdef with
  | CompleteAutomaton (autdesig, sblock, inblock, outblock, transblock, loc) ->
      checkAutDefInternal autdesig sblock inblock outblock transblock loc false
  | IncompleteAutomaton (autdesig, sblock, inblock, outblock, transblock, loc) ->
      checkAutDefInternal autdesig sblock inblock outblock transblock loc true
  | ChannelAutomaton (autdesig, chanProp, msgs, loc) ->
      let autname, autparamtypelist = checkDesigDecl symtab autdesig loc in
      List.iter (checkMsgDecl symtab InputMsg) msgs;
      (autname, autparamtypelist)

let checkInitStateDecl symtab decl loc =
  List.iter
    (fun (lhs, rhs) ->
      let lhsEntry = getRValType symtab lhs in
      let rhsEntry = getRValType symtab rhs in
      checkTypeCompatibility lhsEntry rhsEntry loc) decl;
  ("", [])

let rec checkProp symtab prop =
  match prop with
  | PropTrue _ -> ()
  | PropFalse _ -> ()
  | PropEquals (desig1, desig2, loc)
  | PropNEquals (desig1, desig2, loc) ->
      let lhsentry = getRValType symtab desig1 in
      let rhsEntry = getRValType symtab desig2 in
      checkTypeCompatibility lhsentry rhsEntry loc
  | PropNot (prop1, _)
  | PropCTLAG (prop1, _)
  | PropCTLAF (prop1, _)
  | PropCTLAX (prop1, _)
  | PropCTLEG (prop1, _)
  | PropCTLEF (prop1, _)
  | PropCTLEX (prop1, _) ->
      checkProp symtab prop1
  | PropAnd (prop1, prop2, _)
  | PropOr (prop1, prop2, _)
  | PropImplies (prop1, prop2, _)
  | PropIff (prop1, prop2, _)
  | PropCTLAU (prop1, prop2, _)
  | PropCTLEU (prop1, prop2, _) ->
      checkProp symtab prop1;
      checkProp symtab prop2;
  | PropForall (identlist, typ, prop1, _)
  | PropExists (identlist, typ, prop1, _) ->
      ST.push symtab;
      let rtype = resolveSymType symtab typ in
      List.iter 
        (fun ident ->
          let name, _ = ident in
          ST.bind symtab ident (SymVarName (name, rtype))) identlist;
      checkProp symtab prop1;
      ignore (ST.pop symtab)

let rec checkSpec symtab spec =
  match spec with 
  | SpecInvar (specname, prop, loc) ->
      checkPureProp symtab prop;
      ignore (ST.bindGlobal symtab (specname, None) (InvariantName (specname, prop)))
  | SpecCTL (specname, prop, loc) ->
      checkProp symtab prop;
      ignore (ST.bindGlobal symtab (specname, None) (CTLSpecName (specname, prop)))

let checkProg symtab prog =
  let symtypedecls, autodecls, initstatedecls, specs = prog in
  List.iter (checkSymTypeDecl symtab) symtypedecls;
  List.iter
    (fun aut -> 
      let autname, autparamlist, autPropOpt, autscope = checkDecl symtab checkAutDef aut in
      let name, _ = autname in
      match aut with
      | DeclSimple (CompleteAutomaton _, _)
      | DeclQuantified (CompleteAutomaton _, _, _, _) ->
          ST.bind symtab autname 
            (AutomatonName (name, CompleteAutType, autparamlist, autPropOpt, autscope))
      | DeclSimple (IncompleteAutomaton _, _)
      | DeclQuantified (IncompleteAutomaton _, _, _, _) ->
          ST.bind symtab autname 
            (AutomatonName (name, PartialAutType, autparamlist, autPropOpt, autscope))
      | DeclSimple (ChannelAutomaton _, _)
      | DeclQuantified (ChannelAutomaton _, _, _, _) ->
          ST.bind symtab autname 
            (AutomatonName (name, ChannelAutType, autparamlist, autPropOpt, autscope))) 
    autodecls;
  List.iter (fun decl -> ignore (checkDecl symtab checkInitStateDecl decl)) initstatedecls;
  List.iter (checkSpec symtab) specs
  
