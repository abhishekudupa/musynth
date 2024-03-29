(* semantic checks on AST *)

open MusynthAST
open MusynthTypes
open Format
module ST = MusynthSymTab

(* the name of the automaton we're currently processing *)
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
  ST.bind symtab ident (SymtypeName (name, resst));
  (* bind the constructors *)
  let constlist = 
    (match resst with
     | SymTypeAnon (identlist, _) -> identlist
     | _ -> assert false) 
  in
  List.iter 
    (fun ident ->
     let name, _ = ident in
     ST.bind symtab ident (SymtypeConst (name, resst)))
    constlist

let rec destructDesigDecl desig = 
  match desig with
  | SimpleDesignator ident -> (ident, [])
  | FieldDesignator (ndesig, ident, _) -> 
     let name, loc = ident in
     raise (SemanticError ("Declarations cannot contain field expressions", loc))
  | IndexDesignator (ndesig, indexid, _) ->
     let ident, paramlist = destructDesigDecl ndesig in
     (ident, paramlist @ [ indexid ])

(* return the set of parameters and the condition on the params *)
let getObligationsForIdent symtab ident =
  let entry = ST.lookupOrFail symtab ident in
  match entry with
  | StateName ((_, paramtypelist, propOpt), _) -> (paramtypelist, propOpt)
  | GlobalMsgName (_, paramtypelist, propOpt) -> (paramtypelist, propOpt)
  | AutomatonMsgName ((_, paramtypelist, propOpt), _) -> (paramtypelist, propOpt)
  | AutomatonName (_, paramtypelist, propOpt) -> (paramtypelist, propOpt)
  | SymVarName _ -> ([], None)
  | StateVar _ -> ([], None)
  | _ -> assert false

let getTypeObligationsForIdent symtab ident =
  let paramtypelist, _ = getObligationsForIdent symtab ident in
  List.map (fun (paramname, typ) -> typ) paramtypelist

let checkTypeLists symtab ident exptypelist paramidentlist =
  let name, loc = ident in
  if (List.length exptypelist) <> (List.length paramidentlist) then
    raise (SemanticError ("\"" ^ name ^ "\" expects " ^ 
                            (string_of_int (List.length exptypelist)) ^ 
                              " parameters", loc))
  else
    begin
      List.iter2
        (fun exptype paramident ->
         let acttype = ST.lookupVar symtab paramident in
         if exptype = acttype then 
           ()
         else
           let paramname, paramloc = paramident in
           raise (SemanticError ("Expected type \"" ^ (astToString pSymType exptype) ^ 
                                   "\" but \"" ^ paramname ^ "\" has type \"" ^ 
                                     (astToString pSymType acttype) ^ "\"", paramloc)))
        exptypelist paramidentlist
    end

(* evaluates the entry in the symtab for the designator *)
let getDesigType symtab desig =
  let rec getDesigTypeInt symtab desig paramidentlist =
    match desig with
    | SimpleDesignator ident ->
       let exptypelist = getTypeObligationsForIdent symtab ident in
       checkTypeLists symtab ident exptypelist paramidentlist;
       ST.lookupOrFail symtab ident
    | FieldDesignator (ndesig, ident, loc) ->
       let autentry = getDesigTypeInt symtab ndesig [] in
       begin
         match autentry with
         | AutomatonName ((_, _, scope), _, _) -> 
            ST.pushScope symtab scope;
            let exptypelist = getTypeObligationsForIdent symtab ident in
            checkTypeLists symtab ident exptypelist paramidentlist;
            let retval = ST.lookupOrFail symtab ident in
            ignore (ST.pop symtab);
            retval
         | _ -> raise (SemanticError ("Expected an automaton identifier", loc))
       end
    | IndexDesignator (ndesig, indexident, _) ->
       getDesigTypeInt symtab ndesig (indexident :: paramidentlist)
  in
  getDesigTypeInt symtab desig []

let checkTypeCompatibility symtab desig1 desig2 locopt =
  let entry1 = getDesigType symtab desig1 in
  let entry2 = getDesigType symtab desig2 in
  match entry1, entry2 with
  | SymVarName (_, typ1), SymVarName (_, typ2) ->
     if typ1 <> typ2 then
       raise (SemanticError (((astToString pDesignator desig1) ^ " and " ^ 
                                (astToString pDesignator desig2) ^ " are not type compatible"),
                             locopt))
     else
       (entry1, entry2)

  | SymVarName _, SymtypeConst _ 
  | SymtypeConst _, SymVarName _ ->
     raise (SemanticError ("Symmetric type constants cannot be referred to explicitly", locopt))
  | StateVar aut1, StateVar aut2
  | StateName (_, aut1), StateName (_, aut2)
  | StateVar aut1, StateName (_, aut2)
  | StateName (_, aut1), StateVar aut2 ->
     if aut1 <> aut2 then
       raise (SemanticError (((astToString pDesignator desig1) ^ " and " ^ 
                                (astToString pDesignator desig2) ^ " are not type compatible"),
                             locopt))
     else
       (entry1, entry2)
  | _ -> 
     raise (SemanticError (((astToString pDesignator desig1) ^ " and " ^ 
                              (astToString pDesignator desig2) ^ " are not type compatible"),
                           locopt))

let rec checkPureProp symtab prop = 
  match prop with
  | PropTrue _
  | PropFalse _ -> ()
  | PropDefine ident ->
     let name, loc = ident in
     let entry = ST.lookupOrFail symtab ident in
     begin
       match entry with
       | DeclaredExpr (_, prop1) -> 
          begin
            try
              checkPureProp symtab prop1
            with
            | SemanticError (msg, locopt) ->
               let newmsg = msg ^ "\nError encountered in named/declared " ^ 
                              "expression macro:\n" ^ name
               in
               raise (SemanticError (newmsg, loc))
          end
       | _ -> raise (SemanticError ("Identifier \"" ^ name ^ "\" does not refer to a " ^ 
                                      "pre-defined proposition/macro", loc))
     end
  | PropEquals (desig1, desig2, locopt)
  | PropNEquals (desig1, desig2, locopt) -> 
     ignore (checkTypeCompatibility symtab desig1 desig2 locopt)
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
     let entry1, entry2 = checkTypeCompatibility symtab desig1 desig2 locopt in
     (match entry1, entry2 with
      | SymVarName _, SymVarName _ -> ()
      | _ -> raise (SemanticError ("Quantifier constraint can only refer to variables " ^ 
                                     "quantified over symmetric types", locopt)))
  | PropNot (prop1, _) -> checkPureQProp symtab prop1
  | PropAnd (prop1, prop2, _)
  | PropOr (prop1, prop2, _)
  | PropImplies (prop1, prop2, _)
  | PropIff (prop1, prop2, _) ->
     checkPureQProp symtab prop1;
     checkPureQProp symtab prop2
  | _ -> raise (SemanticError ("Expected quantifier free pure propositional formula, but got:\n" ^ 
                                 (astToString pProp prop), None))

(* checker for declarations which involve a designator *)
let desigDeclChecker symtab desig loc =
  let ident, paramlist = destructDesigDecl desig in
  (* check that the paramlist have all been declared *)
  let paramtypelist = 
    List.map 
      (fun paramname -> (paramname, ST.lookupVar symtab paramname)) 
      paramlist
  in
  (ident, paramtypelist)

let transChecker symtab trans locopt =
  let sstate, msg, fstate = trans in
  let sstateentry = getDesigType symtab sstate in
  let msgentry = getDesigType symtab msg in
  let fstateentry = getDesigType symtab fstate in
  match sstateentry, msgentry, fstateentry with
  | StateName (_, aut1), AutomatonMsgName (_, aut2), StateName (_, aut3) -> 
     if ((aut1 <> aut2) || (aut2 <> aut3)) then
       raise (SemanticError ("States and messages in transition must refer " ^ 
                               "only to the automaton's states " ^ 
                                 "and messages", locopt))
     else
       ("", [])
  | _ -> raise (SemanticError ("Error in transition", locopt))

let autMsgDeclChecker symtab desig loc =
  let ident, paramlist = destructDesigDecl desig in
  let paramtypelist = 
    List.map 
      (fun paramname -> (paramname, ST.lookupVar symtab paramname)) 
      paramlist
  in
  let entry = getDesigType symtab desig in
  match entry with
  | GlobalMsgName _ -> (ident, paramtypelist)
  | _ -> raise (SemanticError ("Undeclared message designator: " ^ (astToString pDesignator desig),
                               loc))

let checkDecl symtab declParamChecker decl =
  match decl with 
  | DeclSimple (declParam, loc) ->
     ST.push symtab;
     let ident, paramtypelist = declParamChecker symtab declParam loc in
     (ident, paramtypelist, None, ST.pop symtab)

  | DeclQuantified (declParam, qMap, propOpt, loc) ->
     ST.push symtab;
     IdentMap.iter
       (fun ident typ ->
        let name, loc = ident in
        if (ST.lookup symtab ident) <> None then
          raise (SemanticError ("Variable \"" ^ name ^ "\" shadows an earlier version", loc))
        else
          ();
        ST.bind symtab ident (SymVarName (name, resolveSymType symtab typ))) qMap;
     (match propOpt with
      | Some prop -> checkPureProp symtab prop
      | None -> ());
     ST.push symtab;
     let ident, paramtypelist = declParamChecker symtab declParam loc in
     let retval = (ident, paramtypelist, propOpt, ST.pop symtab) in
     ignore (ST.pop symtab);
     retval


let cvtParamTypeListForSymtab paramtypelist = 
  List.map 
    (fun (ident, typ) -> 
     let name, _ = ident in
     (name, typ))
    paramtypelist

let checkGlobalMsgDecl symtab msgdecl =
  let ident, paramtypelist, propOpt, _ = checkDecl symtab desigDeclChecker msgdecl in
  let name, _ = ident in
  ST.bind symtab ident (GlobalMsgName (name, cvtParamTypeListForSymtab paramtypelist, propOpt))

let checkStateDecl symtab statedecl =
  let ident, paramtypelist, propOpt, _ = checkDecl symtab desigDeclChecker statedecl in
  let name, _ = ident in
  let entry = StateName ((name, cvtParamTypeListForSymtab paramtypelist, propOpt), !curAutomatonName) in
  ST.bind symtab ident entry;
  ST.bindGlobal symtab ident entry

let checkAutomatonMsgDecl symtab msgtype msgdecl =
  let ident, paramtypelist, propOpt, _ = checkDecl symtab autMsgDeclChecker msgdecl in
  let name, _ = ident in
  ST.bind symtab ident (AutomatonMsgName (((name, msgtype), 
                                           cvtParamTypeListForSymtab paramtypelist, propOpt),
                                          !curAutomatonName))

let checkAnnotationMsgDecl symtab msgdecl =
  ignore (checkDecl symtab autMsgDeclChecker msgdecl)

let checkAnnotation symtab annot =
  match annot with
  | AnnotIncompleteEventList (msgdecls, _)
  | AnnotIncompleteNumEventList (_, msgdecls, _) -> List.iter (checkAnnotationMsgDecl symtab) msgdecls
  | _ -> ()

let checkStateDeclBlock symtab block annotallowed =
  List.iter 
    (fun sdecl -> 
     let decl, annot = sdecl in
     checkStateDecl symtab decl;
     if ((not annotallowed) && annot <> AnnotNone) then
       raise (SemanticError ("State Annotations not allowed in complete automata", None))
     else
       checkAnnotation symtab annot) block

let checkGlobalMsgDeclBlock symtab block =
  List.iter (checkGlobalMsgDecl symtab) block

let checkAutomatonMsgDeclBlock symtab msgtype block =
  List.iter (checkAutomatonMsgDecl symtab msgtype) block

let rec convertDesigToPrimed desig =
  match desig with
  | SimpleDesignator ident ->
     let name, loc = ident in 
     SimpleDesignator (name ^ "'", loc)
  | FieldDesignator _ -> assert false
  | IndexDesignator (ndesig, ident, loc) ->
     IndexDesignator (convertDesigToPrimed ndesig, ident, loc)

let convertDesigDeclToPrimed decl =
  match decl with
  | DeclSimple (desig, loc) -> DeclSimple (convertDesigToPrimed desig, loc)
  | DeclQuantified (desig, qMap, propOpt, loc) ->
     DeclQuantified (convertDesigToPrimed desig, qMap, propOpt, loc)

(* checker for automata *)
let checkAutDef symtab autdef loc =
  let checkAutDefInternal autdesig sblock inblock outblock transblock loc annotallowed =
    let autname, autparamtypelist = desigDeclChecker symtab autdesig loc in
    let actname, _ = autname in
    curAutomatonName := actname;
    checkStateDeclBlock symtab sblock true;
    List.iter (checkAutomatonMsgDecl symtab InputMsg) inblock;
    List.iter (checkAutomatonMsgDecl symtab OutputMsg) outblock;
    List.iter (fun tr -> ignore (checkDecl symtab transChecker tr)) transblock;
    (* bind the state variable now *)
    ST.bind symtab ("state", None) (StateVar !curAutomatonName);
    curAutomatonName := "";
    (autname, autparamtypelist)
  in
  match autdef with
  | CompleteAutomaton (autdesig, sblock, inblock, outblock, transblock, _, loc) ->
     checkAutDefInternal autdesig sblock inblock outblock transblock loc false
  | IncompleteAutomaton (autdesig, sblock, inblock, outblock, transblock, _, loc) ->
     checkAutDefInternal autdesig sblock inblock outblock transblock loc true
  | ChannelAutomaton (autdesig, chanProp, msgs, ftype, lftype, dftype, loc) ->
     let autname, autparamtypelist = desigDeclChecker symtab autdesig loc in
     List.iter (checkAutomatonMsgDecl symtab InputMsg) msgs;
     List.iter 
       (fun msgdecl ->
        checkAutomatonMsgDecl symtab OutputMsg (convertDesigDeclToPrimed msgdecl)) msgs;
     (match chanProp with
      | _, ChanLossless _, _, ChanNonBlocking loc, _ ->
         raise (SemanticError ("Only lossy channels can be declared non-blocking", loc))
      | _ -> ());
     (autname, autparamtypelist)

let initStateDeclChecker symtab propList =
  List.iter (fun prop -> checkPureProp symtab prop) propList

let rec checkProp symtab prop =
  match prop with
  | PropTrue _ -> ()
  | PropFalse _ -> ()
  | PropDefine ident ->
     let entry = ST.lookupOrFail symtab ident in
     let name, loc = ident in
     begin
       match entry with
       | DeclaredExpr _ -> ()
       | _ -> raise (SemanticError ("Identifier \"" ^ name ^ "\" could not be resolved", loc))
     end
  | PropEquals (desig1, desig2, loc)
  | PropNEquals (desig1, desig2, loc) ->
     ignore (checkTypeCompatibility symtab desig1 desig2 loc)
  | PropNot (prop1, _)
  | PropTLG (prop1, _)
  | PropTLF (prop1, _)
  | PropTLX (prop1, _) ->
     checkProp symtab prop1
  | PropAnd (prop1, prop2, _)
  | PropOr (prop1, prop2, _)
  | PropImplies (prop1, prop2, _)
  | PropIff (prop1, prop2, _)
  | PropTLU (prop1, prop2, _)
  | PropTLR (prop1, prop2, _) ->
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
  | SpecLTL (specname, prop, justicelist, compassionlist, loc) ->
     checkProp symtab prop;
     List.iter (fun (a, b) -> checkPureProp symtab a; checkPureProp symtab b) justicelist;
     List.iter (fun (a, b) -> checkPureProp symtab a; checkPureProp symtab b) compassionlist;
     ignore (ST.bindGlobal symtab (specname, None) (LTLSpecName (specname, prop, 
                                                                 justicelist,
                                                                 compassionlist)))
  | SpecDefine (ident, prop, loc) ->
     checkProp symtab prop;
     let name, _ = ident in
     ignore (ST.bindGlobal symtab ident (DeclaredExpr (name, prop)))

let checkProg symtab prog =
  let symtypedecls, msgdecls, autodecls, initstatedecls, specs = prog in
  List.iter (checkSymTypeDecl symtab) symtypedecls;
  checkGlobalMsgDeclBlock symtab msgdecls;
  List.iter
    (fun aut -> 
     let autname, autparamlist, autPropOpt, autscope = checkDecl symtab checkAutDef aut in
     let name, _ = autname in
     match aut with
     | DeclSimple (CompleteAutomaton _, _)
     | DeclQuantified (CompleteAutomaton _, _, _, _) ->
        ST.bind symtab autname 
                (AutomatonName ((name, CompleteAutType, autscope), 
                                cvtParamTypeListForSymtab autparamlist, autPropOpt))
     | DeclSimple (IncompleteAutomaton _, _)
     | DeclQuantified (IncompleteAutomaton _, _, _, _) ->
        ST.bind symtab autname 
                (AutomatonName ((name, PartialAutType, autscope), 
                                cvtParamTypeListForSymtab autparamlist, autPropOpt))
     | DeclSimple (ChannelAutomaton _, _)
     | DeclQuantified (ChannelAutomaton _, _, _, _) ->
        ST.bind symtab autname 
                (AutomatonName ((name, ChannelAutType, autscope), 
                                cvtParamTypeListForSymtab autparamlist, autPropOpt)))
    autodecls;
  initStateDeclChecker symtab initstatedecls;
  List.iter (checkSpec symtab) specs
            
(* low level checking routines *)
let checkLLProg prog =
  let (msglist, autlist, initstates, specs, _) = prog in
  (* check that the system is closed *)
  let allInMsgs = 
    List.fold_left 
      (fun acc aut ->
       match aut with
       | LLCompleteAutomaton (_, _, inmsgs, _, _, _, _, _, _)
       | LLIncompleteAutomaton (_, _, inmsgs, _, _, _) ->
          List.fold_left
            (fun acc2 inmsg -> LLDesigSet.add inmsg acc2) acc inmsgs) 
      LLDesigSet.empty autlist;
  in
  let allOutMsgs = 
    List.fold_left
      (fun acc aut ->
       match aut with
       | LLCompleteAutomaton (_, _, _, outmsgs, _, _, _, _, _)
       | LLIncompleteAutomaton (_, _, _, outmsgs, _, _) ->
          List.fold_left
            (fun acc2 outmsg -> LLDesigSet.add outmsg acc2) acc outmsgs)
      LLDesigSet.empty autlist
  in
  let allMsgs = 
    List.fold_left 
      (fun acc msg -> LLDesigSet.add msg acc) 
      LLDesigSet.empty msglist 
  in
  if (not ((LLDesigSet.subset allInMsgs allOutMsgs) && 
             (LLDesigSet.subset allOutMsgs allInMsgs))) then
    raise (SemanticError ("The system is not closed", None))
  else
    ();
  if (not ((LLDesigSet.subset allInMsgs allMsgs) &&
             (LLDesigSet.subset allMsgs allInMsgs))) then
    raise (SemanticError ("Some global messages are not used by any automaton", None))
  else
    ();

  (* check that every message has exactly one sender *)
  LLDesigSet.iter 
    (fun msg ->
     let senders = 
       List.filter 
         (fun aut -> 
          let outmsgs = 
            (match aut with
             | LLCompleteAutomaton (_, _, _, outmsgs, _, _, _, _, _) -> outmsgs
             | LLIncompleteAutomaton (_, _, _, outmsgs, _, _) -> outmsgs) 
          in
          List.mem msg outmsgs) autlist
     in
     if (List.length senders) <> 1 then
       raise (SemanticError ("Error. Message \"" ^ (lldesigToString msg) ^ "\" does not have exactly one " ^ 
                               "Sender.", None))
     else
       ()) allMsgs;

  let checkState state transitions inmsgs outmsgs =
    let inmsgs = List.fold_left (fun acc elem -> LLDesigSet.add elem acc) LLDesigSet.empty inmsgs in
    let outmsgs = List.fold_left (fun acc elem -> LLDesigSet.add elem acc) LLDesigSet.empty outmsgs 
    in
    let transmsgset = 
      List.fold_left
        (fun acc transition ->
         let start, msg = 
           (match transition with
            | TComplete (start, msg, _) -> start, msg
            | TParametrizedDest (start, msg, _) -> start, msg
            | _ -> assert false) in
         if start = state then
           LLDesigSet.add msg acc
         else
           acc) LLDesigSet.empty transitions
    in
    if ((not (LLDesigSet.is_empty (LLDesigSet.inter transmsgset inmsgs))) &&
          (not (LLDesigSet.is_empty (LLDesigSet.inter transmsgset outmsgs)))) then
      raise (SemanticError ("State \"" ^ (lldesigToString state) ^ "\" is both output and input",
                            None))
    else
      ()
  in

  (* input and output states must be disjoint for each automaton *)
  List.iter 
    (fun aut ->
     match aut with
     | LLIncompleteAutomaton (_, states, inmsgs, outmsgs, transitions, _) ->
        List.iter (fun state -> checkState state transitions inmsgs outmsgs) states
     | _ -> ()) autlist;

  (* Incomplete automata must be deterministic *)
  (* We allow multiple start states, but the transitions must be deterministic *)
  (* This means: An output state can have only ONE output message from it and not other edges *)
  (* An input state might not have two edges on the same message *)

  List.iter
    (fun aut ->
     match aut with
     | LLCompleteAutomaton _ -> ()
     | LLIncompleteAutomaton (name, states, inmsgs, outmsgs, transitions, _) ->
        (* build a mapping from each state to the messages it transitions on *)
        let transmap = 
          List.fold_left 
            (fun acc trans ->
             let start, msg =
               (match trans with
                | TComplete (start, msg, _) -> start, msg
                | TParametrizedDest (start, msg, _) -> start, msg
                | _ -> assert false) 
             in
             let curset = try LLDesigMap.find start acc with Not_found -> LLDesigSet.empty in
             if LLDesigSet.mem msg curset then
               raise (SemanticError ("Automaton \"" ^ (lldesigToString name) ^ "\" is not " ^ 
                                       "deterministic on state \"" ^ (lldesigToString start) ^ 
                                         "\" on msg \"" ^ (lldesigToString msg) ^ "\"",
                                     None))
             else
               LLDesigMap.add start (LLDesigSet.add msg curset) acc)
            LLDesigMap.empty transitions
        in
        (* check that if a state is an output state, it has only one outgoing transition *)
        LLDesigMap.iter 
          (fun state msgset ->
           LLDesigSet.iter 
             (fun msg -> 
              if ((List.mem msg outmsgs) && ((LLDesigSet.cardinal msgset) > 1)) then
                raise (SemanticError ("Automaton \"" ^ (lldesigToString name) ^ "\" has " ^ 
                                        "more than one transition on output state \"" ^ 
                                          (lldesigToString state) ^ "\"", None))
              else
                ()) msgset) transmap) autlist
