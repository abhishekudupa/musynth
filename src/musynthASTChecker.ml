(* semantic checks on AST *)

open MusynthAST
open MusynthTypes
module ST = MusynthSymTab

let rec resolveSymType symtab st =
  match st with
  | SymTypeNamed stident ->
      let entry = ST.lookupOrFail symtab stident in
      (match entry with
      | TypeEntry ntype -> resolveSymType symtab ntype
      | _ -> 
          let name, loc = stident in
          raise (SemanticError ("\"" ^ name ^ "\" does not name a type", loc)))
  | SymTypeAnon idlist ->
      st
      
let checkSymTypeDecl symtab stdecl =
  let ident, symtype = stdecl in
  let resst = resolveSymType symtab symtype in
  ST.bind symtab ident (TypeEntry resst)

let rec destructDesigDecl desig = 
  match desig with
  | SimpleDesignator ident -> (ident, [])
  | FieldDesignator (ndesig, ident) -> 
      let name, loc = ident in
      raise (SemanticError ("Declarations cannot contain index expressions", loc))
  | IndexDesignator (ndesig, indexid) ->
      let ident, paramlist = destructDesigDecl ndesig in
      (ident, paramlist @ [ indexid ])

let getTypeListForIdent symtab ident =
  let entry = ST.lookupOrFail symtab ident in
  match entry with
  | VarEntry _ -> []
  | StateEntry (Some lst) -> lst
  | MsgEntry (_, Some lst) -> lst
  | AutomatonEntry (_, Some lst, _) -> lst
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
  
let checkDesigRVal symtab rval =
  let rec checkRVal symtab rval paramlist =
    match rval with
    | SimpleDesignator ident ->
        let exptypelist = getTypeListForIdent symtab ident in
        let acttypeparamlist =
          List.map 
            (fun param -> 
              let typ = ST.lookupVar symtab param in (typ, param))
            paramlist 
        in
        checkTypeLists exptypelist acttypeparamlist;
        (* evaluate to the identifier *)
        ident

    | FieldDesignator (ndesig, ident) ->
        let name, loc = ident in
        (* recurse on the nested designator *)
        let nident = checkRVal symtab rval [] in
        let entry = ST.lookupOrFail symtab nident in
        begin
          match entry with
          | AutomatonEntry (_, _, scope) ->
              ST.pushScope symtab scope;
              let exptypelist = getTypeListForIdent symtab ident in
              let acttypeparamlist = 
                List.map
                  (fun param -> (ST.lookupVar symtab param, param)) paramlist 
              in
              checkTypeLists exptypelist acttypeparamlist;
              ignore (ST.pop symtab);
              ident
          | _ -> raise (SemanticError ("Invalid field access operation", loc))
        end
          
    | IndexDesignator (ndesig, idxident) ->
        checkRVal symtab ndesig (idxident :: paramlist)
  in
  ignore (checkRVal symtab rval [])

let rec checkPureProp symtab prop = 
  match prop with
  | PropTrue
  | PropFalse -> ()
  | PropEquals (desig1, desig2)
  | PropNEquals (desig1, desig2) -> 
      checkDesigRVal symtab desig1;
      checkDesigRVal symtab desig2
  | PropNot prop1 -> checkPureProp symtab prop1
  | PropAnd (prop1, prop2)
  | PropOr (prop1, prop2)
  | PropImplies (prop1, prop2)
  | PropIff (prop1, prop2) ->
      checkPureProp symtab prop1;
      checkPureProp symtab prop2
  | PropForall (idlist, typ, prop1)
  | PropExists (idlist, typ, prop1) ->
      let acttype = resolveSymType symtab typ in
      ST.push symtab;
      List.iter (fun ident -> ST.bind symtab ident (VarEntry acttype)) idlist;
      checkPureProp symtab prop1;
      ignore (ST.pop symtab)
  | _ -> raise (SemanticError ("Expected pure propositional formula, but got:\n" ^ 
                               (astToString pProp prop), None))

(* checker for declarations which involve a designator *)
let checkDesigDecl symtab desig =
  let ident, paramlist = destructDesigDecl desig in
  (* check that the paramlist have all been declared *)
  let typelist = List.map (ST.lookupVar symtab) paramlist in
  if typelist = [] then
    (ident, None)
  else
    (ident, Some typelist)

let checkTransDecl symtab trans =
  let sstate, msg, fstate = trans in
  checkDesigRVal symtab sstate;
  checkDesigRVal symtab msg;
  checkDesigRVal symtab fstate

let checkDecl symtab declParamChecker decl =
  match decl with 
  | DeclSimple declParam -> 
      declParamChecker symtab declParam
  | DeclQuantified (declParam, qMap, propOpt) ->
      ST.push symtab;
      IdentMap.iter
        (fun ident typ ->
          ST.bind symtab ident (TypeEntry (resolveSymType symtab typ))) qMap;
      (match propOpt with
      | Some prop -> checkPureProp symtab prop
      | None -> ());
      let res = declParamChecker symtab declParam in
      ignore (ST.pop symtab);
      res

let checkStateDecl symtab statedecl =
  let ident, typelistopt = checkDecl symtab checkDesigDecl statedecl in
  ST.bind symtab ident (StateEntry typelistopt)

let checkMsgDecl symtab msgtype msgdecl =
  let ident, typelistopt = checkDecl symtab checkDesigDecl msgdecl in
  ST.bind symtab ident (MsgEntry (msgtype, typelistopt))

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
let checkAutDef symtab autdef =
  match autdef with
  | CompleteAutomaton (autdesig, sblock, inblock, outblock, transblock) ->
      ST.push symtab;
      let _, _ = checkDesigDecl symtab autdesig in
      checkStateDeclBlock symtab sblock true;
      List.iter (checkMsgDecl symtab InputMsg) inblock;
      List.iter (checkMsgDecl symtab OutputMsg) outblock;
      List.iter (checkDecl symtab checkTransDecl) transblock;

  | _ -> ()
      
      
