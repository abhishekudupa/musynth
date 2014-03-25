open MusynthTypes
open Format

module Util = MusynthUtils
module AST = MusynthAST
module ST = MusynthSymTab
module CK = MusynthASTChecker

(* pretty printing for lowered AST *)
let pLLIdent fmt ident =
  let name, idlist = ident in
  fprintf fmt "%s[%a]" name (AST.pList "][" false false pp_print_string) idlist

let pLLAnnot fmt annot =
  match annot with
  | LLAnnotComplete -> ()
  | LLAnnotIncomplete -> fprintf fmt " : incomplete"
  | LLAnnotIncompleteNum num -> fprintf fmt " : incomplete %d" num
  | LLAnnotIncompleteEventList el -> 
      fprintf fmt " : incomplete (%a)" (AST.pList ", " true false pLLIdent) el
  | LLAnnotIncompleteNumEventList (num, el) ->
      fprintf fmt " : incomplete %d (%a)" num (AST.pList ", " true false pLLIdent) el

let pLLState fmt state =
  let state, annot = state in
  fprintf fmt "%a %a" pLLIdent state pLLAnnot annot

let pLLCompleteTrans fmt trans =
  let sstate, msg, fstate = trans in
  fprintf fmt "(%a, %a, %a)" pLLIdent sstate pLLIdent msg pLLIdent fstate

let pLLParametricTrans fmt trans =
  let sstate, msg, fstateset = trans in
  fprintf fmt "(%a, %a, { %a })" pLLIdent sstate pLLIdent msg
    (AST.pList ", " false false pLLIdent) (LLIdentSet.elements fstateset)

let pLLAut fmt aut =
  match aut with
  | LLCompleteAutomaton (name, states, inmsgs, outmsgs, trans) ->
      fprintf fmt "@[<v 4>automaton %a {@," pLLIdent name;
      fprintf fmt "@[<v 4>states {@, %a @]@,};@," (AST.pList ", " true false pLLIdent) states;
      if (inmsgs <> []) then
        fprintf fmt "@[<v 4>inputs {@, %a @]};@," (AST.pList ", " true false pLLIdent) inmsgs
      else 
        ();
      if (outmsgs <> []) then
        fprintf fmt "@[<v 4>outputs {@, %a @]};@," (AST.pList ", " true false pLLIdent) outmsgs
      else
        ();
      fprintf fmt "@[<v 4>transitions {@, %a @]};@," (AST.pList ", " true false pLLCompleteTrans) trans;
      fprintf fmt "@]}@,"
  | LLIncompleteAutomaton (name, states, inmsgs, outmsgs, trans) ->
      fprintf fmt "@[<v 4>partialautomaton %a {@," pLLIdent name;
      fprintf fmt "@[<v 4>states {@, %a @]@,};@," (AST.pList ", " true false pLLIdent) states;
      if (inmsgs <> []) then
        fprintf fmt "@[<v 4>inputs {@, %a @]};@," (AST.pList ", " true false pLLIdent) inmsgs
      else 
        ();
      if (outmsgs <> []) then
        fprintf fmt "@[<v 4>outputs {@, %a @]};@," (AST.pList ", " true false pLLIdent) outmsgs
      else
        ();
      fprintf fmt "@[<v 4>transitions {@, %a @]};@," (AST.pList ", " true false pLLParametricTrans) trans;
      fprintf fmt "@]}@,"


(* routines to lower the AST *)
(* also adds parameters to incomplete automata *)

let evalProp propOpt evalMap =
  let eval id =
    let (ident, _), _ = CK.destructDesigDecl id in
    StringMap.find ident evalMap
  in
  let rec evalPropRec prop =
    match prop with
    | PropTrue _ -> true
    | PropFalse _ -> false
    | PropEquals (str1, str2, _) ->
        (eval str1) = (eval str2)
    | PropNEquals (str1, str2, _) ->
        (eval str1) <> (eval str2)
    | PropNot (prop1, _) ->
        (evalPropRec prop1) <> true
    | PropAnd (prop1, prop2, _) ->
        (evalPropRec prop1) && (evalPropRec prop2)
    | PropOr (prop1, prop2, _) ->
        ((evalPropRec prop1) || (evalPropRec prop2))
    | PropImplies (prop1, prop2, _) ->
        ((not (evalPropRec prop1)) || (evalPropRec prop2))
    | PropIff (prop1, prop2, _) ->
        (evalPropRec prop1) = (evalPropRec prop2)
    | _ -> assert false
  in
  match propOpt with
  | None -> true
  | Some prop -> evalPropRec prop

let filterMaps maps propOpt =
  List.filter (evalProp propOpt) maps

let getMaps symtab qMap propOpt =
  let paramlist = List.map (fun ((id, loc), typ) -> id) (IdentMap.bindings qMap) in
  match paramlist with
  | [] -> []
  | _ ->
      begin
        let rtypelist = 
          List.map 
            (fun (id, typ) -> CK.resolveSymType symtab typ) 
            (IdentMap.bindings qMap) 
        in
        let constLists =
          List.map
            (fun st -> 
              match st with
              | SymTypeAnon (idlist, _) -> List.map (fun (id, loc) -> id) idlist
              | _ -> assert false) 
            rtypelist 
        in
        let constCP = Util.crossProduct constLists in
        let evalMaps = 
          List.map 
            (fun cp -> 
              List.fold_left2
                (fun map name valu ->
                  StringMap.add name valu map) StringMap.empty cp paramlist) constCP 
        in
        filterMaps evalMaps propOpt
      end

let mapArgs name maps paramlist =
  let arglists = 
    List.map 
      (fun map -> 
        List.map 
          (fun param -> 
            let id, _ = param in
            StringMap.find id map) paramlist) 
      maps
  in
  List.map (fun arglist -> (name, arglist)) arglists  

let lowerDesigDecl symtab desig qMap propOpt =
  let ident, paramlist = CK.destructDesigDecl desig in
  let name, _ = ident in
  let maps = getMaps symtab qMap propOpt in
  if maps = [] then
    [ (name, []) ]
  else
    mapArgs name maps paramlist

let lowerCompleteTransDecl symtab trans qMap propOpt =
  let sstate, msg, fstate = trans in
  let sident, sparamlist = CK.destructDesigDecl sstate in
  let sname, _ = sident in
  let msgident, msgparamlist = CK.destructDesigDecl msg in
  let msgname, _ = msgident in
  let fident, fparamlist = CK.destructDesigDecl fstate in
  let fname, _ = fident in
  match sparamlist, msgparamlist, fparamlist with
  | [], [], [] -> [ (sname, []), (msgname, []), (fname, []) ]
  | _ -> 
      begin
        let maps = getMaps symtab qMap propOpt in
        if maps = [] then
          [ (sname, []), (msgname, []), (fname, []) ]
        else
          let sstatelist = mapArgs sname maps sparamlist in
          let msglist = mapArgs msgname maps msgparamlist in
          let fstatelist = mapArgs fname maps fparamlist in
          let clist = List.combine sstatelist msglist in
          let clist = List.combine clist fstatelist in
          List.map (fun ((a, b), c) -> (a, b, c)) clist
      end

let lowerAutomatonDecl symtab aut qMap propOpt =
  match aut with 
  | 
          
let lowerDecl symtab declParamLowerer decl =
  match decl with
  | DeclSimple (declparam, _) -> declParamLowerer symtab declparam IdentMap.empty None
  | DeclQuantified (declparam, qMap, propOpt, _) -> declParamLowerer symtab declparam qMap propOpt
  
let lowerStateAnnotation symtab annot =
  match annot with
  | AnnotNone
  | AnnotComplete _ -> LLAnnotComplete
  | AnnotIncomplete _ -> LLAnnotIncomplete
  | AnnotIncompleteNum (n, _) -> LLAnnotIncompleteNum n
  | AnnotIncompleteEventList (el, _) -> 
      LLAnnotIncompleteEventList (List.concat (List.map (lowerDecl symtab lowerDesigDecl) el))
  | AnnotIncompleteNumEventList (n, el, _) ->
      LLAnnotIncompleteNumEventList (n, List.concat (List.map (lowerDecl symtab lowerDesigDecl) el))

let lowerStateDecl symtab sdecl =
  let decl, annot = sdecl in
  let ldecls = lowerDecl symtab lowerDesigDecl decl in
  let lannot = lowerStateAnnotation symtab annot in
  List.map (fun ldecl -> (ldecl, lannot)) ldecls
  
let lowerStateDeclBlock symtab block = 
  List.concat (List.map (lowerStateDecl symtab) block)
    
let lowerMsgDecl symtab decl =
  lowerDecl symtab lowerDesigDecl decl

let lowerMsgDeclBlock symtab block =
  List.concat (List.map (lowerMsgDecl symtab) block)

let lowerCompleteTransDeclBlock symtab block = 
  List.concat (List.map (lowerDecl symtab lowerCompleteTransDecl) block)

