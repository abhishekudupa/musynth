open Format
open MusynthTypes

(* pretty printing routines for ASTs *)
(* they follow the grammar really *)

let pIdentifier fmt ident =
  let id, _ = ident in
  fprintf fmt "%s" id

let identToName ident =
  let id, _ = ident in 
  id

let astToString astprinter ast =
  let buf = Buffer.create 32 in
  let fmt = formatter_of_buffer buf in
  fprintf fmt "@[<v 0>";
  astprinter fmt ast;
  fprintf fmt "@]";
  pp_print_flush fmt ();
  Buffer.contents buf

let rec pList sep break endsep pfun fmt lst =
  match lst with
  | [] -> ()
  | [ head ] -> 
      fprintf fmt "%a%s" pfun head  (if endsep then sep else "")
  | head :: rest ->
      begin
        if break then
          fprintf fmt "%a%s@," pfun head sep
        else
          fprintf fmt "%a%s" pfun head sep;
        pList sep break endsep pfun fmt rest
      end

let pSymType fmt typ =
  match typ with
  | SymTypeNamed ident -> pIdentifier fmt ident
  | SymTypeAnon identlist ->
      fprintf fmt "{ %a }" (pList ", " false false pIdentifier) identlist

let pSymTypeDecl fmt decl =
  let ident, typ = decl in
  fprintf fmt "%a : %a;" pIdentifier ident pSymType typ

let musMakeIndentedBox fmt prefixPrinter prefixArg 
    bodyPrinter bodyArg suffixPrinter suffixArg =
  fprintf fmt "@[<v 0>@[<v 4>";
  fprintf fmt "%a" prefixPrinter prefixArg;
  fprintf fmt "@,";
  fprintf fmt "%a" bodyPrinter bodyArg;
  fprintf fmt "@]@,";
  fprintf fmt "%a" suffixPrinter suffixArg;
  fprintf fmt "@]"

let pSymTypeDeclBlock fmt block =
  musMakeIndentedBox fmt pp_print_string "symmetrictypes {" 
    (pList "" true false pSymTypeDecl) block 
    pp_print_string "}"

let rec pDesignator fmt desig =
  match desig with
  | SimpleDesignator ident -> pIdentifier fmt ident
  | IndexDesignator (nestedDesig, ident) -> 
      pDesignator fmt nestedDesig;
      fprintf fmt "[%a]" pIdentifier ident
  | FieldDesignator (nestedDesig, ident) ->
      pDesignator fmt nestedDesig;
      fprintf fmt ".%a" pIdentifier ident


let rec pProp fmt prop =
  match prop with
  | PropTrue -> fprintf fmt "true"
  | PropFalse -> fprintf fmt "false"
  | PropEquals (desig1, desig2) ->
      fprintf fmt "@[<b 3>(= %a@ %a)@]" pDesignator desig1 pDesignator desig2
  | PropNEquals (desig1, desig2) ->
      fprintf fmt "@[<b 4>(!= %a@ %a)@]" pDesignator desig1 pDesignator desig2
  | PropNot prop1 ->
      fprintf fmt "@[<b 5>(not@ %a)" pProp prop1
  | PropAnd (prop1, prop2) ->
      fprintf fmt "@[<b 5>(and@ %a@ %a)@]" pProp prop1 pProp prop2
  | PropOr (prop1, prop2) ->
      fprintf fmt "@[<b 4>(or@ %a@ %a)@]" pProp prop1 pProp prop2
  | PropImplies (prop1, prop2) ->
      fprintf fmt "@[<b 9>(implies@ %a@ %a)@]" pProp prop1 pProp prop2
  | PropIff (prop1, prop2) ->
      fprintf fmt "@[<b 5>(iff@ %a@ %a)@]" pProp prop1 pProp prop2
  | PropForall (identlist, symtype, prop1) ->
      fprintf fmt "@[<b 1>(forall %a in %a@ %a)@]" 
        (pList "" false false pIdentifier) identlist
        pSymType symtype pProp prop1
  | PropExists (identlist, symtype, prop1) ->
      fprintf fmt "@[<b 1>(exists %a in %a@ %a)@]" 
        (pList "" false false pIdentifier) identlist 
        pSymType symtype pProp prop1
  | PropCTLAG prop1 ->
      fprintf fmt "@[<b 4>(AG@ %a)@]" pProp prop1
  | PropCTLAF prop1 ->
      fprintf fmt "@[<b 4>(AF@ %a)@]" pProp prop1
  | PropCTLAX prop1 ->
      fprintf fmt "@[<b 4>(AX@ %a)@]" pProp prop1
  | PropCTLAU (prop1, prop2) ->
      fprintf fmt "@[<b 4>(AU@ %a@ %a)@]" pProp prop1 pProp prop2
  | PropCTLEG prop1 ->
      fprintf fmt "@[<b 4>(EG@ %a)@]" pProp prop1
  | PropCTLEF prop1 ->
      fprintf fmt "@[<b 4>(EF@ %a)@]" pProp prop1
  | PropCTLEX prop1 ->
      fprintf fmt "@[<b 4>(EX@ %a)@]" pProp prop1
  | PropCTLEU (prop1, prop2) ->
      fprintf fmt "@[<b 4>(EU@ %a@ %a)@]" pProp prop1 pProp prop2

let rec pPropOpt fmt optProp =
  match optProp with
  | Some prop -> pProp fmt prop
  | None -> ()

let pDecl declPrefixPrinter declSuffixPrinter fmt decl =
  match decl with
  | DeclSimple declParam -> 
      declPrefixPrinter fmt declParam;
      declSuffixPrinter fmt declParam
  | DeclQuantified (declParam, qMap, optProp) ->
      declPrefixPrinter fmt declParam;
      fprintf fmt "@[<v 0>";
      let qList = IdentMap.bindings qMap in
      pList "" true false
        (fun fmtloc (ident, typ) -> 
          fprintf fmtloc " foreach %a in %a" 
            pIdentifier ident pSymType typ) fmt qList;
      fprintf fmt "@]";
      declSuffixPrinter fmt declParam

let noopPrinter fmt thing =
  ()

let pMsgDecl fmt decl =
  pDecl pDesignator noopPrinter fmt decl

let pMsgDeclBlock name fmt block =
  musMakeIndentedBox fmt pp_print_string (name ^ " {")
    (pList "," true false pMsgDecl) block fprintf "};"

let pStateAnnot fmt annot =
  match annot with
  | AnnotNone -> ()
  | AnnotComplete -> fprintf fmt " : complete"
  | AnnotIncomplete -> fprintf fmt " : incomplete"
  | AnnotIncompleteEventList msglist -> 
      fprintf fmt " : incomplete (@[<v 0>%a@])" 
        (pList "," true false pMsgDecl) msglist
  | AnnotIncompleteNum num ->
      fprintf fmt " : incomplete %d" num
  | AnnotIncompleteNumEventList (num, msglist) ->
      fprintf fmt " : incomplete %d (@[<v 0>%a@])" num 
        (pList "," true false pMsgDecl) msglist

let pStateDecl fmt decl =
  let sdecl, annot = decl in
  pDecl pDesignator 
    (fun fmtloc declloc -> pStateAnnot fmtloc annot) fmt sdecl
  
let pStateDeclBlock prefix fmt block =
  musMakeIndentedBox fmt 
    pp_print_string (prefix ^ " : {")
    (pList "," true false pStateDecl) block
    fprintf "};"

let pTransDecl fmt decl =
  let prefixPrinter fmt (init, msg, final) =
    fprintf fmt "(%a, %a, %a" pDesignator init pDesignator msg pDesignator final
  in
  let suffixPrinter fmt decl =
    fprintf fmt ")"
  in
  pDecl prefixPrinter suffixPrinter fmt decl

let pTransDeclBlock fmt block =
  musMakeIndentedBox fmt 
    pp_print_string "transitions {"
    (pList "," true false pTransDecl) block
    fprintf "};"

let pInitStateConstraint fmt decl =
  let lhs, rhs = decl in
  fprintf fmt "%a = %a" pDesignator lhs pDesignator rhs

let pInitStateDecl fmt decl =
  let prefixPrinter fmt initstateconstr =
    pList "," true false pInitStateConstraint fmt initstateconstr
  in
  fprintf fmt "@[<v 4>";
  pDecl prefixPrinter noopPrinter fmt decl;
  fprintf fmt "@]"

let pInitStateDeclBlock fmt block =
  musMakeIndentedBox fmt 
    pp_print_string "init {"
    (pList ";" true true pInitStateDecl) block
    pp_print_string "}"

let pChanProp fmt chanprop =
  let ord, loss, dup, cap = chanprop in
  if ord = ChanUnordered then
    fprintf fmt "ordered"
  else
    fprintf fmt "unordered";
  fprintf fmt ", ";
  if loss = ChanLossy then
    fprintf fmt "lossy"
  else
    fprintf fmt "lossless";
  fprintf fmt ", ";
  if dup = ChanDuplicating then
    fprintf fmt "duplicating"
  else
    fprintf fmt "nonduplicating";
  fprintf fmt ";@,";
  fprintf fmt "capacity = %d;@," cap


let pAutomatonDecl fmt autdecl =
  let pBlockCond ?br:(b = true) pfun fmt block =
    if block = [] then
      ()
    else
      begin
        pfun fmt block;
        if b then fprintf fmt "@,@," else ()
      end
  in
  let prefixPrinter fmt automaton =
    match automaton with
    | CompleteAutomaton (d, _, _, _, _) ->
        fprintf fmt "@[<v 0>automaton %a" pDesignator d
    | IncompleteAutomaton (d, _, _, _, _) ->
        fprintf fmt "@[<v 0>partialautomaton %a" pDesignator d
    | ChannelAutomaton (d, _, _) ->
        fprintf fmt "@[<v 0>channelautomaton %a" pDesignator d
  in
  let suffixPrinter fmt automaton =
    match automaton with
    | CompleteAutomaton (_, states, inblock, outblock, transblock)
    | IncompleteAutomaton (_, states, inblock, outblock, transblock) ->
        fprintf fmt "@,@[<v 4>{@,";
        pBlockCond (pStateDeclBlock "states") fmt states;
        pBlockCond (pMsgDeclBlock "inputs") fmt inblock;
        pBlockCond (pMsgDeclBlock "outputs") fmt outblock;
        pBlockCond ~br:false pTransDeclBlock fmt transblock;
        fprintf fmt "@]@,}@,@]@,"
    | ChannelAutomaton (_, chanprop, msgblock) ->
        fprintf fmt "@,@[<v 4>{@,";
        pChanProp fmt chanprop;
        fprintf fmt "@,@,";
        pMsgDeclBlock "messages" fmt msgblock;
        fprintf fmt "@]@,}@,@]@,"
  in
  pDecl prefixPrinter suffixPrinter fmt autdecl

let pSpec fmt spec =
  let prefix, prop = 
    (match spec with
    | SpecInvar (name, prop) -> ("invariant \"" ^ name ^ "\" {", prop)
    | SpecCTL (name, prop) -> ("ctlspec \"" ^ name ^ "\" {", prop))
  in
  musMakeIndentedBox fmt pp_print_string prefix
    pProp prop pp_print_string "}"

let pProg fmt prog =
  let symtypes, automata, initblock, specs = prog in
  fprintf fmt "@[<v 0>";
  pSymTypeDeclBlock fmt symtypes;
  fprintf fmt "@,@,";
  List.iter (pAutomatonDecl fmt) automata;
  pInitStateDeclBlock fmt initblock;
  fprintf fmt "@,@,";
  List.iter (fun spec -> pSpec fmt spec; fprintf fmt "@,@,") specs;
  fprintf fmt "@]"
