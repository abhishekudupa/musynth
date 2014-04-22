open Format
open MusynthTypes

(* pretty printing routines for ASTs *)
(* they follow the grammar really *)

let pLoc fmt loc =
  let locstr = locToString loc in
  fprintf fmt "%s" locstr;
  pp_print_flush fmt ()

let pLocOpt fmt loc =
  match loc with
  | Some locn -> pLoc fmt locn
  | None -> ()

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
  | SymTypeNamed (ident, _) -> pIdentifier fmt ident
  | SymTypeAnon (identlist, _) ->
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
  | IndexDesignator (nestedDesig, ident, _) -> 
     pDesignator fmt nestedDesig;
     fprintf fmt "[%a]" pIdentifier ident
  | FieldDesignator (nestedDesig, ident, _) ->
     pDesignator fmt nestedDesig;
     fprintf fmt ".%a" pIdentifier ident


let rec pProp fmt prop =
  match prop with
  | PropTrue _ -> fprintf fmt "true"
  | PropFalse _ -> fprintf fmt "false"
  | PropDefine (ident) -> fprintf fmt "%a" pIdentifier ident
  | PropEquals (desig1, desig2, _) ->
     fprintf fmt "@[<b 3>(= %a@ %a)@]" pDesignator desig1 pDesignator desig2
  | PropNEquals (desig1, desig2, _) ->
     fprintf fmt "@[<b 4>(!= %a@ %a)@]" pDesignator desig1 pDesignator desig2
  | PropNot (prop1, _) ->
     fprintf fmt "@[<b 5>(not %a)@]" pProp prop1
  | PropAnd (prop1, prop2, _) ->
     fprintf fmt "@[<b 5>(and %a@ %a)@]" pProp prop1 pProp prop2
  | PropOr (prop1, prop2, _) ->
     fprintf fmt "@[<b 4>(or %a@ %a)@]" pProp prop1 pProp prop2
  | PropImplies (prop1, prop2, _) ->
     fprintf fmt "@[<b 9>(implies %a@ %a)@]" pProp prop1 pProp prop2
  | PropIff (prop1, prop2, _) ->
     fprintf fmt "@[<b 5>(iff %a@ %a)@]" pProp prop1 pProp prop2
  | PropForall (identlist, symtype, prop1, _) ->
     fprintf fmt "@[<b 1>(forall %a in %a@ %a)@]" 
             (pList "" false false pIdentifier) identlist
             pSymType symtype pProp prop1
  | PropExists (identlist, symtype, prop1, _) ->
     fprintf fmt "@[<b 1>(exists %a in %a@ %a)@]" 
             (pList "" false false pIdentifier) identlist 
             pSymType symtype pProp prop1
  | PropTLG (prop1, _) ->
     fprintf fmt "@[<b 4>(G %a)@]" pProp prop1
  | PropTLF (prop1, _) ->
     fprintf fmt "@[<b 4>(F %a)@]" pProp prop1
  | PropTLX (prop1, _) ->
     fprintf fmt "@[<b 4>(X %a)@]" pProp prop1
  | PropTLU (prop1, prop2, _) ->
     fprintf fmt "@[<b 4>(U %a@ %a)@]" pProp prop1 pProp prop2
  | PropTLR (prop1, prop2, _) ->
     fprintf fmt "@[<b 4>(R %a@ %a)@]" pProp prop1 pProp prop2

let rec pPropOpt fmt optProp =
  match optProp with
  | Some prop -> pProp fmt prop
  | None -> ()

let pDecl declPrefixPrinter declSuffixPrinter fmt decl =
  match decl with
  | DeclSimple (declParam, _) -> 
     declPrefixPrinter fmt declParam;
     declSuffixPrinter fmt declParam
  | DeclQuantified (declParam, qMap, optProp, _) ->
     declPrefixPrinter fmt declParam;
     fprintf fmt "@[<v 0>";
     let qList = IdentMap.bindings qMap in
     pList "" true false
           (fun fmtloc (ident, typ) -> 
            fprintf fmtloc " foreach %a in %a" 
                    pIdentifier ident pSymType typ) fmt qList;
     (match optProp with
      | Some prop -> fprintf fmt " %a" pProp prop
      | None -> ());
     fprintf fmt "@]";
     declSuffixPrinter fmt declParam

let noopPrinter fmt thing =
  ()

let pMsgDecl fmt decl =
  pDecl pDesignator noopPrinter fmt decl
        
let pMsgDeclBlock name fmt block =
  musMakeIndentedBox fmt pp_print_string (name ^ " {")
                     (pList "," true false pMsgDecl) block fprintf "};"

let pMessagesDeclBlock fmt block =
  musMakeIndentedBox fmt pp_print_string ("messages {")
                     (pList "," true false pMsgDecl) block fprintf "}"

let pStateAnnot fmt annot =
  match annot with
  | AnnotNone -> ()
  | AnnotComplete _ -> fprintf fmt " : complete"
  | AnnotIncomplete _ -> fprintf fmt " : incomplete"
  | AnnotIncompleteEventList (msglist, _) -> 
     fprintf fmt " : incomplete (@[<v 0>%a@])" 
             (pList "," true false pMsgDecl) msglist
  | AnnotIncompleteNum (num, _) ->
     fprintf fmt " : incomplete %d" num
  | AnnotIncompleteNumEventList (num, msglist, _) ->
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
  fprintf fmt "%a" pProp decl


let pInitStateDeclBlock fmt block =
  musMakeIndentedBox fmt 
                     pp_print_string "init {"
                     (pList "," true true pInitStateConstraint) block
                     pp_print_string "}"

let pChanProp fmt chanprop =
  let ord, loss, dup, block, cap = chanprop in
  (match ord with
   | ChanUnordered _ ->
      fprintf fmt "ordered"
   | _ ->
      fprintf fmt "unordered");
  fprintf fmt ", ";
  (match loss with 
   | ChanLossy _ ->
      fprintf fmt "lossy"
   | _ ->
      fprintf fmt "lossless");
  fprintf fmt ", ";
  (match dup with
   | ChanDuplicating _ ->
      fprintf fmt "duplicating"
   | _ ->
      fprintf fmt "nonduplicating");
  fprintf fmt ", ";
  (match block with
   | ChanNonBlocking _ -> fprintf fmt "nonblocking"
   | ChanBlocking _ -> fprintf fmt "blocking");
  fprintf fmt ";@,";
  fprintf fmt "capacity = %d;@," cap

let pFairness fmt f =
  match f with
  | FairnessTypeNone -> ()
  | FairnessTypeJustice _ -> fprintf fmt " justice"
  | FairnessTypeCompassion _ -> fprintf fmt " compassion"

let pLossFairness fmt f =
  match f with
  | LossFairnessNone -> ()
  | LossFairnessFinite _ -> fprintf fmt " finiteloss"

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
    | CompleteAutomaton (d, _, _, _, _, _, _) ->
       fprintf fmt "@[<v 0>automaton %a" pDesignator d
    | IncompleteAutomaton (d, _, _, _, _, _, _) ->
       fprintf fmt "@[<v 0>partialautomaton %a" pDesignator d
    | ChannelAutomaton (d, _, _, _, _, _) ->
       fprintf fmt "@[<v 0>channelautomaton %a" pDesignator d
  in
  let suffixPrinter fmt automaton =
    match automaton with
    | CompleteAutomaton (_, states, inblock, outblock, transblock, _, _)
    | IncompleteAutomaton (_, states, inblock, outblock, transblock, _) ->
       fprintf fmt "@,@[<v 4>{@,";
       pBlockCond (pStateDeclBlock "states") fmt states;
       pBlockCond (pMsgDeclBlock "inputs") fmt inblock;
       pBlockCond (pMsgDeclBlock "outputs") fmt outblock;
       pBlockCond ~br:false pTransDeclBlock fmt transblock;
       fprintf fmt "@]@,}@,@]@,"
    | ChannelAutomaton (_, chanprop, msgblock, _) ->
       fprintf fmt "@,@[<v 4>{@,";
       pChanProp fmt chanprop;
       fprintf fmt "@,@,";
       pMsgDeclBlock "messages" fmt msgblock;
       fprintf fmt "@]@,}@,@]@,"
  in
  pDecl prefixPrinter suffixPrinter fmt autdecl

let pSpec fmt spec =
  match spec with
  | SpecInvar (name, prop, _) -> 
     musMakeIndentedBox fmt pp_print_string ("invariant \"" ^ name ^ "\" {")
                        pProp prop pp_print_string "}"
  | SpecLTL (name, prop, justicelist, compassionlist, _) -> 
     musMakeIndentedBox fmt pp_print_string ("ltlspec \"" ^ name ^ "\" {")
                        (fun fmt (prop, jlist, clist) ->
                         pProp fmt prop;
                         if jlist = [] then
                           ()
                         else
                           musMakeIndentedBox fmt pp_print_string 
                                              " with justice ("
                                              (pList "," true false pProp) 
                                              jlist pp_print_string ")";
                        if clist = [] then
                          () 
                        else
                          musMakeIndentedBox fmt pp_print_string 
                                             " with compassion ("
                                             (pList "," true false 
                                                    (fun fmt (a, b) -> 
                                                     fprintf fmt "(%a, %a)" 
                                                             pProp a pProp b))
                                             clist pp_print_string ")")
                        (prop, justicelist, compassionlist)
                        pp_print_string "}"
  | SpecDefine (ident, prop, _) -> 
     fprintf fmt "define %a %a" pIdentifier ident pProp prop

let pProg fmt prog =
  let symtypes, msgdecls, automata, initblock, specs = prog in
  fprintf fmt "@[<v 0>";
  pSymTypeDeclBlock fmt symtypes;
  fprintf fmt "@,@,";
  pMessagesDeclBlock fmt msgdecls;
  fprintf fmt "@,@,";
  List.iter (pAutomatonDecl fmt) automata;
  pInitStateDeclBlock fmt initblock;
  fprintf fmt "@,@,";
  List.iter (fun spec -> pSpec fmt spec; fprintf fmt "@,@,") specs;
  fprintf fmt "@]"


(* printers for low-level representations *)
let rec pLLDesignator fmt desig =
  match desig with
  | LLSimpleDesignator name -> fprintf fmt "%s" name
  | LLIndexDesignator (ndesig, name) -> 
     fprintf fmt "%a[%s]" pLLDesignator ndesig name
  | LLFieldDesignator (ndesig, name) ->
     fprintf fmt "%a.%s" pLLDesignator ndesig name

let pLLIdent fmt ident =
  pLLDesignator fmt ident

let pLLVar fmt param =
  let name, opts = param in
  fprintf fmt "%a : {" pLLDesignator name;
  LLDesigSet.iter (fun desig -> fprintf fmt " %a" pLLDesignator desig) opts;
  fprintf fmt " }"

let pLLAnnot fmt annot = 
  match annot with
  | LLAnnotEventList (msglist) -> fprintf fmt " : (%a)" (pList ", " false false pLLIdent) msglist
  | LLAnnotNumEventList (num, msglist) -> fprintf fmt " : (%d of (%a))" num 
                                                  (pList ", " false false pLLIdent) msglist
  | LLAnnotNone -> ()

let pLLTrans fmt trans =
  match trans with
  | TComplete (start, msg, final) ->
     fprintf fmt "(%a, %a, %a)" pLLDesignator start 
             pLLDesignator msg 
             pLLDesignator final
  | TParametrizedDest (start, msg, param) ->
     fprintf fmt "(%a, %a, %a)" pLLDesignator start 
             pLLDesignator msg pLLVar param
  | TParametrizedMsgDest (start, msgparam, destparam) ->
     fprintf fmt "(%a, %a, %a)" pLLDesignator start
             pLLVar msgparam pLLVar destparam

let rec pLLProp fmt prop =
  match prop with
  | LLPropTrue -> fprintf fmt "true"
  | LLPropFalse -> fprintf fmt "false"
  | LLPropEquals (desig1, desig2) -> 
     fprintf fmt "@[<b 3>(= %a@ %a)@]" pLLDesignator desig1 pLLDesignator desig2
  | LLPropNot prop1 -> 
     fprintf fmt "@[<b 5>(not %a)@]" pLLProp prop1
  | LLPropAnd (prop1, prop2) ->
     fprintf fmt "@[<b 5>(and %a@ %a)@]" pLLProp prop1 pLLProp prop2
  | LLPropOr (prop1, prop2) ->
     fprintf fmt "@[<b 4>(or %a@ %a)@]" pLLProp prop1 pLLProp prop2
  | LLPropTLX prop1 ->
     fprintf fmt "@[<b 3>(X %a)@]" pLLProp prop1
  | LLPropTLU (prop1, prop2) ->
     fprintf fmt "@[<b 3>(U %a %a)@]" pLLProp prop1 pLLProp prop2

let pLLSpec fmt spec = 
  match spec with
  | LLSpecInvar (name, prop) -> 
     musMakeIndentedBox fmt pp_print_string ("invariant \"" ^ name ^ "\" {")
                        pLLProp prop pp_print_string "}"
  | LLSpecLTL (name, prop, justicelist, compassionlist) -> 
     musMakeIndentedBox fmt pp_print_string ("ltlspec \"" ^ name ^ "\" {")
                        (fun fmt (prop, jlist, clist) ->
                         pLLProp fmt prop;
                         if jlist = [] then
                           ()
                         else
                           musMakeIndentedBox fmt pp_print_string " with justice ("
                                              (pList "," true false pLLProp) 
                                              jlist pp_print_string ")";
                         if clist = [] then
                           () 
                         else
                           musMakeIndentedBox fmt pp_print_string " with compassion ("
                                              (pList "," true false 
                                                     (fun fmt (a, b) -> 
                                                      fprintf fmt "(%a, %a)" 
                                                              pLLProp a pLLProp b))
                                              clist pp_print_string ")")
                        (prop, justicelist, compassionlist)
                        pp_print_string "}"

let pLLAutomaton fmt aut =
  match aut with
  | LLCompleteAutomaton (name, states, inmsgs, outmsgs, transitions, _)
  | LLIncompleteAutomaton (name, states, inmsgs, outmsgs, transitions) ->
     fprintf fmt "@[<v 0>@[<v 4>%s %a {@," 
             (match aut with 
              | LLCompleteAutomaton _ -> "completeautomaton" 
              | _ -> "partialautomaton")
             pLLDesignator name;
     fprintf fmt "@[<v 4>states {@,%a@]@,}@," (pList "," true false pLLDesignator) states;
     fprintf fmt "@[<v 4>inputs {@,%a@]@,}@," (pList "," true false pLLDesignator) inmsgs;
     fprintf fmt "@[<v 4>outputs {@,%a@]@,}@," (pList "," true false pLLDesignator) outmsgs;
     fprintf fmt "@[<v 4>transitions {@,%a@]@,}@," (pList "," true false pLLTrans) transitions;
     fprintf fmt "@]@,}@,@]"

let pLLProg fmt prog = 
  let mdecls, auts, initstateconstraints, specs = prog in
  fprintf fmt "@[<v 0>@[<v 4>messages {@,%a@]@,}@,@]" (pList "," true false pLLDesignator) mdecls;
  List.iter (fun aut -> pLLAutomaton fmt aut) auts;
  fprintf fmt "@[<v 0>@[<v 4>initial constraints:@,%a@,@]@,@]" pLLProp initstateconstraints;
  List.iter (fun spec -> fprintf fmt "@[<v 0>@[<v 1>%a@]@,@,@]" pLLSpec spec) specs
