(* types and functions for the parse tree *)
open Format
  
(* file name * start line num * start col * end line num * end col *)
type sourcelocation = int * int * int * int
  
(* Filename * Line Num * Col num *)
exception ParseError of string * sourcelocation
exception SemanticError of string * sourcelocation
    
module StringMap = Map.Make
    (struct 
      type t = string 
      let compare = Pervasives.compare 
    end)
  
type identifierT = string * sourcelocation option

module IdentMap = Map.Make 
    (struct 
      type t = identifierT 
      let compare = fun ident1 ident2 -> 
        let id1, loc1 = ident1 in
        let id2, loc2 = ident2 in
        Pervasives.compare id1 id2
    end)
    
type musSymTypeT = 
  | SymTypeNamed of identifierT
  | SymTypeAnon of identifierT list
        
type musSymTypeDeclT = identifierT * musSymTypeT
  
type musSymTypeDeclBlockT = musSymTypeDeclT list
  
type musDesignatorT =
  | SimpleDesignator of identifierT
  | IndexDesignator of musDesignatorT * identifierT
  | FieldDesignator of musDesignatorT * identifierT
        
(* Only CTL at present *)
type musPropT =
  | PropTrue
  | PropFalse
  | PropEquals of (musDesignatorT * musDesignatorT)
  | PropNEquals of (musDesignatorT * musDesignatorT)
  | PropNot of musPropT
  | PropAnd of musPropT * musPropT
  | PropOr of musPropT * musPropT
  | PropImplies of musPropT * musPropT
  | PropIff of musPropT * musPropT
  | PropForall of identifierT list * musSymTypeT * musPropT
  | PropExists of identifierT list * musSymTypeT * musPropT
  | PropCTLAG of musPropT
  | PropCTLAF of musPropT
  | PropCTLAX of musPropT
  | PropCTLEG of musPropT
  | PropCTLEF of musPropT
  | PropCTLEX of musPropT
  | PropCTLAU of musPropT * musPropT
  | PropCTLEU of musPropT * musPropT
    
type 'a musDeclType = 
  | DeclSimple of 'a
  | DeclQuantified of 'a * (musSymTypeT IdentMap.t) * musPropT option

type musMsgDeclT = musDesignatorT musDeclType

type musMsgDeclBlockT = musMsgDeclT list

type musStateAnnotationT = 
  | AnnotNone
  | AnnotComplete
  | AnnotIncomplete
  | AnnotIncompleteEventList of musMsgDeclT list
  | AnnotIncompleteNum of int
  | AnnotIncompleteNumEventList of int * musMsgDeclT list

type musStateDeclT = musDesignatorT musDeclType * musStateAnnotationT

type musStateDeclBlockT = musStateDeclT list

type musTransDeclT = (musDesignatorT * musDesignatorT * musDesignatorT) musDeclType

type musTransDeclBlockT = musTransDeclT list

type musChanDupT = 
  | ChanDuplicating
  | ChanNonDuplicating

type musChanOrdT = 
  | ChanOrdered
  | ChanUnordered

type musChanLossT = 
  | ChanLossy
  | ChanLossless

type musChanPropT = musChanOrdT * musChanLossT * musChanDupT * int

type musAutomatonDeclType = 
  | CompleteAutomaton of musDesignatorT * musStateDeclBlockT * musStateDeclBlockT * 
        musMsgDeclBlockT * musMsgDeclBlockT * musTransDeclBlockT
  | IncompleteAutomaton of musDesignatorT * musStateDeclBlockT * musStateDeclBlockT * 
        musMsgDeclBlockT * musMsgDeclBlockT * musTransDeclBlockT
  | ChannelAutomaton of musDesignatorT * musChanPropT * musMsgDeclBlockT

type musAutomatonDeclT = musAutomatonDeclType musDeclType

type musSpecT = 
  | SpecInvar of string * musPropT
  | SpecCTL of string * musPropT

type musProgT = musSymTypeDeclBlockT * musAutomatonDeclT list * musSpecT list

(* pretty printing routines *)
(* they follow the grammar really *)

let pIdentifier fmt ident =
  let id, _ = ident in
  fprintf fmt "%s" id

let rec pList sep break pfun fmt lst =
  match lst with
  | [] -> ()
  | [ head ] -> fprintf fmt "%a" pfun head
  | head :: rest ->
      begin
        if break then
          fprintf fmt "%a%s@," pfun head sep
        else
          fprintf fmt "%a%s" pfun head sep;
        pList sep break pfun fmt rest
      end

let pSymType fmt typ =
  match typ with
  | SymTypeNamed ident -> pIdentifier fmt ident
  | SymTypeAnon identlist ->
      fprintf fmt "{ %a }" (pList ", " false pIdentifier) identlist

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
    (pList "" true pSymTypeDecl) block 
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
        (pList "" false pIdentifier) identlist
        pSymType symtype pProp prop1
  | PropExists (identlist, symtype, prop1) ->
      fprintf fmt "@[<b 1>(exists %a in %a@ %a)@]" 
        (pList "" false pIdentifier) identlist 
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
      pList "" true 
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
    (pList "," true pMsgDecl) block fprintf "};"

let pStateAnnot fmt annot =
  match annot with
  | AnnotNone -> ()
  | AnnotComplete -> fprintf fmt " : complete"
  | AnnotIncomplete -> fprintf fmt " : incomplete"
  | AnnotIncompleteEventList msglist -> 
      fprintf fmt " : incomplete (@[<v 0>%a@])" 
        (pList "," true pMsgDecl) msglist
  | AnnotIncompleteNum num ->
      fprintf fmt " : incomplete %d" num
  | AnnotIncompleteNumEventList (num, msglist) ->
      fprintf fmt " : incomplete %d (@[<v 0>%a@])" num 
        (pList "," true pMsgDecl) msglist

let pStateDecl fmt decl =
  let sdecl, annot = decl in
  pDecl pDesignator 
    (fun fmtloc declloc -> pStateAnnot fmtloc annot) fmt sdecl
  
let pStateDeclBlock prefix fmt block =
  musMakeIndentedBox fmt 
    pp_print_string (prefix ^ " : {")
    (pList "," true pStateDecl) block
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
    (pList "," true pTransDecl) block
    fprintf "};"

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
    | CompleteAutomaton (d, _, _, _, _, _) ->
        fprintf fmt "@[<v 0>automaton %a" pDesignator d
    | IncompleteAutomaton (d, _, _, _, _, _) ->
        fprintf fmt "@[<v 0>partialautomaton %a" pDesignator d
    | ChannelAutomaton (d, _, _) ->
        fprintf fmt "@[<v 0>channelautomaton %a" pDesignator d
  in
  let suffixPrinter fmt automaton =
    match automaton with
    | CompleteAutomaton (_, states, initstates, inblock, outblock, transblock)
    | IncompleteAutomaton (_, states, initstates, inblock, outblock, transblock) ->
        fprintf fmt "@,@[<v 4>{@,";
        pBlockCond (pStateDeclBlock "states") fmt states;
        pBlockCond (pStateDeclBlock "initstates") fmt initstates;
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
  let symtypes, automata, specs = prog in
  fprintf fmt "@[<v 0>";
  pSymTypeDeclBlock fmt symtypes;
  fprintf fmt "@,@,";
  List.iter (pAutomatonDecl fmt) automata;
  List.iter (fun spec -> pSpec fmt spec; fprintf fmt "@,@,") specs;
  fprintf fmt "@]"
