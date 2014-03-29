(* AST types *)
(* file name * start line num * start col * end line num * end col *)
type sourcelocation = int * int * int * int
  
(* Filename * Line Num * Col num *)
exception ParseError of string * sourcelocation
exception SemanticError of string * sourcelocation option
    
module StringMap = Map.Make
    (struct 
      type t = string 
      let compare = Pervasives.compare 
    end)

module StringSet = Set.Make
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
  | SymTypeNamed of identifierT * sourcelocation option
  | SymTypeAnon of identifierT list * sourcelocation option
        
type musSymTypeDeclT = identifierT * musSymTypeT
  
type musSymTypeDeclBlockT = musSymTypeDeclT list
  
type musDesignatorT =
  | SimpleDesignator of identifierT
  | IndexDesignator of musDesignatorT * identifierT * sourcelocation option
  | FieldDesignator of musDesignatorT * identifierT * sourcelocation option

(* Only CTL at present *)
type musPropT =
  | PropTrue of sourcelocation option
  | PropFalse of sourcelocation option
  | PropDefine of identifierT
  | PropEquals of (musDesignatorT * musDesignatorT * sourcelocation option)
  | PropNEquals of (musDesignatorT * musDesignatorT * sourcelocation option)
  | PropNot of musPropT * sourcelocation option
  | PropAnd of musPropT * musPropT * sourcelocation option
  | PropOr of musPropT * musPropT * sourcelocation option
  | PropImplies of musPropT * musPropT * sourcelocation option
  | PropIff of musPropT * musPropT * sourcelocation option
  | PropForall of identifierT list * musSymTypeT * musPropT * sourcelocation option
  | PropExists of identifierT list * musSymTypeT * musPropT * sourcelocation option
  | PropCTLAG of musPropT * sourcelocation option
  | PropCTLAF of musPropT * sourcelocation option
  | PropCTLAX of musPropT * sourcelocation option
  | PropCTLEG of musPropT * sourcelocation option
  | PropCTLEF of musPropT * sourcelocation option
  | PropCTLEX of musPropT * sourcelocation option
  | PropCTLAU of musPropT * musPropT * sourcelocation option
  | PropCTLEU of musPropT * musPropT * sourcelocation option
    
type 'a musDeclType = 
  | DeclSimple of 'a * sourcelocation option
  | DeclQuantified of 'a * (musSymTypeT IdentMap.t) * musPropT option * sourcelocation option

type musMsgDeclT = musDesignatorT musDeclType

type musMsgDeclBlockT = musMsgDeclT list

type musStateAnnotationT = 
  | AnnotNone
  | AnnotComplete of sourcelocation option
  | AnnotIncomplete of sourcelocation option
  | AnnotIncompleteEventList of musMsgDeclT list * sourcelocation option
  | AnnotIncompleteNum of int * sourcelocation option
  | AnnotIncompleteNumEventList of int * musMsgDeclT list * sourcelocation option

type musStateDeclT = musDesignatorT musDeclType * musStateAnnotationT

type musStateDeclBlockT = musStateDeclT list

type musTransDeclT = (musDesignatorT * musDesignatorT * musDesignatorT) musDeclType

type musTransDeclBlockT = musTransDeclT list

type musChanDupT = 
  | ChanDuplicating of sourcelocation option
  | ChanNonDuplicating of sourcelocation option

type musChanOrdT = 
  | ChanOrdered of sourcelocation option 
  | ChanUnordered of sourcelocation option

type musChanLossT = 
  | ChanLossy of sourcelocation option
  | ChanLossless of sourcelocation option

type musChanPropT = musChanOrdT * musChanLossT * musChanDupT * int

type musInitStateDeclT = ((musDesignatorT * musDesignatorT) list) musDeclType

type musInitStateDeclBlockT = musInitStateDeclT list

type musAutomatonDeclType = 
  | CompleteAutomaton of musDesignatorT * musStateDeclBlockT *
        musMsgDeclBlockT * musMsgDeclBlockT * musTransDeclBlockT * sourcelocation option
  | IncompleteAutomaton of musDesignatorT * musStateDeclBlockT *
        musMsgDeclBlockT * musMsgDeclBlockT * musTransDeclBlockT * sourcelocation option
  | ChannelAutomaton of musDesignatorT * musChanPropT * musMsgDeclBlockT * sourcelocation option

type musAutomatonDeclT = musAutomatonDeclType musDeclType

type musSpecT = 
  | SpecInvar of string * musPropT * sourcelocation option
  | SpecCTL of string * musPropT * sourcelocation option
  | SpecDefine of identifierT * musPropT * sourcelocation option

type musProgT = musSymTypeDeclBlockT * musMsgDeclBlockT * 
      musAutomatonDeclT list * musInitStateDeclBlockT * musSpecT list

(* Symbol table types *)
exception SymtabUnderflow
exception DuplicateSymbol of identifierT

type msgType = 
  | InputMsg
  | OutputMsg

type autType =
  | ChannelAutType
  | PartialAutType
  | CompleteAutType

type 'a declEntry = ('a * (string * musSymTypeT) list * musPropT option)

type symtabEntry =
  | SymtypeConst of string * musSymTypeT
  | StateName of string declEntry
  | MessageName of string declEntry
  | AutomataMsgName of (string * msgType) declEntry
  | SymtypeName of string * musSymTypeT
  | StateVar of string list
  | SymVarName of string * musSymTypeT
  | AutomatonName of string * autType * musSymTypeT list * musPropT option * symTabScope
  | InvariantName of string * musPropT
  | CTLSpecName of string * musPropT
  | DeclaredExpr of string * musPropT

and symTabScope = symtabEntry IdentMap.t ref

type symTableT = (symTabScope list) ref

(* checker exceptions *)
exception ImpurePropException of string
exception UndeclaredIdentifier of identifierT
exception WrongTypeIdentifier of (string * identifierT)
exception ConstantExpression of sourcelocation option

(* stringification of source locations *)
let locToString loc =
  let sline, scol, eline, ecol = loc in
  (string_of_int sline) ^ ":" ^ (string_of_int scol) ^ " - " ^
  (string_of_int eline) ^ ":" ^ (string_of_int ecol)
  
let locOptToString locOpt =
  match locOpt with
  | None -> "No source location information found"
  | Some locn -> locToString locn

(* exception printing routines *)
let exToString ex =
  match ex with
  | ParseError (msg, loc) ->
      "Parse Error: " ^ msg ^ "\nAt: " ^ (locOptToString (Some loc))
  | SemanticError (msg, loc) ->
      "Semantic Error: " ^ msg ^ "\nAt:" ^ (locOptToString loc)
  | SymtabUnderflow -> "Error: Symbol table underflow"
  | DuplicateSymbol ident ->
      let name, loc = ident in
      "Error: Identifier \"" ^ name ^ "\" already declared\nAt: " ^ 
      (locOptToString loc)

  | WrongTypeIdentifier (msg, ident) ->
      let name, loc = ident in
      "Error: Identifier \"" ^ name ^ "\" has wrong type.\n" ^ msg ^ "\nAt: " ^ 
      (locOptToString loc)

  | ConstantExpression loc ->
      "Error: Constant Expression at: " ^ (locOptToString loc)
  | UndeclaredIdentifier ident ->
      let name, loc = ident in
      "Error: Undeclared Identifier " ^ name ^ "\nAt: " ^ (locOptToString loc)
  | _ -> Printexc.to_string ex


(* low level IR types *)
type llIdentT = string * string list

type llAnnotT =
  | LLAnnotComplete
  | LLAnnotIncomplete
  | LLAnnotIncompleteNum of int
  | LLAnnotIncompleteEventList of llIdentT list
  | LLAnnotIncompleteNumEventList of int * llIdentT list

type llStateT = llIdentT * llAnnotT

module LLIdentSet = Set.Make
    (struct 
      type t = llIdentT
      let compare = Pervasives.compare
    end)

module LLIdentMap = Map.Make
    (struct
      type t = llIdentT
      let compare = Pervasives.compare
    end)

type llCompleteTransT = (llIdentT * llIdentT * llIdentT)

type llParametricTransT = (llIdentT * llIdentT * LLIdentSet.t)

(* name, state set, inmsgs, outmsgs, transitions *)
type llAutomatonT = 
  | LLCompleteAutomaton of (llIdentT * llIdentT list * 
                              llIdentT list * llIdentT list * llCompleteTransT list)
  | LLIncompleteAutomaton of (llIdentT * llIdentT list * 
                                llIdentT list * llIdentT list * llParametricTransT list)

type llDesignatorT = 
  | LLDesigConst of llIdentT
  | LLDesigState of llIdentT

type llPropT = 
  | LLPropTrue
  | LLPropFalse
  | LLPropEquals of (llDesignatorT * llDesignatorT)
  | LLPropNEquals of (llDesignatorT * llDesignatorT)
  | LLPropNot of llPropT
  | LLPropAnd of (llPropT * llPropT)
  | LLPropOr of (llPropT * llPropT)
  | LLPropCTLAG of llPropT
  | LLPropCTLAF of llPropT
  | LLPropCTLAX of llPropT
  | LLPropCTLEG of llPropT
  | LLPropCTLEF of llPropT
  | LLPropCTLEX of llPropT
  | LLPropCTLAU of (llPropT * llPropT)
  | LLPropCTLEU of (llPropT * llPropT)

type llSpecT =
  | LLSpecInvar of (string * llPropT)
  | LLSpecCTL of (string * llPropT)

(* automata, initstate list, spec list *)
type llProgT = llAutomatonT list * (llIdentT LLIdentMap.t) list *  llSpecT list
