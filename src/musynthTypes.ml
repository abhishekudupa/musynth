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

module IdentSet = Set.Make
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
  | PropTLG of musPropT * sourcelocation option
  | PropTLF of musPropT * sourcelocation option 
  | PropTLX of musPropT * sourcelocation option
  | PropTLU of musPropT * musPropT * sourcelocation option
  | PropTLR of musPropT * musPropT * sourcelocation option
                                                    
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

type musChanBlockT =
  | ChanBlocking of sourcelocation option
  | ChanNonBlocking of sourcelocation option

type musChanPropT = musChanOrdT * musChanLossT * musChanDupT * musChanBlockT * int

type musLossFairnessT =
  | LossFairnessNone
  | LossFairnessFinite of sourcelocation option

type musDupFairnessT =
  | DupFairnessNone
  | DupFairnessFinite of sourcelocation option

type musFairnessT = 
  | FairnessTypeJustice of sourcelocation option
  | FairnessTypeCompassion of sourcelocation option
  | FairnessTypeNone

type musInitStateDeclT = musPropT

type musInitStateDeclBlockT = musInitStateDeclT list

type musAutomatonDeclType = 
  | CompleteAutomaton of musDesignatorT * musStateDeclBlockT *
                           musMsgDeclBlockT * musMsgDeclBlockT * 
                             musTransDeclBlockT * musFairnessT * 
                               sourcelocation option
  | IncompleteAutomaton of musDesignatorT * musStateDeclBlockT *
                             musMsgDeclBlockT * musMsgDeclBlockT * 
                               musTransDeclBlockT * musFairnessT * 
                                 sourcelocation option
  | ChannelAutomaton of musDesignatorT * musChanPropT * 
                          musMsgDeclBlockT * musFairnessT * 
                            musLossFairnessT * musDupFairnessT *
                              sourcelocation option

type musAutomatonDeclT = musAutomatonDeclType musDeclType

type musSpecT = 
  | SpecInvar of string * musPropT * sourcelocation option
  | SpecLTL of string * musPropT * musPropT list * 
                 (musPropT * musPropT) list * sourcelocation option
  | SpecDefine of identifierT * musPropT * sourcelocation option

type musProgT = musSymTypeDeclBlockT * musMsgDeclBlockT * 
                  musAutomatonDeclT list * musInitStateDeclBlockT * musSpecT list

(* Symbol table types *)
exception SymtabUnderflow
exception DuplicateSymbol of identifierT
exception UnimplementedException

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
  (* params, automaton name *)
  | StateName of string declEntry * string
  | GlobalMsgName of string declEntry
  | AutomatonMsgName of (string * msgType) declEntry * string
  | SymtypeName of string * musSymTypeT
  (* automaton name *)
  | StateVar of string
  | SymVarName of string * musSymTypeT
  | AutomatonName of (string * autType * symTabScope) declEntry
  | InvariantName of string * musPropT
  (* name, ltl property * fairness list *)
  | LTLSpecName of string * musPropT * musPropT list * (musPropT * musPropT) list
  | DeclaredExpr of string * musPropT

 and symTabScope = symtabEntry IdentMap.t ref

type symTableT = (symTabScope list) ref

(* type checking *)

type musExpType = 
  | BooleanType
  | StateEnumType of string
  | AutomatonType of string

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

exception BddException of string

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
  | BddException str ->
     "Error: BDD operation failed. Details: " ^ str
  | _ -> Printexc.to_string ex

(* types for low-level representation *)
type llDesignatorT = 
  | LLSimpleDesignator of string
  | LLIndexDesignator of llDesignatorT * string
  | LLFieldDesignator of llDesignatorT * string

module LLDesigSet = Set.Make
                      (struct
                        type t = llDesignatorT
                        let compare = Pervasives.compare
                      end)

module LLDesigMap = Map.Make
                      (struct 
                        type t = llDesignatorT
                        let compare = Pervasives.compare
                      end)

module LLDesigLLDesigMap = Map.Make
                             (struct 
                               type t = llDesignatorT * llDesignatorT
                               let compare = Pervasives.compare
                             end)

module LLDesigSetMap = 
  Map.Make
    (struct 
      type t = LLDesigSet.t
      let compare = LLDesigSet.compare
     end)

module IntMap = Map.Make
                  (struct 
                    type t = int
                    let compare = Pervasives.compare
                  end)

module IntSet = Set.Make
                  (struct 
                    type t = int
                    let compare = Pervasives.compare
                  end)

type llIdentT = llDesignatorT

(* the set of symbolic values a variable can take *)
type llTypeT = LLDesigSet.t

type llVarT = llDesignatorT * llTypeT

type llTransT = 
  | TComplete of (llIdentT * llIdentT * llIdentT)
  | TParametrizedDest of (llIdentT * llIdentT * llVarT)
  | TParametrizedMsgDest of (llIdentT * llVarT * llVarT)

type llAnnotT = 
  | LLAnnotEventList of (llIdentT list)
  | LLAnnotNumEventList of (int * llIdentT list)
  | LLAnnotNone

type llFairnessT = 
  | LLFairnessJustice
  | LLFairnessCompassion
  | LLFairnessNone

type llDupFairnessT =
  | LLDupFairnessNone 
  | LLDupFairnessFinite

type llLossFairnessT =
  | LLLossFairnessNone
  | LLLossFairnessFinite

type llAutomatonT = 
  (* name, states, msgs, transitions, ischannel *)
  | LLCompleteAutomaton of (llIdentT * llIdentT list * llIdentT list * 
                              llIdentT list * llTransT list * llFairnessT * 
                                llLossFairnessT * llDupFairnessT * bool)
  | LLIncompleteAutomaton of (llIdentT * llIdentT list * llIdentT list * 
                                llIdentT list * llTransT list * llFairnessT)

type llPropT = 
  | LLPropTrue
  | LLPropFalse
  | LLPropEquals of (llDesignatorT * llDesignatorT)
  | LLPropNot of llPropT
  | LLPropAnd of (llPropT * llPropT)
  | LLPropOr of (llPropT * llPropT)
  | LLPropTLX of llPropT
  | LLPropTLU of (llPropT * llPropT)

module PropMap = Map.Make
                   (struct 
                     type t = llPropT
                     let compare = Pervasives.compare
                   end)

(* init states, accepting states, transitions *)
type llMonitorT = (llIdentT list * llIdentT list * (llIdentT * musPropT * llIdentT) list)

type llSpecT =
  | LLSpecInvar of string * llPropT
  | LLSpecLTL of string * llPropT * llPropT list * (llPropT * llPropT) list

(* global messages, automata, initial state constraints, properties *)
type llProgT = (llIdentT list * llAutomatonT list * llPropT * llSpecT list)

(* helper to convert designators to strings *)
let rec lldesigToString lldesig =
  match lldesig with
  | LLSimpleDesignator name -> name
  | LLIndexDesignator (ndesig, name) -> (lldesigToString ndesig) ^ "[" ^ name ^ "]"
  | LLFieldDesignator (ndesig, name) -> (lldesigToString ndesig) ^ "." ^ name

let rec getPrimedLLDesig lldesig =
  match lldesig with
  | LLSimpleDesignator name -> LLSimpleDesignator (name ^ "'")
  | LLIndexDesignator (ndesig, name) -> LLIndexDesignator (getPrimedLLDesig ndesig, name)
  | LLFieldDesignator (ndesig, name) -> LLFieldDesignator (ndesig, name ^ "'")

(* type for signalling synthesis status *)
type 'a synthExitStatT =
  | SynthSafe
  | SynthCEX of 'a

(* type for signalling fixpoints *)
type 'a execExitStatT =
  | ExecNonConverged of 'a
  | ExecFixpoint of 'a

(* type for constructing fairness specs *)
(* Paramterized. We use the same type for bdds as well as props *)
type fairnessSpecT =
  | Justice of llPropT
  | Compassion of llPropT * llPropT

(* type for traces *)
type musynthTraceT = (llDesignatorT LLDesigMap.t) list

type 'a modelCheckingStatusT =
  | MCSuccess of 'a
  | MCFailureSafety of musynthTraceT
  | MCFailureLiveness of string * musynthTraceT * musynthTraceT

