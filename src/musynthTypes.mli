type sourcelocation = int * int * int * int
exception ParseError of string * sourcelocation
exception SemanticError of string * sourcelocation option

module StringMap : Map.S with type key = string
module StringStringMap : Map.S with type key = (string * string)
module StringSet : Set.S with type elt = string
module StringStringSet : Set.S with type elt = (string * string)

type identifierT = string * sourcelocation option

module IdentMap : Map.S with type key = identifierT
module IdentSet : Set.S with type elt = identifierT
                    
type musSymTypeT =
  | SymTypeNamed of identifierT * sourcelocation option
  | SymTypeAnon of identifierT list * sourcelocation option

type musSymTypeDeclT = identifierT * musSymTypeT

type musSymTypeDeclBlockT = musSymTypeDeclT list

type musDesignatorT =
  | SimpleDesignator of identifierT
  | IndexDesignator of musDesignatorT * identifierT * sourcelocation option
  | FieldDesignator of musDesignatorT * identifierT * sourcelocation option

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
  | DeclQuantified of 'a * musSymTypeT IdentMap.t * musPropT option * 
                        sourcelocation option

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
  | SpecLTL of string * musPropT * (musPropT * musPropT) list *
                 (musPropT * musPropT) list * sourcelocation option
  | SpecDefine of identifierT * musPropT * sourcelocation option

type musProgT = musSymTypeDeclBlockT * musMsgDeclBlockT * musAutomatonDeclT list *
                  musInitStateDeclBlockT * musSpecT list

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

type 'a declEntry = 'a * (string * musSymTypeT) list * musPropT option

type symtabEntry =
  | SymtypeConst of string * musSymTypeT
  | StateName of string declEntry * string
  | GlobalMsgName of string declEntry
  | AutomatonMsgName of (string * msgType) declEntry * string
  | SymtypeName of string * musSymTypeT
  | StateVar of string
  | SymVarName of string * musSymTypeT
  | AutomatonName of (string * autType * symTabScope) declEntry
  | InvariantName of string * musPropT
  | LTLSpecName of string * musPropT * (musPropT * musPropT) list *
                     (musPropT * musPropT) list
  | DeclaredExpr of string * musPropT

and symTabScope = symtabEntry IdentMap.t ref

type symTableT = symTabScope list ref

type musExpType =
  | BooleanType
  | StateEnumType of string
  | AutomatonType of string

exception ImpurePropException of string
exception UndeclaredIdentifier of identifierT
exception WrongTypeIdentifier of (string * identifierT)
exception ConstantExpression of sourcelocation option
exception BddException of string

val locToString : int * int * int * int -> string
val locOptToString : (int * int * int * int) option -> string

val exToString : exn -> string

type llDesignatorT =
  | LLSimpleDesignator of string
  | LLIndexDesignator of llDesignatorT * string
  | LLFieldDesignator of llDesignatorT * string

module LLDesigSet : Set.S with type elt = llDesignatorT
module LLDesigMap : Map.S with type key = llDesignatorT
module LLDesigLLDesigMap : Map.S with type key = (llDesignatorT * llDesignatorT)
module LLDesigSetMap : Map.S with type key = LLDesigSet.t
module IntMap : Map.S with type key = int
module IntSet : Set.S with type elt = int

type llIdentT = llDesignatorT

type llTypeT = LLDesigSet.t

type llVarT = llDesignatorT * llTypeT

type llTransT =
  | TComplete of (llIdentT * llIdentT * llIdentT)
  | TParametrizedDest of (llIdentT * llIdentT * llVarT)
  | TParametrizedMsgDest of (llIdentT * llVarT * llVarT)

type llAnnotT =
  | LLAnnotEventList of llIdentT list
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
  | LLCompleteAutomaton of
      (llIdentT * llIdentT list * llIdentT list * llIdentT list *
         llTransT list * llFairnessT * llLossFairnessT * llDupFairnessT * 
           bool)
  | LLIncompleteAutomaton of
      (llIdentT * llIdentT list * llIdentT list * llIdentT list *
         llTransT list * llFairnessT)

type llPropT =
  | LLPropTrue
  | LLPropFalse
  | LLPropEquals of (llDesignatorT * llDesignatorT)
  | LLPropNot of llPropT
  | LLPropAnd of (llPropT * llPropT)
  | LLPropOr of (llPropT * llPropT)
  | LLPropTLX of llPropT
  | LLPropTLU of (llPropT * llPropT)

module PropMap : Map.S with type key = llPropT

type llSpecT =
  | LLSpecInvar of string * llPropT
  | LLSpecLTL of string * llPropT * (llPropT * llPropT) list *
      (llPropT * llPropT) list

type llProgT =
    llIdentT list * llAutomatonT list * llPropT * llSpecT list * llPropT

val lldesigToString : llDesignatorT -> string
val getPrimedLLDesig : llDesignatorT -> llDesignatorT
val getBaseLLDesig : llDesignatorT -> string
val countLLDesigParams : llDesignatorT -> int

type 'a synthExitStatT = 
  | SynthSafe 
  | SynthCEX of 'a

type 'a execExitStatT = 
  | ExecNonConverged of 'a 
  | ExecFixpoint of 'a

type 'a fairnessSpecT =
  | ProcessJustice of llDesignatorT * 'a * 'a
  | ProcessCompassion of llDesignatorT * 'a * 'a
  | LossCompassion of llDesignatorT * llDesignatorT * 'a * 'a
  | DupCompassion of llDesignatorT * llDesignatorT * 'a * 'a
  | Justice of 'a * 'a
  | Compassion of 'a * 'a
  | LTLJustice of 'a * 'a

type musynthTraceT = llDesignatorT LLDesigMap.t list

type 'a modelCheckingStatusT =
  | MCSuccess of 'a
  | MCFailureSafety of musynthTraceT
  | MCFailureLiveness of string * musynthTraceT * musynthTraceT
