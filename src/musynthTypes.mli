type sourcelocation = int * int * int * int
exception ParseError of string * sourcelocation
exception SemanticError of string * sourcelocation option
module StringMap :
  sig
    type key = string
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
type identifierT = string * sourcelocation option
module IdentMap :
  sig
    type key = identifierT
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
type musSymTypeT =
    SymTypeNamed of identifierT * sourcelocation option
  | SymTypeAnon of identifierT list * sourcelocation option
type musSymTypeDeclT = identifierT * musSymTypeT
type musSymTypeDeclBlockT = musSymTypeDeclT list
type musDesignatorT =
    SimpleDesignator of identifierT
  | IndexDesignator of musDesignatorT * identifierT * sourcelocation option
  | FieldDesignator of musDesignatorT * identifierT * sourcelocation option
type musPropT =
    PropTrue of sourcelocation option
  | PropFalse of sourcelocation option
  | PropEquals of (musDesignatorT * musDesignatorT * sourcelocation option)
  | PropNEquals of (musDesignatorT * musDesignatorT * sourcelocation option)
  | PropNot of musPropT * sourcelocation option
  | PropAnd of musPropT * musPropT * sourcelocation option
  | PropOr of musPropT * musPropT * sourcelocation option
  | PropImplies of musPropT * musPropT * sourcelocation option
  | PropIff of musPropT * musPropT * sourcelocation option
  | PropForall of identifierT list * musSymTypeT * musPropT *
      sourcelocation option
  | PropExists of identifierT list * musSymTypeT * musPropT *
      sourcelocation option
  | PropCTLAG of musPropT * sourcelocation option
  | PropCTLAF of musPropT * sourcelocation option
  | PropCTLAX of musPropT * sourcelocation option
  | PropCTLEG of musPropT * sourcelocation option
  | PropCTLEF of musPropT * sourcelocation option
  | PropCTLEX of musPropT * sourcelocation option
  | PropCTLAU of musPropT * musPropT * sourcelocation option
  | PropCTLEU of musPropT * musPropT * sourcelocation option
type 'a musDeclType =
    DeclSimple of 'a * sourcelocation option
  | DeclQuantified of 'a * musSymTypeT IdentMap.t * musPropT option *
      sourcelocation option
type musMsgDeclT = musDesignatorT musDeclType
type musMsgDeclBlockT = musMsgDeclT list
type musStateAnnotationT =
    AnnotNone
  | AnnotComplete of sourcelocation option
  | AnnotIncomplete of sourcelocation option
  | AnnotIncompleteEventList of musMsgDeclT list * sourcelocation option
  | AnnotIncompleteNum of int * sourcelocation option
  | AnnotIncompleteNumEventList of int * musMsgDeclT list *
      sourcelocation option
type musStateDeclT = musDesignatorT musDeclType * musStateAnnotationT
type musStateDeclBlockT = musStateDeclT list
type musTransDeclT =
    (musDesignatorT * musDesignatorT * musDesignatorT) musDeclType
type musTransDeclBlockT = musTransDeclT list
type musChanDupT =
    ChanDuplicating of sourcelocation option
  | ChanNonDuplicating of sourcelocation option
type musChanOrdT =
    ChanOrdered of sourcelocation option
  | ChanUnordered of sourcelocation option
type musChanLossT =
    ChanLossy of sourcelocation option
  | ChanLossless of sourcelocation option
type musChanPropT = musChanOrdT * musChanLossT * musChanDupT * int
type musInitStateDeclT = (musDesignatorT * musDesignatorT) list musDeclType
type musInitStateDeclBlockT = musInitStateDeclT list
type musAutomatonDeclType =
    CompleteAutomaton of musDesignatorT * musStateDeclBlockT *
      musMsgDeclBlockT * musMsgDeclBlockT * musTransDeclBlockT *
      sourcelocation option
  | IncompleteAutomaton of musDesignatorT * musStateDeclBlockT *
      musMsgDeclBlockT * musMsgDeclBlockT * musTransDeclBlockT *
      sourcelocation option
  | ChannelAutomaton of musDesignatorT * musChanPropT * musMsgDeclBlockT *
      sourcelocation option
type musAutomatonDeclT = musAutomatonDeclType musDeclType
type musSpecT =
    SpecInvar of string * musPropT * sourcelocation option
  | SpecCTL of string * musPropT * sourcelocation option
type musProgT =
    musSymTypeDeclBlockT * musAutomatonDeclT list * musInitStateDeclBlockT *
    musSpecT list
exception SymtabUnderflow
exception DuplicateSymbol of identifierT
type msgType = InputMsg | OutputMsg
type autType = ChannelAutType | PartialAutType | CompleteAutType
type symtabEntry =
    SymtypeConst of string * musSymTypeT
  | StateName of string * musSymTypeT list * musPropT option * string
  | MsgName of string * msgType * musSymTypeT list * musPropT option
  | SymtypeName of string * musSymTypeT
  | StateVar of string list * string
  | SymVarName of string * musSymTypeT
  | AutomatonName of string * autType * musSymTypeT list * musPropT option *
      symTabScope
  | InvariantName of string * musPropT
  | CTLSpecName of string * musPropT
and symTabScope = symtabEntry IdentMap.t ref
type symTableT = symTabScope list ref
exception ImpurePropException of string
exception UndeclaredIdentifier of identifierT
exception WrongTypeIdentifier of (string * identifierT)
exception ConstantExpression of sourcelocation option
val locToString : int * int * int * int -> string
val locOptToString : (int * int * int * int) option -> string
val exToString : exn -> string
