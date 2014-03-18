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
    SymTypeNamed of identifierT
  | SymTypeAnon of identifierT list
type musSymTypeDeclT = identifierT * musSymTypeT
type musSymTypeDeclBlockT = musSymTypeDeclT list
type musDesignatorT =
    SimpleDesignator of identifierT
  | IndexDesignator of musDesignatorT * identifierT
  | FieldDesignator of musDesignatorT * identifierT
type musPropT =
    PropTrue
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
    DeclSimple of 'a
  | DeclQuantified of 'a * musSymTypeT IdentMap.t * musPropT option
type musMsgDeclT = musDesignatorT musDeclType
type musMsgDeclBlockT = musMsgDeclT list
type musStateAnnotationT =
    AnnotNone
  | AnnotComplete
  | AnnotIncomplete
  | AnnotIncompleteEventList of musMsgDeclT list
  | AnnotIncompleteNum of int
  | AnnotIncompleteNumEventList of int * musMsgDeclT list
type musStateDeclT = musDesignatorT musDeclType * musStateAnnotationT
type musStateDeclBlockT = musStateDeclT list
type musTransDeclT =
    (musDesignatorT * musDesignatorT * musDesignatorT) musDeclType
type musTransDeclBlockT = musTransDeclT list
type musChanDupT = ChanDuplicating | ChanNonDuplicating
type musChanOrdT = ChanOrdered | ChanUnordered
type musChanLossT = ChanLossy | ChanLossless
type musChanPropT = musChanOrdT * musChanLossT * musChanDupT * int
type musAutomatonDeclType =
    CompleteAutomaton of musDesignatorT * musStateDeclBlockT *
      musStateDeclBlockT * musMsgDeclBlockT * musMsgDeclBlockT *
      musTransDeclBlockT
  | IncompleteAutomaton of musDesignatorT * musStateDeclBlockT *
      musStateDeclBlockT * musMsgDeclBlockT * musMsgDeclBlockT *
      musTransDeclBlockT
  | ChannelAutomaton of musDesignatorT * musChanPropT * musMsgDeclBlockT
type musAutomatonDeclT = musAutomatonDeclType musDeclType
type musSpecT = SpecInvar of string * musPropT | SpecCTL of string * musPropT
type musProgT = musSymTypeDeclBlockT * musAutomatonDeclT list * musSpecT list
exception SymtabUnderflow
exception DuplicateSymbol of identifierT
type msgType = InputMsg | OutputMsg
type autType = ChannelAutType | PartialAutType | CompleteAutType
type symtabEntry =
    VarEntry of musSymTypeT
  | TypeEntry of musSymTypeT
  | StateEntry of musSymTypeT list option
  | MsgEntry of msgType * musSymTypeT list option
  | AutomatonEntry of autType * musSymTypeT list option * symTabScope
and symTabScope = symtabEntry IdentMap.t ref
type symTableT = symTabScope list ref
val locToString : (int * int * int * int) option -> string
exception ImpurePropException of string
exception UndeclaredIdentifier of identifierT
val exToString : exn -> string
