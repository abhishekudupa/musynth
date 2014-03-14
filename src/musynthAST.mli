type sourcelocation = int * int * int * int
exception ParseError of string * sourcelocation
exception SemanticError of string * sourcelocation
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
val pIdentifier : Format.formatter -> string * 'a -> unit
val pList :
  string ->
  bool ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val pSymType : Format.formatter -> musSymTypeT -> unit
val pSymTypeDecl : Format.formatter -> (string * 'a) * musSymTypeT -> unit
val musMakeIndentedBox :
  Format.formatter ->
  (Format.formatter -> 'a -> unit) ->
  'a ->
  (Format.formatter -> 'b -> unit) ->
  'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
val pSymTypeDeclBlock :
  Format.formatter -> ((string * 'a) * musSymTypeT) list -> unit
val pDesignator : Format.formatter -> musDesignatorT -> unit
val pProp : Format.formatter -> musPropT -> unit
val pPropOpt : Format.formatter -> musPropT option -> unit
val pDecl :
  (Format.formatter -> 'a -> 'b) ->
  (Format.formatter -> 'a -> 'c) -> Format.formatter -> 'a musDeclType -> 'c
val noopPrinter : 'a -> 'b -> unit
val pMsgDecl : Format.formatter -> musDesignatorT musDeclType -> unit
val pMsgDeclBlock :
  string -> Format.formatter -> musDesignatorT musDeclType list -> unit
val pStateAnnot : Format.formatter -> musStateAnnotationT -> unit
val pStateDecl :
  Format.formatter ->
  musDesignatorT musDeclType * musStateAnnotationT -> unit
val pStateDeclBlock :
  string ->
  Format.formatter ->
  (musDesignatorT musDeclType * musStateAnnotationT) list -> unit
val pTransDecl :
  Format.formatter ->
  (musDesignatorT * musDesignatorT * musDesignatorT) musDeclType -> unit
val pTransDeclBlock :
  Format.formatter ->
  (musDesignatorT * musDesignatorT * musDesignatorT) musDeclType list -> unit
val pChanProp :
  Format.formatter -> musChanOrdT * musChanLossT * musChanDupT * int -> unit
val pAutomatonDecl :
  Format.formatter -> musAutomatonDeclType musDeclType -> unit
val pSpec : Format.formatter -> musSpecT -> unit
val pProg :
  Format.formatter ->
  ((string * 'a) * musSymTypeT) list *
  musAutomatonDeclType musDeclType list * musSpecT list -> unit
