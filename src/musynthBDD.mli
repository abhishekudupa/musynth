module CK :
  sig
    module ST :
      sig
        val createSymTab : unit -> 'a MusynthTypes.IdentMap.t ref list ref
        val push : 'a MusynthTypes.IdentMap.t ref list ref -> unit
        val pushScope : 'a list ref -> 'a -> unit
        val pop : 'a list ref -> 'a
        val peek : 'a list ref -> 'a
        val getNumScopes : 'a list ref -> int
        val lookup :
          'a MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> 'a option
        val bind :
          'a MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> 'a -> unit
        val bindGlobal :
          'a MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> 'a -> unit
        val lookupOrFail :
          'a MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> 'a
        val lookupVar :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT
        val lookupType :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT
        val lookupGMsg :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
        val lookupAMsg :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
        val lookupState :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
        val lookupAutomaton :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
        val lookupConst :
          MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
          MusynthTypes.IdentMap.key -> MusynthTypes.symtabEntry
      end
    val curAutomatonName : string ref
    val resolveSymType :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.musSymTypeT -> MusynthTypes.musSymTypeT
    val checkSymTypeDecl :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT -> unit
    val destructDesigDecl :
      MusynthTypes.musDesignatorT ->
      MusynthTypes.identifierT * MusynthTypes.identifierT list
    val getObligationsForIdent :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key ->
      (string * MusynthTypes.musSymTypeT) list * MusynthTypes.musPropT option
    val getTypeObligationsForIdent :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT list
    val checkTypeLists :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      string * MusynthTypes.sourcelocation option ->
      MusynthTypes.musSymTypeT list -> MusynthTypes.IdentMap.key list -> unit
    val getDesigType :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT -> MusynthTypes.symtabEntry
    val checkTypeCompatibility :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT ->
      MusynthTypes.musDesignatorT ->
      MusynthTypes.sourcelocation option ->
      MusynthTypes.symtabEntry * MusynthTypes.symtabEntry
    val checkPureProp :
      MusynthTypes.symTabScope list ref -> MusynthTypes.musPropT -> unit
    val checkPureQProp :
      MusynthTypes.symTabScope list ref -> MusynthTypes.musPropT -> unit
    val desigDeclChecker :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.musDesignatorT ->
      'a ->
      MusynthTypes.identifierT *
      (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
    val transChecker :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
      MusynthTypes.musDesignatorT ->
      MusynthTypes.sourcelocation option -> string * 'a list
    val autMsgDeclChecker :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT ->
      MusynthTypes.sourcelocation option ->
      MusynthTypes.identifierT *
      (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
    val checkDecl :
      MusynthTypes.symTabScope list ref ->
      (MusynthTypes.symTabScope list ref ->
       'a -> MusynthTypes.sourcelocation option -> 'b * 'c) ->
      'a MusynthTypes.musDeclType ->
      'b * 'c * MusynthTypes.musPropT option * MusynthTypes.symTabScope
    val cvtParamTypeListForSymtab : (('a * 'b) * 'c) list -> ('a * 'c) list
    val checkGlobalMsgDecl :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
    val checkStateDecl :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
    val checkAutomatonMsgDecl :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.msgType ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
    val checkStateDeclBlock :
      MusynthTypes.symTabScope list ref ->
      (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
       MusynthTypes.musStateAnnotationT)
      list -> bool -> unit
    val checkGlobalMsgDeclBlock :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
    val checkAutomatonMsgDeclBlock :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.msgType ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
    val convertDesigToPrimed :
      MusynthTypes.musDesignatorT -> MusynthTypes.musDesignatorT
    val convertDesigDeclToPrimed :
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType
    val checkAutDef :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musAutomatonDeclType ->
      'a ->
      MusynthTypes.identifierT *
      (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list
    val initStateDeclChecker :
      MusynthTypes.symTabScope list ref ->
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT) list ->
      MusynthTypes.sourcelocation option -> string * 'a list
    val checkProp :
      MusynthTypes.symTabScope list ref -> MusynthTypes.musPropT -> unit
    val checkSpec :
      MusynthTypes.symTabScope list ref -> MusynthTypes.musSpecT -> unit
    val checkProg :
      MusynthTypes.symTabScope list ref ->
      (MusynthTypes.IdentMap.key * MusynthTypes.musSymTypeT) list *
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
      MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT) list
      MusynthTypes.musDeclType list * MusynthTypes.musSpecT list -> unit
  end
module AST :
  sig
    val pLoc : Format.formatter -> int * int * int * int -> unit
    val pLocOpt : Format.formatter -> (int * int * int * int) option -> unit
    val pIdentifier : Format.formatter -> string * 'a -> unit
    val identToName : 'a * 'b -> 'a
    val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
    val pList :
      string ->
      bool ->
      bool ->
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
    val pSymType : Format.formatter -> MusynthTypes.musSymTypeT -> unit
    val pSymTypeDecl :
      Format.formatter -> (string * 'a) * MusynthTypes.musSymTypeT -> unit
    val musMakeIndentedBox :
      Format.formatter ->
      (Format.formatter -> 'a -> unit) ->
      'a ->
      (Format.formatter -> 'b -> unit) ->
      'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
    val pSymTypeDeclBlock :
      Format.formatter ->
      ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
    val pDesignator : Format.formatter -> MusynthTypes.musDesignatorT -> unit
    val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
    val pPropOpt : Format.formatter -> MusynthTypes.musPropT option -> unit
    val pDecl :
      (Format.formatter -> 'a -> 'b) ->
      (Format.formatter -> 'a -> 'c) ->
      Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
    val noopPrinter : 'a -> 'b -> unit
    val pMsgDecl :
      Format.formatter ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
    val pMsgDeclBlock :
      string ->
      Format.formatter ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
    val pMessagesDeclBlock :
      Format.formatter ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
    val pStateAnnot :
      Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
    val pStateDecl :
      Format.formatter ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
      MusynthTypes.musStateAnnotationT -> unit
    val pStateDeclBlock :
      string ->
      Format.formatter ->
      (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
       MusynthTypes.musStateAnnotationT)
      list -> unit
    val pTransDecl :
      Format.formatter ->
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
       MusynthTypes.musDesignatorT)
      MusynthTypes.musDeclType -> unit
    val pTransDeclBlock :
      Format.formatter ->
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
       MusynthTypes.musDesignatorT)
      MusynthTypes.musDeclType list -> unit
    val pInitStateConstraint :
      Format.formatter ->
      MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT -> unit
    val pInitStateDecl :
      Format.formatter ->
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT) list
      MusynthTypes.musDeclType -> unit
    val pInitStateDeclBlock :
      Format.formatter ->
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT) list
      MusynthTypes.musDeclType list -> unit
    val pChanProp :
      Format.formatter ->
      MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
      MusynthTypes.musChanDupT * int -> unit
    val pAutomatonDecl :
      Format.formatter ->
      MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType -> unit
    val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
    val pProg :
      Format.formatter ->
      ((string * 'a) * MusynthTypes.musSymTypeT) list *
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
      MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT) list
      MusynthTypes.musDeclType list * MusynthTypes.musSpecT list -> unit
    val pLLDesignator :
      Format.formatter -> MusynthTypes.llDesignatorT -> unit
    val pLLIdent : Format.formatter -> MusynthTypes.llDesignatorT -> unit
    val pLLVar :
      Format.formatter ->
      MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
    val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
    val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
    val pLLAutomaton : Format.formatter -> MusynthTypes.llAutomatonT -> unit
  end
module Utils :
  sig
    module AST :
      sig
        val pLoc : Format.formatter -> int * int * int * int -> unit
        val pLocOpt :
          Format.formatter -> (int * int * int * int) option -> unit
        val pIdentifier : Format.formatter -> string * 'a -> unit
        val identToName : 'a * 'b -> 'a
        val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
        val pList :
          string ->
          bool ->
          bool ->
          (Format.formatter -> 'a -> unit) ->
          Format.formatter -> 'a list -> unit
        val pSymType : Format.formatter -> MusynthTypes.musSymTypeT -> unit
        val pSymTypeDecl :
          Format.formatter ->
          (string * 'a) * MusynthTypes.musSymTypeT -> unit
        val musMakeIndentedBox :
          Format.formatter ->
          (Format.formatter -> 'a -> unit) ->
          'a ->
          (Format.formatter -> 'b -> unit) ->
          'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
        val pSymTypeDeclBlock :
          Format.formatter ->
          ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
        val pDesignator :
          Format.formatter -> MusynthTypes.musDesignatorT -> unit
        val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
        val pPropOpt :
          Format.formatter -> MusynthTypes.musPropT option -> unit
        val pDecl :
          (Format.formatter -> 'a -> 'b) ->
          (Format.formatter -> 'a -> 'c) ->
          Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
        val noopPrinter : 'a -> 'b -> unit
        val pMsgDecl :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
        val pMsgDeclBlock :
          string ->
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val pMessagesDeclBlock :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
        val pStateAnnot :
          Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
        val pStateDecl :
          Format.formatter ->
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
          MusynthTypes.musStateAnnotationT -> unit
        val pStateDeclBlock :
          string ->
          Format.formatter ->
          (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
           MusynthTypes.musStateAnnotationT)
          list -> unit
        val pTransDecl :
          Format.formatter ->
          (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
           MusynthTypes.musDesignatorT)
          MusynthTypes.musDeclType -> unit
        val pTransDeclBlock :
          Format.formatter ->
          (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
           MusynthTypes.musDesignatorT)
          MusynthTypes.musDeclType list -> unit
        val pInitStateConstraint :
          Format.formatter ->
          MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT -> unit
        val pInitStateDecl :
          Format.formatter ->
          (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT) list
          MusynthTypes.musDeclType -> unit
        val pInitStateDeclBlock :
          Format.formatter ->
          (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT) list
          MusynthTypes.musDeclType list -> unit
        val pChanProp :
          Format.formatter ->
          MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
          MusynthTypes.musChanDupT * int -> unit
        val pAutomatonDecl :
          Format.formatter ->
          MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType -> unit
        val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
        val pProg :
          Format.formatter ->
          ((string * 'a) * MusynthTypes.musSymTypeT) list *
          MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
          MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
          (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT) list
          MusynthTypes.musDeclType list * MusynthTypes.musSpecT list -> 
          unit
        val pLLDesignator :
          Format.formatter -> MusynthTypes.llDesignatorT -> unit
        val pLLIdent : Format.formatter -> MusynthTypes.llDesignatorT -> unit
        val pLLVar :
          Format.formatter ->
          MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
        val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
        val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
        val pLLAutomaton :
          Format.formatter -> MusynthTypes.llAutomatonT -> unit
      end
    module MSSet :
      sig
        type elt = int MusynthTypes.LLDesigMap.t
        type t = MusynthUtils.MSSet.t
        val empty : t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val diff : t -> t -> t
        val compare : t -> t -> int
        val equal : t -> t -> bool
        val subset : t -> t -> bool
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val filter : (elt -> bool) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val max_elt : t -> elt
        val choose : t -> elt
        val split : elt -> t -> t * bool * t
      end
    val splatList : 'a -> int -> 'a list
    val makeEmptyMS : unit -> 'a MusynthTypes.LLDesigMap.t
    val msToList :
      int MusynthTypes.LLDesigMap.t -> MusynthTypes.LLDesigMap.key list
    val msToStr :
      (MusynthTypes.LLDesigMap.key -> string) ->
      int MusynthTypes.LLDesigMap.t -> string
    val addToMs :
      MusynthTypes.LLDesigMap.key ->
      int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
    val delFromMS :
      MusynthTypes.LLDesigMap.key ->
      int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t
    val makeEmptyMSWithAlphabet :
      MusynthTypes.LLDesigMap.key list -> int MusynthTypes.LLDesigMap.t
    val enumerateMS :
      MusynthTypes.LLDesigMap.key list -> int -> MSSet.elt list
    val enumerateLists : 'a list -> int -> 'a list list
    val listToStr : ('a -> string) -> 'a list -> string
    val nextuid : int ref
    val getuid : unit -> int
    val resetuid : unit -> unit
    val crossProduct : 'a list list -> 'a list list
    val identConstPairList2Map :
      (MusynthTypes.IdentMap.key * 'a) list -> 'a MusynthTypes.IdentMap.t
    val sDesigToIdent :
      MusynthTypes.musDesignatorT -> MusynthTypes.identifierT
    val conjoinPropOpts :
      MusynthTypes.musPropT option ->
      MusynthTypes.musPropT option -> MusynthTypes.musPropT option
    val mergeIdentMaps :
      'a MusynthTypes.IdentMap.t ->
      'a MusynthTypes.IdentMap.t -> 'a MusynthTypes.IdentMap.t
    val evalProp :
      MusynthTypes.musPropT -> ('a * 'b) MusynthTypes.IdentMap.t -> bool
    val getMapsForProp :
      MusynthTypes.IdentMap.key list ->
      MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
      MusynthTypes.musPropT option ->
      MusynthTypes.identifierT MusynthTypes.IdentMap.t list
  end
val lg : int -> int
val numBitsForValues : 'a list -> int
module IntMap :
  sig
    type key = int
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
val varMap :
  (int * int * MusynthTypes.StringMap.key IntMap.t *
   IntMap.key MusynthTypes.StringMap.t)
  MusynthTypes.StringMap.t ref
val numTotalBits : int ref
val bddMan : Cudd.Man.d Cudd.Man.t
val registerVar :
  MusynthTypes.StringMap.key -> MusynthTypes.StringMap.key list -> unit
val lookupVar :
  MusynthTypes.StringMap.key ->
  int * int * MusynthTypes.StringMap.key IntMap.t *
  IntMap.key MusynthTypes.StringMap.t
val registerVarAndPrimed :
  MusynthTypes.StringMap.key -> MusynthTypes.StringMap.key list -> unit
val mkBddForVal : int -> int -> int -> Cudd.Man.d Cudd.Bdd.t
val prop2BDD : MusynthTypes.musPropT -> Cudd.Man.d Cudd.Bdd.t
