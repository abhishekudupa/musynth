module Util : sig val crossProduct : 'a list list -> 'a list list end
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
      MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT) list
      MusynthTypes.musDeclType list * MusynthTypes.musSpecT list -> unit
  end
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
    val lookupMsg :
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
        val lookupMsg :
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
    val getTypeListForIdent :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.IdentMap.key -> MusynthTypes.musSymTypeT list
    val checkTypeLists :
      MusynthTypes.musSymTypeT list ->
      (MusynthTypes.musSymTypeT *
       (string * MusynthTypes.sourcelocation option))
      list -> unit
    val getRValType :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT -> MusynthTypes.symtabEntry
    val checkTypeCompatibility :
      MusynthTypes.symtabEntry ->
      MusynthTypes.symtabEntry -> MusynthTypes.sourcelocation option -> unit
    val checkPureProp :
      MusynthTypes.symTabScope list ref -> MusynthTypes.musPropT -> unit
    val checkPureQProp :
      MusynthTypes.symTabScope list ref -> MusynthTypes.musPropT -> unit
    val checkDesigDecl :
      MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
      MusynthTypes.musDesignatorT ->
      'a -> MusynthTypes.identifierT * MusynthTypes.musSymTypeT list
    val checkTransDecl :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
      MusynthTypes.musDesignatorT ->
      MusynthTypes.sourcelocation option -> string * 'a list
    val checkDecl :
      MusynthTypes.symTabScope list ref ->
      (MusynthTypes.symTabScope list ref ->
       'a -> MusynthTypes.sourcelocation option -> 'b * 'c) ->
      'a MusynthTypes.musDeclType ->
      'b * 'c * MusynthTypes.musPropT option * MusynthTypes.symTabScope
    val checkStateDecl :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
    val checkMsgDecl :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.msgType ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
    val checkStateDeclBlock :
      MusynthTypes.symTabScope list ref ->
      (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
       MusynthTypes.musStateAnnotationT)
      list -> bool -> unit
    val checkAutDef :
      MusynthTypes.symTabScope list ref ->
      MusynthTypes.musAutomatonDeclType ->
      'a -> MusynthTypes.identifierT * MusynthTypes.musSymTypeT list
    val checkInitStateDecl :
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
      MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT) list
      MusynthTypes.musDeclType list * MusynthTypes.musSpecT list -> unit
  end
val pLLIdent : Format.formatter -> string * string list -> unit
val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
val pLLState :
  Format.formatter -> (string * string list) * MusynthTypes.llAnnotT -> unit
val pLLCompleteTrans :
  Format.formatter ->
  (string * string list) * (string * string list) * (string * string list) ->
  unit
val pLLParametricTrans :
  Format.formatter ->
  (string * string list) * (string * string list) * MusynthTypes.LLIdentSet.t ->
  unit
val pLLAut : Format.formatter -> MusynthTypes.llAutomatonT -> unit
val evalProp :
  MusynthTypes.musPropT option -> 'a MusynthTypes.StringMap.t -> bool
val filterMaps :
  'a MusynthTypes.StringMap.t list ->
  MusynthTypes.musPropT option -> 'a MusynthTypes.StringMap.t list
val getMaps :
  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
  MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
  MusynthTypes.musPropT option -> string MusynthTypes.StringMap.t list
val mapArgs :
  'a ->
  'b MusynthTypes.StringMap.t list ->
  (MusynthTypes.StringMap.key * 'c) list -> ('a * 'b list) list
val lowerDesigDecl :
  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
  MusynthTypes.musDesignatorT ->
  MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
  MusynthTypes.musPropT option -> (string * string list) list
val lowerCompleteTransDecl :
  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
  MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
  MusynthTypes.musDesignatorT ->
  MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
  MusynthTypes.musPropT option ->
  ((string * string list) * (string * string list) * (string * string list))
  list
val lowerDecl :
  'a ->
  ('a ->
   'b ->
   MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
   MusynthTypes.musPropT option -> 'c) ->
  'b MusynthTypes.musDeclType -> 'c
val lowerStateAnnotation :
  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
  MusynthTypes.musStateAnnotationT -> MusynthTypes.llAnnotT
val lowerStateDecl :
  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
  MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
  MusynthTypes.musStateAnnotationT ->
  ((string * string list) * MusynthTypes.llAnnotT) list
val lowerStateDeclBlock :
  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
  (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
   MusynthTypes.musStateAnnotationT)
  list -> ((string * string list) * MusynthTypes.llAnnotT) list
val lowerMsgDecl :
  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
  MusynthTypes.musDesignatorT MusynthTypes.musDeclType ->
  (string * string list) list
val lowerMsgDeclBlock :
  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
  MusynthTypes.musDesignatorT MusynthTypes.musDeclType list ->
  (string * string list) list
val lowerCompleteTransDeclBlock :
  MusynthTypes.symtabEntry MusynthTypes.IdentMap.t ref list ref ->
  (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
   MusynthTypes.musDesignatorT)
  MusynthTypes.musDeclType list ->
  ((string * string list) * (string * string list) * (string * string list))
  list
