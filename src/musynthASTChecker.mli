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
