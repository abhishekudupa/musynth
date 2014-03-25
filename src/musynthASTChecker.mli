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
  (MusynthTypes.musSymTypeT * (string * MusynthTypes.sourcelocation option))
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
