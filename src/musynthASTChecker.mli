module ST :
  sig
    val createSymTab : unit -> 'a MusynthTypes.IdentMap.t ref list ref
    val push : MusynthTypes.symTableT -> unit
    val pushScope :
      MusynthTypes.symTableT -> MusynthTypes.symTabScope -> unit
    val pop : MusynthTypes.symTableT -> MusynthTypes.symTabScope
    val peek : MusynthTypes.symTableT -> MusynthTypes.symTabScope
    val getNumScopes : MusynthTypes.symTableT -> int
    val lookup :
      MusynthTypes.symTableT ->
      MusynthTypes.identifierT -> MusynthTypes.symtabEntry option
    val bind :
      MusynthTypes.symTableT ->
      MusynthTypes.identifierT -> MusynthTypes.symtabEntry -> unit
    val lookupOrFail :
      MusynthTypes.symTableT ->
      MusynthTypes.identifierT -> MusynthTypes.symtabEntry
    val lookupVar :
      MusynthTypes.symTableT ->
      MusynthTypes.identifierT -> MusynthTypes.musSymTypeT
    val lookupType :
      MusynthTypes.symTableT ->
      MusynthTypes.identifierT -> MusynthTypes.musSymTypeT
  end
val resolveSymType :
  MusynthTypes.symTableT ->
  MusynthTypes.musSymTypeT -> MusynthTypes.musSymTypeT
val checkSymTypeDecl :
  MusynthTypes.symTableT ->
  MusynthTypes.identifierT * MusynthTypes.musSymTypeT -> unit
val destructDesigDecl :
  MusynthTypes.musDesignatorT ->
  MusynthTypes.identifierT * MusynthTypes.identifierT list
val getTypeListForIdent :
  MusynthTypes.symTableT ->
  MusynthTypes.identifierT -> MusynthTypes.musSymTypeT list
val checkTypeLists :
  MusynthTypes.musSymTypeT list ->
  (MusynthTypes.musSymTypeT * (string * MusynthTypes.sourcelocation option))
  list -> unit
val checkDesigRVal :
  MusynthTypes.symTableT -> MusynthTypes.musDesignatorT -> unit
val checkPureProp : MusynthTypes.symTableT -> MusynthTypes.musPropT -> unit
val checkDesigDecl :
  MusynthTypes.symTableT ->
  MusynthTypes.musDesignatorT ->
  MusynthTypes.identifierT * MusynthTypes.musSymTypeT list option
val checkTransDecl :
  MusynthTypes.symTableT ->
  MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
  MusynthTypes.musDesignatorT -> unit
val checkDecl :
  MusynthTypes.symTableT ->
  (MusynthTypes.symTableT -> 'a -> 'b) -> 'a MusynthTypes.musDeclType -> 'b
val checkStateDecl :
  MusynthTypes.symTableT ->
  MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
val checkMsgDecl :
  MusynthTypes.symTableT ->
  MusynthTypes.msgType ->
  MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
