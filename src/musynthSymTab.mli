val createSymTab : unit -> 'a MusynthTypes.IdentMap.t ref list ref
val push : MusynthTypes.symTableT -> unit
val pushScope : MusynthTypes.symTableT -> MusynthTypes.symTabScope -> unit
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
