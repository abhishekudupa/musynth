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
  'a MusynthTypes.IdentMap.t ref list ref -> MusynthTypes.IdentMap.key -> 'a
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
