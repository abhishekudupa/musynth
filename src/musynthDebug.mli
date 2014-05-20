val debugOC : out_channel option ref
val debugFmt : Format.formatter option ref
val debugEnabled : unit -> bool
val debugOptEnabled : MusynthTypes.StringSet.elt -> bool
val getDebugFmt : unit -> Format.formatter
val initDebugSubsys : string -> unit
val shutDownDebugSubsys : unit -> unit
val dprintf :
  MusynthTypes.StringSet.elt -> ('a, Format.formatter, unit) format -> 'a
val dflush : unit -> unit
