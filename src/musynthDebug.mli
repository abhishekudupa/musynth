module Opts :
  sig
    val debugLevel : int ref
    val fairnessType : MusynthTypes.ltlFairnessT ref
    val onlySafety : bool ref
    val conjunctivePart : bool ref
    val inputFileName : string ref
  end
val debugOC : out_channel option ref
val debugFmt : Format.formatter option ref
val getDebugFmt : unit -> Format.formatter
val initDebugSubsys : string -> unit
val shutDownDebugSubsys : unit -> unit
val dprintf : int -> ('a, Format.formatter, unit) format -> 'a
val dflush : unit -> unit
