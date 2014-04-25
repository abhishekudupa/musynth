module Opts :
  sig
    val debugDisabled : bool ref
    val debugOptions : MusynthTypes.StringSet.t ref
    val debugFileName : string ref
    val onlySafety : bool ref
    val conjunctivePart : bool ref
    val inputFileName : string ref
    val numSolsRequested : int ref
    val reorderEnabled : bool ref
    val reorderMethod : Cudd.Man.reorder ref
    val reorderMethods : string list
  end
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
