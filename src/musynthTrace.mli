val printState : Format.formatter ->
                 MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t -> unit

val printTraceFullSafety : Format.formatter ->
                           MusynthTypes.llDesignatorT 
                             MusynthTypes.LLDesigMap.t list -> unit
val computeDiff : 'a MusynthTypes.LLDesigMap.t ->
                  'a MusynthTypes.LLDesigMap.t -> 
                  'a MusynthTypes.LLDesigMap.t

val printTraceDiffSafety : Format.formatter ->
                           MusynthTypes.llDesignatorT 
                             MusynthTypes.LLDesigMap.t list -> unit

val printTraceDiffLiveness : Format.formatter ->
                             MusynthTypes.llDesignatorT 
                               MusynthTypes.LLDesigMap.t list ->
                             MusynthTypes.llDesignatorT 
                               MusynthTypes.LLDesigMap.t list -> unit

val printTraceFullLiveness : Format.formatter ->
                             MusynthTypes.llDesignatorT 
                               MusynthTypes.LLDesigMap.t list ->
                             MusynthTypes.llDesignatorT 
                               MusynthTypes.LLDesigMap.t list -> unit

val printTraceSafety : Format.formatter -> MusynthTypes.llDesignatorT 
                                             MusynthTypes.LLDesigMap.t list -> unit
val printTraceLiveness : Format.formatter ->
                         MusynthTypes.llDesignatorT 
                           MusynthTypes.LLDesigMap.t list ->
                         MusynthTypes.llDesignatorT 
                           MusynthTypes.LLDesigMap.t list -> unit
