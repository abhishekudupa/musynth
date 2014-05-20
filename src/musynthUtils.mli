module MSSet : Set.S with type elt = int MusynthTypes.LLDesigMap.t

val splatList : 'a -> int -> 'a list
val makeEmptyMS : unit -> 'a MusynthTypes.LLDesigMap.t
val msToList : int MusynthTypes.LLDesigMap.t -> MusynthTypes.LLDesigMap.key list
val msToStr : (MusynthTypes.LLDesigMap.key -> string) ->
              int MusynthTypes.LLDesigMap.t -> string

val addToMs : MusynthTypes.LLDesigMap.key ->
              int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t

val delFromMS : MusynthTypes.LLDesigMap.key ->
                int MusynthTypes.LLDesigMap.t -> int MusynthTypes.LLDesigMap.t

val makeEmptyMSWithAlphabet : MusynthTypes.LLDesigMap.key list -> 
                              int MusynthTypes.LLDesigMap.t

val enumerateMS : MusynthTypes.LLDesigMap.key list -> int -> MSSet.elt list

val enumerateLists : 'a list -> int -> 'a list list

val listToStr : ('a -> string) -> 'a list -> string

val getuid : unit -> int

val resetuid : unit -> unit

val getSynthVarUID : unit -> string

val crossProduct : 'a list list -> 'a list list

val identConstPairList2Map : (MusynthTypes.IdentMap.key * 'a) list -> 
                             'a MusynthTypes.IdentMap.t

val sDesigToIdent : MusynthTypes.musDesignatorT -> MusynthTypes.identifierT

val conjoinPropOpts : MusynthTypes.musPropT option ->
                      MusynthTypes.musPropT option -> MusynthTypes.musPropT option

val mergeIdentMaps : 'a MusynthTypes.IdentMap.t ->
                     'a MusynthTypes.IdentMap.t -> 'a MusynthTypes.IdentMap.t

val evalProp :
  MusynthTypes.musPropT -> ('a * 'b) MusynthTypes.IdentMap.t -> bool

val getMapsForProp : MusynthTypes.IdentMap.key list ->
                     MusynthTypes.musSymTypeT MusynthTypes.IdentMap.t ->
                     MusynthTypes.musPropT option ->
                     MusynthTypes.identifierT MusynthTypes.IdentMap.t list

val makeTrueDesig : unit -> MusynthTypes.llDesignatorT
val makeFalseDesig : unit -> MusynthTypes.llDesignatorT
val makeDeferDesig : unit -> MusynthTypes.llDesignatorT

val getMsgsForAut : MusynthTypes.llAutomatonT ->
                    MusynthTypes.llIdentT list * MusynthTypes.llIdentT list

val getNameForAut : MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT

val getTransitionsForAut : MusynthTypes.llAutomatonT -> MusynthTypes.llTransT list
val getStatesForAut : MusynthTypes.llAutomatonT -> MusynthTypes.llIdentT list

val getAutomatonByName : MusynthTypes.llAutomatonT list ->
                         MusynthTypes.llIdentT -> MusynthTypes.llAutomatonT

val getFairnessForAutomaton : MusynthTypes.llAutomatonT -> MusynthTypes.llFairnessT

val getLFairnessForAutomaton : MusynthTypes.llAutomatonT -> 
                               MusynthTypes.llLossFairnessT

val getDFairnessForAutomaton : MusynthTypes.llAutomatonT -> 
                               MusynthTypes.llDupFairnessT

val getSender : MusynthTypes.llIdentT ->
                MusynthTypes.llAutomatonT list -> MusynthTypes.llAutomatonT

val getReceivers : MusynthTypes.llIdentT ->
                   MusynthTypes.llAutomatonT list -> MusynthTypes.llAutomatonT list

val getStateNameForAutomaton : MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT
val getStateNamePForAutomaton : MusynthTypes.llAutomatonT -> MusynthTypes.llDesignatorT

val getCSPredsForMsg : MusynthTypes.llIdentT -> 
                       MusynthTypes.llAutomatonT -> MusynthTypes.llPropT

val getCSPredsForMsgAll : MusynthTypes.llIdentT ->
                          MusynthTypes.llAutomatonT list -> MusynthTypes.llPropT

val getMsgsToSyncOnFromState : MusynthTypes.llAutomatonT ->
                               MusynthTypes.llIdentT -> 
                               MusynthTypes.LLDesigSet.elt list

val getStatesFromWhichMsgSync : MusynthTypes.llAutomatonT ->
                                MusynthTypes.llIdentT -> 
                                MusynthTypes.LLDesigSet.elt list

val canonicalizeProp : MusynthTypes.llPropT -> MusynthTypes.llPropT
val canonicalizePropFP : MusynthTypes.llPropT -> MusynthTypes.llPropT
val makeFormatterOfName : string -> out_channel * Format.formatter
val makeConjunction : MusynthTypes.llPropT list -> MusynthTypes.llPropT
val makeDisjunction : MusynthTypes.llPropT list -> MusynthTypes.llPropT
