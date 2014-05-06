val constructFairnessSpecs : 'a * MusynthTypes.llAutomatonT list * 'b * 'c * 'd ->
                             MusynthTypes.fairnessSpecT list

val constructTableau : MusynthTypes.llPropT ->
                       MusynthTypes.llDesignatorT MusynthTypes.PropMap.t *
                         MusynthTypes.PropMap.key MusynthTypes.LLDesigMap.t *
                           MusynthTypes.PropMap.key MusynthTypes.PropMap.t *
                             MusynthTypes.PropMap.key * MusynthTypes.llPropT *
                               MusynthTypes.fairnessSpecT list
