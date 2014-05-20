val constructFairnessSpecs : MusynthTypes.llPropT MusynthTypes.LLDesigMap.t ->
                             'a * MusynthTypes.llAutomatonT list * 'b * 'c * 'd ->
                             ((MusynthTypes.llPropT, MusynthTypes.llPropT list) 
                                MusynthTypes.fairnessSpecT) list

val constructTableau : MusynthTypes.llPropT ->
                       MusynthTypes.llDesignatorT MusynthTypes.PropMap.t *
                         MusynthTypes.PropMap.key MusynthTypes.LLDesigMap.t *
                           MusynthTypes.PropMap.key MusynthTypes.PropMap.t *
                             MusynthTypes.PropMap.key * MusynthTypes.llPropT *
                               ((MusynthTypes.llPropT, 'a) 
                                  MusynthTypes.fairnessSpecT) list
