val encodeProg : MusynthBDDManager.bddManager ->
                 MusynthTypes.llProgT ->
                 MusynthBDDManager.bddType  MusynthTypes.LLDesigMap.t * 
                   MusynthBDDManager.bddType * MusynthBDDManager.bddType * 
                     MusynthBDDManager.bddType *
                       ((MusynthTypes.llDesignatorT MusynthTypes.PropMap.t *
                           MusynthTypes.llPropT MusynthTypes.LLDesigMap.t *
                             MusynthTypes.llPropT MusynthTypes.PropMap.t * 
                               MusynthBDDManager.bddType * MusynthBDDManager.bddType * 
                                 ((MusynthBDDManager.bddType, MusynthBDDManager.bddType list) 
                                    MusynthTypes.fairnessSpecT) list * 
                                   ((MusynthBDDManager.bddType, MusynthBDDManager.bddType list) 
                                      MusynthTypes.fairnessSpecT) list)
                          MusynthTypes.StringMap.t) *
                         MusynthBDDManager.bddType
