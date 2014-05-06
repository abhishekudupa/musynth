val encodeProg : MusynthBDDManager.bddManager ->
                 MusynthTypes.llProgT ->
                 MusynthBDDManager.bddType  MusynthTypes.LLDesigMap.t * 
                   MusynthBDDManager.bddType * MusynthBDDManager.bddType * 
                     MusynthBDDManager.bddType *
                       (MusynthTypes.llDesignatorT MusynthTypes.PropMap.t *
                          MusynthTypes.PropMap.key MusynthTypes.LLDesigMap.t *
                            MusynthTypes.PropMap.key MusynthTypes.PropMap.t * 
                              MusynthBDDManager.bddType * MusynthBDDManager.bddType * 
                                MusynthBDDManager.bddType list * 
                                  (MusynthBDDManager.bddType * 
                                     MusynthBDDManager.bddType) list)
                         MusynthTypes.StringMap.t * 
                         MusynthBDDManager.bddType
