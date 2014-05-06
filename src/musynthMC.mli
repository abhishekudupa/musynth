val synthFrontEnd :
  MusynthBDDManager.bddManager ->
  MusynthBDDManager.bddType MusynthTypes.LLDesigMap.t ->
  MusynthBDDManager.bddType ->
  MusynthBDDManager.bddType ->
  MusynthBDDManager.bddType ->
  ('c * 'd * 'e * MusynthBDDManager.bddType * 
     MusynthBDDManager.bddType * MusynthBDDManager.bddType list *
       (MusynthBDDManager.bddType * MusynthBDDManager.bddType) list)
    MusynthTypes.StringMap.t -> MusynthBDDManager.bddType -> MusynthBDDManager.bddType

val check :
  MusynthBDDManager.bddManager ->
  MusynthBDDManager.bddType MusynthTypes.LLDesigMap.t ->
  MusynthBDDManager.bddType ->
  MusynthBDDManager.bddType ->
  MusynthBDDManager.bddType ->
  ('b * 'c * 'd * MusynthBDDManager.bddType * 
     MusynthBDDManager.bddType * MusynthBDDManager.bddType list *
       (MusynthBDDManager.bddType * MusynthBDDManager.bddType) list)
  MusynthTypes.StringMap.t -> 
  MusynthBDDManager.bddType MusynthTypes.modelCheckingStatusT
