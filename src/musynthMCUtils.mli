val post : MusynthBDDManager.bddManager ->
           MusynthBDDManager.bddType -> 
           MusynthBDDManager.bddType -> 
           MusynthBDDManager.bddType

val pre : MusynthBDDManager.bddManager ->
          MusynthBDDManager.bddType -> 
          MusynthBDDManager.bddType -> 
          MusynthBDDManager.bddType

val inclusionFixPointTester : MusynthBDDManager.bddType -> 
                              MusynthBDDManager.bddType -> bool

val eqFixPointTester : MusynthBDDManager.bddType -> MusynthBDDManager.bddType -> bool

val postAndTransformer : MusynthBDDManager.bddManager ->
                         MusynthBDDManager.bddType -> 
                         MusynthBDDManager.bddType -> 
                         MusynthBDDManager.bddType

val postOrTransformer : MusynthBDDManager.bddManager->
                        MusynthBDDManager.bddType -> 
                        MusynthBDDManager.bddType -> 
                        MusynthBDDManager.bddType

val preAndTransformer : MusynthBDDManager.bddManager ->
                        MusynthBDDManager.bddType -> 
                        MusynthBDDManager.bddType -> 
                        MusynthBDDManager.bddType

val preOrTransformer : MusynthBDDManager.bddManager ->
                       MusynthBDDManager.bddType -> 
                       MusynthBDDManager.bddType -> 
                       MusynthBDDManager.bddType

val postK : int -> MusynthBDDManager.bddManager ->
            MusynthBDDManager.bddType -> 
            MusynthBDDManager.bddType -> 
            MusynthBDDManager.bddType MusynthTypes.execExitStatT

val prunedPostK : int -> MusynthBDDManager.bddManager ->
                  MusynthBDDManager.bddType -> 
                  MusynthBDDManager.bddType -> 
                  MusynthBDDManager.bddType ->
                  (MusynthBDDManager.bddType * MusynthBDDManager.bddType) 
                    MusynthTypes.execExitStatT

val preK :
  int -> MusynthBDDManager.bddManager ->
  MusynthBDDManager.bddType -> MusynthBDDManager.bddType -> 
  MusynthBDDManager.bddType MusynthTypes.execExitStatT

val computeFixPoint : ('a -> 'a) -> ('a -> 'a -> bool) -> 'a -> 'a
                                                                  
val findPathCube : MusynthBDDManager.bddManager ->
                   MusynthBDDManager.bddType -> 
                   MusynthBDDManager.bddType -> 
                   MusynthBDDManager.bddType -> 
                   MusynthBDDManager.bddType list

val findPath : MusynthBDDManager.bddManager ->
               MusynthBDDManager.bddType -> 
               MusynthBDDManager.bddType -> 
               MusynthBDDManager.bddType -> 
               MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list

val findLoopCube : MusynthBDDManager.bddManager ->
                   MusynthBDDManager.bddType ->
                   MusynthBDDManager.bddType ->
                   MusynthBDDManager.bddType ->
                   MusynthBDDManager.bddType list ->
                   (MusynthBDDManager.bddType * MusynthBDDManager.bddType) list ->
                   MusynthBDDManager.bddType list * MusynthBDDManager.bddType list

val findLoop : MusynthBDDManager.bddManager ->
               MusynthBDDManager.bddType ->
               MusynthBDDManager.bddType ->
               MusynthBDDManager.bddType ->
               MusynthBDDManager.bddType list ->
               (MusynthBDDManager.bddType * MusynthBDDManager.bddType) list -> 
               (MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list * 
                  MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list)
