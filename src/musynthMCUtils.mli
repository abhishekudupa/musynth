val getTransRel : MusynthBDDManager.bddManager ->
                  MusynthBDDManager.bddType MusynthTypes.LLDesigMap.t ->
                  MusynthBDDManager.bddType list

val addTableauTransRel : MusynthBDDManager.bddType list ->
                         MusynthBDDManager.bddType ->
                         MusynthBDDManager.bddType list

val restrictTransRelToStates : MusynthBDDManager.bddType list ->
                               MusynthBDDManager.bddType ->
                               MusynthBDDManager.bddType list

val composeTransRel : MusynthBDDManager.bddManager ->
                      MusynthBDDManager.bddType list ->
                      MusynthBDDManager.bddType

val postPrime : MusynthBDDManager.bddManager ->
                MusynthBDDManager.bddType list -> 
                MusynthBDDManager.bddType -> 
                MusynthBDDManager.bddType

val prePrime : MusynthBDDManager.bddManager ->
               MusynthBDDManager.bddType list -> 
               MusynthBDDManager.bddType -> 
               MusynthBDDManager.bddType

val primeSet : MusynthBDDManager.bddManager ->
               MusynthBDDManager.bddType -> 
               MusynthBDDManager.bddType
                   
val unprimeSet : MusynthBDDManager.bddManager ->
                 MusynthBDDManager.bddType -> 
                 MusynthBDDManager.bddType

val pre : MusynthBDDManager.bddManager ->
          MusynthBDDManager.bddType list -> 
          MusynthBDDManager.bddType -> 
          MusynthBDDManager.bddType

val post : MusynthBDDManager.bddManager ->
           MusynthBDDManager.bddType list -> 
           MusynthBDDManager.bddType -> 
           MusynthBDDManager.bddType

val restrictedPre : MusynthBDDManager.bddManager ->
                    MusynthBDDManager.bddType list ->
                    MusynthBDDManager.bddType ->
                    MusynthBDDManager.bddType ->
                    MusynthBDDManager.bddType

val restrictedPost : MusynthBDDManager.bddManager ->
                     MusynthBDDManager.bddType list ->
                     MusynthBDDManager.bddType ->
                     MusynthBDDManager.bddType ->
                     MusynthBDDManager.bddType

val inclusionFixPointTester : MusynthBDDManager.bddType -> 
                              MusynthBDDManager.bddType -> bool

val eqFixPointTester : MusynthBDDManager.bddType -> MusynthBDDManager.bddType -> bool

val postAndTransformer : MusynthBDDManager.bddManager ->
                         MusynthBDDManager.bddType list -> 
                         MusynthBDDManager.bddType -> 
                         MusynthBDDManager.bddType

val postOrTransformer : MusynthBDDManager.bddManager->
                        MusynthBDDManager.bddType list -> 
                        MusynthBDDManager.bddType -> 
                        MusynthBDDManager.bddType

val preAndTransformer : MusynthBDDManager.bddManager ->
                        MusynthBDDManager.bddType list -> 
                        MusynthBDDManager.bddType -> 
                        MusynthBDDManager.bddType

val preOrTransformer : MusynthBDDManager.bddManager ->
                       MusynthBDDManager.bddType list -> 
                       MusynthBDDManager.bddType -> 
                       MusynthBDDManager.bddType

val iterativeSquarer : MusynthBDDManager.bddManager ->
                       MusynthBDDManager.bddType list ->
                       MusynthBDDManager.bddType list

val postK : int -> MusynthBDDManager.bddManager ->
            MusynthBDDManager.bddType list -> 
            MusynthBDDManager.bddType -> 
            MusynthBDDManager.bddType MusynthTypes.execExitStatT

val prunedPostK : int -> MusynthBDDManager.bddManager ->
                  MusynthBDDManager.bddType list -> 
                  MusynthBDDManager.bddType -> 
                  MusynthBDDManager.bddType ->
                  (MusynthBDDManager.bddType * 
                     MusynthBDDManager.bddType) 
                    MusynthTypes.execExitStatT

val preK : int -> MusynthBDDManager.bddManager ->
           MusynthBDDManager.bddType list -> MusynthBDDManager.bddType -> 
           MusynthBDDManager.bddType MusynthTypes.execExitStatT

val computeFixPoint : ('a -> 'a) -> ('a -> 'a -> bool) -> 'a -> 'a
                                                                  
val findPathCube : MusynthBDDManager.bddManager ->
                   MusynthBDDManager.bddType -> 
                   MusynthBDDManager.bddType list -> 
                   MusynthBDDManager.bddType -> 
                   MusynthBDDManager.bddType list

val findPath : MusynthBDDManager.bddManager ->
               MusynthBDDManager.bddType -> 
               MusynthBDDManager.bddType list -> 
               MusynthBDDManager.bddType -> 
               MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list

val findLoopCube : MusynthBDDManager.bddManager ->
                   MusynthBDDManager.bddType ->
                   MusynthBDDManager.bddType list ->
                   MusynthBDDManager.bddType ->
                   MusynthBDDManager.bddType ->
                   ((MusynthBDDManager.bddType, 
                     MusynthBDDManager.bddType list) MusynthTypes.fairnessSpecT) list ->
                   ((MusynthBDDManager.bddType, 
                     MusynthBDDManager.bddType list) MusynthTypes.fairnessSpecT) list ->
                   MusynthBDDManager.bddType list * MusynthBDDManager.bddType list

val findLoop : MusynthBDDManager.bddManager ->
               MusynthBDDManager.bddType ->
               MusynthBDDManager.bddType list ->
               MusynthBDDManager.bddType ->
               MusynthBDDManager.bddType ->
               ((MusynthBDDManager.bddType, 
                 MusynthBDDManager.bddType list) MusynthTypes.fairnessSpecT) list ->
               ((MusynthBDDManager.bddType, 
                 MusynthBDDManager.bddType list) MusynthTypes.fairnessSpecT) list ->
               (MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list * 
                  MusynthTypes.llDesignatorT MusynthTypes.LLDesigMap.t list)
