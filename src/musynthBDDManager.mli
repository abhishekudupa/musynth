type bddType = Cudd.Man.d Cudd.Bdd.t

class bddManager :
object
  val mutable bitNameToBddMap : Cudd.Man.d Cudd.Bdd.t MusynthTypes.StringMap.t
  val mutable cachedAllButParamCube : Cudd.Man.d Cudd.Bdd.t option
  val mutable cachedAllVarCube : Cudd.Man.d Cudd.Bdd.t option
  val mutable cachedAllVarPrinter : (Format.formatter -> 
                                     Cudd.Man.tbool array -> 
                                     unit) option
  val mutable cachedConstraintsOnAllVars : Cudd.Man.d Cudd.Bdd.t option
  val mutable cachedConstraintsOnParams : Cudd.Man.d Cudd.Bdd.t option
  val mutable cachedCubePrinter : (Format.formatter -> 
                                   Cudd.Man.tbool array -> 
                                   unit) option
  val mutable cachedP2DPSubstTable : Cudd.Man.d Cudd.Bdd.t array option
  val mutable cachedP2USubstTable : Cudd.Man.d Cudd.Bdd.t array option
  val mutable cachedParamVarCube : Cudd.Man.d Cudd.Bdd.t option
  val mutable cachedParamVarPrinter : (Format.formatter -> 
                                       Cudd.Man.tbool array -> 
                                       unit) option

  val mutable cachedPrimedVarCube : Cudd.Man.d Cudd.Bdd.t option
  val mutable cachedStateVarPrinter : (Format.formatter -> 
                                       Cudd.Man.tbool array -> 
                                       unit) option

  val mutable cachedU2PSubstTable : Cudd.Man.d Cudd.Bdd.t array option
  val mutable cachedU2DPSubstTable : Cudd.Man.d Cudd.Bdd.t array option
  val mutable cachedUnprimedVarCube : Cudd.Man.d Cudd.Bdd.t option
  val mutable cachedVarCubes : Cudd.Man.d Cudd.Bdd.t MusynthTypes.LLDesigSetMap.t
  val mutable dPStateBitSet : MusynthTypes.IntSet.t
  val mutable indexToBitNameMap : string MusynthTypes.IntMap.t
  val mutable internalStateVars : MusynthTypes.LLDesigSet.t
  val mutable manager : Cudd.Man.d Cudd.Man.t
  val mutable numInternalStateBits : int
  val mutable numParamBits : int
  val mutable numStateBits : int
  val mutable numTotalBits : MusynthTypes.IntSet.elt
  val mutable pStateBitSet : MusynthTypes.IntSet.t
  val mutable paramBitSet : MusynthTypes.IntSet.t
  val mutable paramVars : MusynthTypes.LLDesigSet.t
  val mutable stateBitSet : MusynthTypes.IntSet.t
  val mutable stateVars : MusynthTypes.LLDesigMap.key MusynthTypes.LLDesigMap.t
  val mutable varMap : (MusynthTypes.LLDesigSet.elt list * 
                          MusynthTypes.IntSet.elt * int *
                            MusynthTypes.StringMap.key list *
                              Cudd.Man.d Cudd.Bdd.t MusynthTypes.LLDesigMap.t *
                                MusynthTypes.LLDesigMap.key MusynthTypes.IntMap.t *
                                  (Cudd.Man.tbool array -> 
                                   MusynthTypes.LLDesigSet.elt) *
                                    Cudd.Man.d Cudd.Bdd.t)
                         MusynthTypes.LLDesigMap.t

  method private checkVarReregister : MusynthTypes.LLDesigMap.key ->
                                      MusynthTypes.LLDesigSet.elt list ->
                                      MusynthTypes.LLDesigSet.elt list option

  method cubeOfMinTerm : Cudd.Man.tbool array -> Cudd.Man.d Cudd.Bdd.t

  method private determinizeOnSet : MusynthTypes.IntSet.t -> 
                                    Cudd.Man.tbool array -> 
                                    Cudd.Man.tbool array

  method disableAutoReorder : unit -> unit
  method enableAutoReorder : unit -> unit

  method getAllButParamCube : unit -> Cudd.Man.d Cudd.Bdd.t
  method getAllVarPrinter : unit -> Format.formatter -> Cudd.Man.tbool array -> unit
  method getBitPrinter : unit -> Format.formatter -> MusynthTypes.IntMap.key -> unit
  method getConstraintsOnAllVars : unit -> Cudd.Man.d Cudd.Bdd.t
  method getConstraintsOnParams : unit -> Cudd.Man.d Cudd.Bdd.t
  method getCubeForAllVars : unit -> Cudd.Man.d Cudd.Bdd.t
  method private getCubeForOneVar : MusynthTypes.LLDesigMap.key -> 
                                    Cudd.Man.d Cudd.Bdd.t
                                               
  method getCubeForParamVars : unit -> Cudd.Man.d Cudd.Bdd.t
  method getCubeForPrimedVars : unit -> Cudd.Man.d Cudd.Bdd.t
  method getCubeForDPrimedVars : unit -> Cudd.Man.d Cudd.Bdd.t
  method getCubeForUnprimedVars : unit -> Cudd.Man.d Cudd.Bdd.t
  method getCubeForVar : MusynthTypes.LLDesigSet.elt -> Cudd.Man.d Cudd.Bdd.t
  method getCubeForVars : MusynthTypes.LLDesigSet.elt list -> Cudd.Man.d Cudd.Bdd.t
  method getCubePrinter : unit -> Format.formatter -> Cudd.Man.tbool array -> unit
  method getNParamVars : int -> Cudd.Man.d Cudd.Bdd.t ->
                         MusynthTypes.LLDesigSet.elt MusynthTypes.LLDesigMap.t list

  method getNStateVars : int -> Cudd.Man.d Cudd.Bdd.t ->
                         MusynthTypes.LLDesigSet.elt MusynthTypes.LLDesigMap.t list

  method getNumInternalStateBits : unit -> int
  method getNumMinTerms : Cudd.Man.d Cudd.Bdd.t -> float
  method getNumMinTermsParam : Cudd.Man.d Cudd.Bdd.t -> float
  method getNumMinTermsState : Cudd.Man.d Cudd.Bdd.t -> float
  method getNumMinTermsStateNI : Cudd.Man.d Cudd.Bdd.t -> float
  method getNumParamBits : unit -> int
  method getNumParamVars : unit -> int
  method getNumStateBits : unit -> int
  method getNumTotalBits : unit -> MusynthTypes.IntSet.elt
  method getParamVarNames : unit -> MusynthTypes.LLDesigSet.elt list
  method getParamVarPrinter : unit -> Format.formatter -> Cudd.Man.tbool array -> unit
  method getParamVars : Cudd.Man.d Cudd.Bdd.t -> MusynthTypes.LLDesigSet.elt 
                                                   MusynthTypes.LLDesigMap.t
  method getPeakBDDSize : unit -> int
  method getStateVarNames : unit -> MusynthTypes.LLDesigMap.key list
  method getStateVarNamesNI : unit -> MusynthTypes.LLDesigMap.key list
  method getStateVarPrinter : unit -> Format.formatter -> Cudd.Man.tbool array -> unit
  method getStateVars : Cudd.Man.d Cudd.Bdd.t ->
                        MusynthTypes.LLDesigSet.elt MusynthTypes.LLDesigMap.t

  method getSubstTableP2U : unit -> Cudd.Man.d Cudd.Bdd.t array
  method getSubstTableU2P : unit -> Cudd.Man.d Cudd.Bdd.t array
  method getSubstTableP2DP : unit -> Cudd.Man.d Cudd.Bdd.t array
  method getSubstTableU2DP : unit -> Cudd.Man.d Cudd.Bdd.t array
  method private invalidateCaches : unit -> unit
  method isFalse : Cudd.Man.d Cudd.Bdd.t -> bool
  method private lg : int -> int
  method lookupVar : MusynthTypes.LLDesigMap.key ->
                     (MusynthTypes.LLDesigSet.elt list * 
                        MusynthTypes.IntSet.elt * int *
                          MusynthTypes.StringMap.key list *
                            Cudd.Man.d Cudd.Bdd.t MusynthTypes.LLDesigMap.t *
                              MusynthTypes.LLDesigMap.key MusynthTypes.IntMap.t *
                                (Cudd.Man.tbool array -> MusynthTypes.LLDesigSet.elt) *
                                  Cudd.Man.d Cudd.Bdd.t) option
                                                         
  method private makeBDDForRepr : MusynthTypes.IntSet.elt ->
                                  int -> MusynthTypes.IntMap.key -> 
                                  Cudd.Man.d Cudd.Bdd.t

  method makeFalse : unit -> Cudd.Man.d Cudd.Bdd.t
  method makeTrue : unit -> Cudd.Man.d Cudd.Bdd.t
  method minimize : unit -> unit
  method reorder : int -> unit
  method pickMinTermOnPStates : Cudd.Man.d Cudd.Bdd.t -> Cudd.Man.tbool array
  method pickMinTermOnParams : Cudd.Man.d Cudd.Bdd.t -> Cudd.Man.tbool array
  method pickMinTermOnStates : Cudd.Man.d Cudd.Bdd.t -> Cudd.Man.tbool array
  method printCubes : int -> Format.formatter -> Cudd.Man.d Cudd.Bdd.t -> unit
  method printParamVars : int -> Format.formatter -> Cudd.Man.d Cudd.Bdd.t -> unit
  method printStateVars : int -> Format.formatter -> Cudd.Man.d Cudd.Bdd.t -> unit
  method prop2BDD : MusynthTypes.llPropT -> Cudd.Man.d Cudd.Bdd.t
  method private registerBits : MusynthTypes.LLDesigMap.key -> int -> 
                                MusynthTypes.IntSet.elt * 
                                  MusynthTypes.StringMap.key list

  method private registerBitsForVar : MusynthTypes.IntSet.elt -> int -> 
                                      MusynthTypes.IntSet.t

  method registerInternalStateVariable : MusynthTypes.LLDesigMap.key -> 
                                         MusynthTypes.LLDesigSet.elt list -> unit

  method registerParamVariable : MusynthTypes.LLDesigMap.key -> 
                                 MusynthTypes.LLDesigSet.elt list -> unit

  method registerStateVariable : MusynthTypes.LLDesigMap.key -> 
                                 MusynthTypes.LLDesigSet.elt list -> unit

  method registerVar : MusynthTypes.LLDesigMap.key -> 
                       MusynthTypes.LLDesigSet.elt list -> unit

  method reset : unit -> unit
  method private substOneVarInTable : Cudd.Man.d Cudd.Bdd.t array ->
                                      MusynthTypes.LLDesigMap.key -> 
                                      MusynthTypes.LLDesigMap.key -> unit
end
