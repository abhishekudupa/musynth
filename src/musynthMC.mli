module AST :
  sig
    val pLoc : Format.formatter -> int * int * int * int -> unit
    val pLocOpt : Format.formatter -> (int * int * int * int) option -> unit
    val pIdentifier : Format.formatter -> string * 'a -> unit
    val identToName : 'a * 'b -> 'a
    val astToString : (Format.formatter -> 'a -> 'b) -> 'a -> string
    val pList :
      string ->
      bool ->
      bool ->
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
    val pSymType : Format.formatter -> MusynthTypes.musSymTypeT -> unit
    val pSymTypeDecl :
      Format.formatter -> (string * 'a) * MusynthTypes.musSymTypeT -> unit
    val musMakeIndentedBox :
      Format.formatter ->
      (Format.formatter -> 'a -> unit) ->
      'a ->
      (Format.formatter -> 'b -> unit) ->
      'b -> (Format.formatter -> 'c -> unit) -> 'c -> unit
    val pSymTypeDeclBlock :
      Format.formatter ->
      ((string * 'a) * MusynthTypes.musSymTypeT) list -> unit
    val pDesignator : Format.formatter -> MusynthTypes.musDesignatorT -> unit
    val pProp : Format.formatter -> MusynthTypes.musPropT -> unit
    val pPropOpt : Format.formatter -> MusynthTypes.musPropT option -> unit
    val pDecl :
      (Format.formatter -> 'a -> 'b) ->
      (Format.formatter -> 'a -> 'c) ->
      Format.formatter -> 'a MusynthTypes.musDeclType -> 'c
    val noopPrinter : 'a -> 'b -> unit
    val pMsgDecl :
      Format.formatter ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType -> unit
    val pMsgDeclBlock :
      string ->
      Format.formatter ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
    val pMessagesDeclBlock :
      Format.formatter ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list -> unit
    val pStateAnnot :
      Format.formatter -> MusynthTypes.musStateAnnotationT -> unit
    val pStateDecl :
      Format.formatter ->
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
      MusynthTypes.musStateAnnotationT -> unit
    val pStateDeclBlock :
      string ->
      Format.formatter ->
      (MusynthTypes.musDesignatorT MusynthTypes.musDeclType *
       MusynthTypes.musStateAnnotationT)
      list -> unit
    val pTransDecl :
      Format.formatter ->
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
       MusynthTypes.musDesignatorT)
      MusynthTypes.musDeclType -> unit
    val pTransDeclBlock :
      Format.formatter ->
      (MusynthTypes.musDesignatorT * MusynthTypes.musDesignatorT *
       MusynthTypes.musDesignatorT)
      MusynthTypes.musDeclType list -> unit
    val pInitStateConstraint :
      Format.formatter -> MusynthTypes.musPropT -> unit
    val pInitStateDeclBlock :
      Format.formatter -> MusynthTypes.musPropT list -> unit
    val pChanProp :
      Format.formatter ->
      MusynthTypes.musChanOrdT * MusynthTypes.musChanLossT *
      MusynthTypes.musChanDupT * MusynthTypes.musChanBlockT * int -> 
      unit
    val pFairness : Format.formatter -> MusynthTypes.musFairnessT -> unit
    val pLossFairness :
      Format.formatter -> MusynthTypes.musLossFairnessT -> unit
    val pAutomatonDecl :
      Format.formatter ->
      MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType -> unit
    val pSpec : Format.formatter -> MusynthTypes.musSpecT -> unit
    val pProg :
      Format.formatter ->
      ((string * 'a) * MusynthTypes.musSymTypeT) list *
      MusynthTypes.musDesignatorT MusynthTypes.musDeclType list *
      MusynthTypes.musAutomatonDeclType MusynthTypes.musDeclType list *
      MusynthTypes.musPropT list * MusynthTypes.musSpecT list -> unit
    val pLLDesignator :
      Format.formatter -> MusynthTypes.llDesignatorT -> unit
    val pLLIdent : Format.formatter -> MusynthTypes.llDesignatorT -> unit
    val pLLVar :
      Format.formatter ->
      MusynthTypes.llDesignatorT * MusynthTypes.LLDesigSet.t -> unit
    val pLLAnnot : Format.formatter -> MusynthTypes.llAnnotT -> unit
    val pLLTrans : Format.formatter -> MusynthTypes.llTransT -> unit
    val pLLProp : Format.formatter -> MusynthTypes.llPropT -> unit
    val pLLSpec : Format.formatter -> MusynthTypes.llSpecT -> unit
    val pLLAutomaton : Format.formatter -> MusynthTypes.llAutomatonT -> unit
    val pLLProg :
      Format.formatter ->
      MusynthTypes.llDesignatorT list * MusynthTypes.llAutomatonT list *
      MusynthTypes.llPropT * MusynthTypes.llSpecT list -> unit
  end
module Debug :
  sig
    module Opts :
      sig
        val debugLevel : int ref
        val debugFileName : string ref
        val onlySafety : bool ref
        val conjunctivePart : bool ref
        val inputFileName : string ref
        val numSolsRequested : int ref
        val reorderEnabled : bool ref
        val reorderMethod : Cudd.Man.reorder ref
        val reorderMethods : string list
      end
    val debugOC : out_channel option ref
    val debugFmt : Format.formatter option ref
    val getDebugFmt : unit -> Format.formatter
    val initDebugSubsys : string -> unit
    val shutDownDebugSubsys : unit -> unit
    val dprintf : int -> ('a, Format.formatter, unit) format -> 'a
    val dflush : unit -> unit
  end
module Opts :
  sig
    val debugLevel : int ref
    val debugFileName : string ref
    val onlySafety : bool ref
    val conjunctivePart : bool ref
    val inputFileName : string ref
    val numSolsRequested : int ref
    val reorderEnabled : bool ref
    val reorderMethod : Cudd.Man.reorder ref
    val reorderMethods : string list
  end
type bddType = Cudd.Man.d Cudd.Bdd.t
val fixPoint : ('a -> 'a) -> 'a -> 'a
val post :
  < getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
    getSubstTableP2U : unit -> 'a Cudd.Bdd.t array; .. > ->
  'a Cudd.Bdd.t -> 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t
val pre :
  < getCubeForPrimedVars : unit -> 'a Cudd.Bdd.t;
    getSubstTableU2P : unit -> 'a Cudd.Bdd.t array; .. > ->
  'a Cudd.Bdd.t -> 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t
val explain :
  < cubeOfMinTerm : 'a -> 'b Cudd.Bdd.t;
    getCubeForPrimedVars : unit -> 'b Cudd.Bdd.t;
    getCubeForUnprimedVars : unit -> 'b Cudd.Bdd.t;
    getCubePrinter : unit -> Format.formatter -> 'a -> unit;
    getStateVarPrinter : unit -> Format.formatter -> 'a -> unit;
    getSubstTableP2U : unit -> 'b Cudd.Bdd.t array;
    getSubstTableU2P : unit -> 'b Cudd.Bdd.t array;
    makeFalse : unit -> 'b Cudd.Bdd.t;
    pickMinTermOnStates : 'b Cudd.Bdd.t -> 'a; .. > ->
  'b Cudd.Bdd.t -> 'b Cudd.Bdd.t -> 'b Cudd.Bdd.t -> unit
val countCycles :
  < getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
    getNumMinTermsState : 'a Cudd.Bdd.t -> float;
    getSubstTableP2U : unit -> 'a Cudd.Bdd.t array; .. > ->
  'a Cudd.Bdd.t -> 'a Cudd.Bdd.t -> float
val getParamsForFeasible : 'a -> 'b -> 'c -> 'd -> unit
val synthForwardSafety :
  < cubeOfMinTerm : 'a -> 'b Cudd.Bdd.t;
    getCubeForParamVars : unit -> 'b Cudd.Bdd.t;
    getCubeForPrimedVars : unit -> 'b Cudd.Bdd.t;
    getCubeForUnprimedVars : unit -> 'b Cudd.Bdd.t;
    getCubePrinter : unit -> Format.formatter -> 'a -> unit;
    getNumMinTermsState : 'b Cudd.Bdd.t -> float;
    getParamVarPrinter : unit -> Format.formatter -> 'a -> unit;
    getStateVarPrinter : unit -> Format.formatter -> 'a -> unit;
    getSubstTableP2U : unit -> 'b Cudd.Bdd.t array;
    getSubstTableU2P : unit -> 'b Cudd.Bdd.t array;
    makeFalse : unit -> 'b Cudd.Bdd.t;
    pickMinTermOnStates : 'b Cudd.Bdd.t -> 'a; .. > ->
  'b Cudd.Bdd.t ->
  'b Cudd.Bdd.t -> 'b Cudd.Bdd.t -> 'b Cudd.Bdd.t MusynthTypes.synthExitStatT
val synthesize :
  < cubeOfMinTerm : 'a -> 'b Cudd.Bdd.t;
    getAllButParamCube : unit -> 'b Cudd.Bdd.t;
    getCubeForParamVars : unit -> 'b Cudd.Bdd.t;
    getCubeForPrimedVars : unit -> 'b Cudd.Bdd.t;
    getCubeForUnprimedVars : unit -> 'b Cudd.Bdd.t;
    getCubePrinter : unit -> Format.formatter -> 'a -> unit;
    getNumMinTermsParam : 'b Cudd.Bdd.t -> float;
    getNumMinTermsState : 'b Cudd.Bdd.t -> float;
    getParamVarPrinter : unit -> Format.formatter -> 'a -> unit;
    getStateVarPrinter : unit -> Format.formatter -> 'a -> unit;
    getSubstTableP2U : unit -> 'b Cudd.Bdd.t array;
    getSubstTableU2P : unit -> 'b Cudd.Bdd.t array;
    makeFalse : unit -> 'b Cudd.Bdd.t;
    pickMinTermOnStates : 'b Cudd.Bdd.t -> 'a; .. > ->
  'b Cudd.Bdd.t -> 'b Cudd.Bdd.t -> 'b Cudd.Bdd.t -> 'c -> 'b Cudd.Bdd.t
val synthFrontEnd :
  < cubeOfMinTerm : 'a -> 'b Cudd.Bdd.t;
    getAllButParamCube : unit -> 'b Cudd.Bdd.t;
    getConstraintsOnParams : unit -> 'b Cudd.Bdd.t;
    getCubeForParamVars : unit -> 'b Cudd.Bdd.t;
    getCubeForPrimedVars : unit -> 'b Cudd.Bdd.t;
    getCubeForUnprimedVars : unit -> 'b Cudd.Bdd.t;
    getCubePrinter : unit -> Format.formatter -> 'a -> unit;
    getNumMinTermsParam : 'b Cudd.Bdd.t -> float;
    getNumMinTermsState : 'b Cudd.Bdd.t -> float;
    getParamVarPrinter : unit -> Format.formatter -> 'a -> unit;
    getStateVarPrinter : unit -> Format.formatter -> 'a -> unit;
    getSubstTableP2U : unit -> 'b Cudd.Bdd.t array;
    getSubstTableU2P : unit -> 'b Cudd.Bdd.t array;
    makeFalse : unit -> 'b Cudd.Bdd.t; makeTrue : unit -> 'b Cudd.Bdd.t;
    pickMinTermOnStates : 'b Cudd.Bdd.t -> 'a; .. > ->
  'b Cudd.Bdd.t MusynthTypes.LLDesigMap.t ->
  'b Cudd.Bdd.t -> 'b Cudd.Bdd.t -> 'b Cudd.Bdd.t -> 'c -> 'b Cudd.Bdd.t
