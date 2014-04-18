type bddType = Cudd.Man.d Cudd.Bdd.t
val fixPoint : ('a -> 'a) -> 'a -> 'a
val post :
  < getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
    getNumMinTerms : 'a Cudd.Bdd.t -> float;
    getSubstTableP2U : unit -> 'a Cudd.Bdd.t array; .. > ->
  'a Cudd.Bdd.t -> 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t
val synthForwardSafety :
  < getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
    getNumMinTerms : 'a Cudd.Bdd.t -> float;
    getSubstTableP2U : unit -> 'a Cudd.Bdd.t array;
    getVarPrinter : unit -> Format.formatter -> int -> unit;
    makeFalse : unit -> 'a Cudd.Bdd.t; .. > ->
  'a Cudd.Bdd.t ->
  'a Cudd.Bdd.t -> 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t MusynthTypes.synthExitStatT
val synthesize :
  < getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
    getNumMinTerms : 'a Cudd.Bdd.t -> float;
    getSubstTableP2U : unit -> 'a Cudd.Bdd.t array;
    getVarPrinter : unit -> Format.formatter -> int -> unit;
    makeFalse : unit -> 'a Cudd.Bdd.t; .. > ->
  'a Cudd.Bdd.t -> 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t
val synthFrontEnd :
  < getCubeForUnprimedVars : unit -> 'a Cudd.Bdd.t;
    getNumMinTerms : 'a Cudd.Bdd.t -> float;
    getSubstTableP2U : unit -> 'a Cudd.Bdd.t array;
    getVarPrinter : unit -> Format.formatter -> int -> unit;
    makeFalse : unit -> 'a Cudd.Bdd.t; makeTrue : unit -> 'a Cudd.Bdd.t; .. > ->
  'a Cudd.Bdd.t MusynthTypes.LLDesigMap.t ->
  'a Cudd.Bdd.t -> 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t -> 'a Cudd.Bdd.t
