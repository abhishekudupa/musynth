module StringMap :
  sig
    type key = string
    type +'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end
type ltl3baprop =
    LTL3BAPropTrue
  | LTL3BAPropFalse
  | LTL3BAPropLiteral of string
  | LTL3BAPropNegation of ltl3baprop
  | LTL3BAPropConjunction of ltl3baprop list
  | LTL3BAPropDisjunction of ltl3baprop list
type ltl3baedge = string * ltl3baprop * string
type ltl3banode = string * bool * bool * ltl3baedge list
type ltl3baautomaton = ltl3banode StringMap.t * string list
type ltl3bapropLL = bool * bool * string * bool
type ltl3bacubeLL = ltl3bapropLL array
type ltl3baedgeLL = string * ltl3bacubeLL array * string
type ltl3banodeLL = string * bool * bool * ltl3baedgeLL array
type ltl3baautomatonLL = ltl3banodeLL array
val ltl3baproptostring : ltl3baprop -> string
val ltl3baToDot :
  ('a * bool * bool * (string * ltl3baprop * string) list) StringMap.t * 'b ->
  string -> unit
external ltl3ba_mk_ba : string -> bool -> bool -> unit
  = "ltl3ba_native_mk_ba"
external ltl3ba_translate_ba : unit -> ltl3baautomatonLL
  = "ltl3ba_native_translate_ba"
external ltl3ba_teardown : unit -> unit = "ltl3ba_native_teardown"
val raiseLLProp : bool * bool * string * bool -> ltl3baprop
val raiseCube : (bool * bool * string * bool) list -> ltl3baprop
val raiseCubeList : (bool * bool * string * bool) array list -> ltl3baprop
val raiseLLEdge :
  'a * (bool * bool * string * bool) array array * 'b -> 'a * ltl3baprop * 'b
val raiseLLNode :
  'a * 'b * 'c * ('d * (bool * bool * string * bool) array array * 'e) array ->
  'a * 'b * 'c * ('d * ltl3baprop * 'e) list
val raiseLLAut :
  (StringMap.key * bool * 'a *
   ('b * (bool * bool * string * bool) array array * 'c) array)
  array ->
  (StringMap.key * bool * 'a * ('b * ltl3baprop * 'c) list) StringMap.t *
  StringMap.key list
val ltl3ba :
  string ->
  bool ->
  bool ->
  (StringMap.key * bool * bool * (string * ltl3baprop * string) list)
  StringMap.t * StringMap.key list
