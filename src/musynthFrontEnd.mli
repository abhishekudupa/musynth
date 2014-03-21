val locToStr : int * int * int * int -> string
val pLoc : Format.formatter -> int * int * int * int -> unit
val pLocOpt : Format.formatter -> (int * int * int * int) option -> unit
val musynthParse : string option -> unit
