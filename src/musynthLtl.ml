(* module for dealing with ltl formulae and buchi automaton *)

open MusynthTypes

(* all states, initial states, accepting states, *)
type buchiAutomatonT = string list * string list * 
                         string list * 
