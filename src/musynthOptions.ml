open MusynthTypes

(* options for musynth *)
let debugDisabled = ref true

let debugOptions = ref StringSet.empty

let debugFileName = ref ""

let onlySafety = ref true 

let disjunctivePart = ref false 

let inputFileName = ref ""

let numSolsRequested = ref 1

let reorderEnabled = ref false

let reorderMethod = ref Cudd.Man.REORDER_SIFT

let tracePrintMode = ref "diff"

let jumpStep = ref 1

let reorderMethods = [ "random"; "randompivot"; "sift"; "siftconverge"; 
                       "symmsift"; "symmsiftconverge"; "window2"; "window3";
                       "window4"; "window2converge"; "window3converge"; "window4converge";
                       "groupsift"; "groupsiftconverge"; "annealing"; "genetic"; 
                       "linear"; "linearconverge"; "lazysift"; "exact" ]


