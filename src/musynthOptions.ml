open MusynthTypes

(* options for musynth *)
let debugDisabled = ref false

let debugOptions = ref StringSet.empty

let debugFileName = ref ""

let onlySafety = ref true 

let conjunctivePart = ref false 

let inputFileName = ref ""

let numSolsRequested = ref 1

let reorderEnabled = ref false

let reorderMethod = ref Cudd.Man.REORDER_NONE

let tracePrintMode = ref "diff"

let reorderMethods = [ "random"; "randompivot"; "sift"; "siftconverge"; 
                       "symmsift"; "symmsiftconverge"; "window2"; "window3";
                       "window4"; "window2converge"; "window3converge"; "window4converge";
                       "groupsift"; "groupsiftconverge"; "annealing"; "genetic"; 
                       "linear"; "linearconverge"; "lazysift"; "exact" ]


