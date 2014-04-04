(* wrappers and types for interfacing with ltl3ba *)

open Format
open Array

module StringMap = Map.Make
    (struct
      type t = string
      let compare = Pervasives.compare
    end)

type ltl3baprop =
  | LTL3BAPropTrue
  | LTL3BAPropFalse
  | LTL3BAPropLiteral of string
  | LTL3BAPropNegation of ltl3baprop
  | LTL3BAPropConjunction of ltl3baprop list
  | LTL3BAPropDisjunction of ltl3baprop list

(* start node name, condition, end node name *)
type ltl3baedge = (string * ltl3baprop * string)

(* node name, initial, accepting, outgoing edges *)
type ltl3banode = (string * bool * bool * ltl3baedge list)

(* node name to node map, initial state list *)
type ltl3baautomaton = ltl3banode StringMap.t * string list

type ltl3bapropLL = 
  | LTL3BAPropLLTrue
  | LTL3BAPropLLFalse
  | LTL3BAPropLLLiteral of (string * bool)

type ltl3bacubeLL = ltl3bapropLL list

type ltl3baedgeLL = (string * ltl3bacubeLL list * string)

type ltl3banodeLL = (string * bool * bool * ltl3baedgeLL array)

type ltl3baautomatonLL = ltl3banodeLL array

let rec ltl3baproptostring prop = 
  let rec listFolder lst sep =
    match lst with
    | [] -> assert false
    | [ head ] -> ltl3baproptostring head
    | head :: rest -> (ltl3baproptostring head) ^ sep ^ (listFolder rest sep)
  in
  match prop with
  | LTL3BAPropTrue -> "true"
  | LTL3BAPropFalse -> "false"
  | LTL3BAPropLiteral name -> name
  | LTL3BAPropNegation prop -> "(!" ^ (ltl3baproptostring prop) ^ ")"
  | LTL3BAPropConjunction proplist -> "(" ^ (listFolder proplist " & ") ^ ")"
  | LTL3BAPropDisjunction proplist -> "(" ^ (listFolder proplist " | ") ^ ")"
      
  
let ltl3baToDot aut fname =
  let oc = open_out fname in
  let fmt = formatter_of_out_channel oc in
  let autmap, initstates = aut in
  (* create the declarations for each node *)
  fprintf fmt "@[<v 0>@[<v 4>digraph BA {@,";
  fprintf fmt "rankdir=LR;@,";
  List.iter 
    (fun nodename -> 
      fprintf fmt "%s [shape=ellipse,style=filled];@," nodename)
    initstates;
  StringMap.iter
    (fun nodename nodeattr ->
      if (List.mem nodename initstates) then
        ()
      else
        begin
          let _, init, accept, edges = nodeattr in
          if accept then
            fprintf fmt "%s [shape=doublecircle];@," nodename
          else
            fprintf fmt "%s [shape=ellipse];@," nodename
        end) autmap;

  (* draw out the edges *)
  StringMap.iter 
    (fun nodename nodeattr ->
      let _, _, _, edges = nodeattr in
      List.iter
        (fun edge ->
          let start, prop, final = edge in
          fprintf fmt "%s -> %s [label=\"%s\"];@," start final 
            (ltl3baproptostring prop)) edges) autmap;
  fprintf fmt "@]@,}@,@]";
  pp_print_flush fmt ();
  close_out oc

(* defs for externals *)
external ltl3ba_mk_ba : string -> bool -> bool -> unit =
  "ltl3ba_native_mk_ba"

external ltl3ba_translate_ba : unit -> ltl3baautomatonLL =
  "ltl3ba_native_translate_ba"

external ltl3ba_teardown : unit -> unit =
  "ltl3ba_native_teardown"

let raiseLLProp prop =
  match prop with
  | LTL3BAPropLLTrue -> LTL3BAPropTrue
  | LTL3BAPropLLFalse -> LTL3BAPropFalse
  | LTL3BAPropLLLiteral (name, negated) ->
      if negated then
        LTL3BAPropNegation (LTL3BAPropLiteral name)
      else
        LTL3BAPropLiteral name

let raiseCube cube =
  match cube with
  | [] -> assert false
  | [ head ] -> raiseLLProp head
  | head :: rest -> 
      LTL3BAPropConjunction (List.map raiseLLProp cube)

let raiseCubeList cubelist = 
  match cubelist with
  | [] -> assert false
  | [ head ] -> raiseCube head
  | head :: rest -> 
      LTL3BAPropDisjunction (List.map raiseCube cubelist)

let raiseLLEdge edge = 
  let initname, cubelist, finalname = edge in
  (initname, raiseCubeList cubelist, finalname)

let raiseLLNode node =
  let nodename, initial, accepting, edges = node in
  (nodename, initial, accepting, 
   (Array.to_list (Array.map raiseLLEdge edges)))

let raiseLLAut llba =
  (Array.fold_left 
     (fun map llnode ->
       let node = raiseLLNode llnode in
       let nodename, _, _, _ = node in
       StringMap.add nodename node map)
     StringMap.empty llba,
   Array.fold_left 
     (fun initstates llnode ->
       let name, initial, _, _ = llnode in
       if initial then
         name :: initstates
       else
         initstates) 
     [] llba)

let ltl3ba propString det simp =
  ltl3ba_mk_ba propString det simp;
  let llba = ltl3ba_translate_ba () in
  ltl3ba_teardown ();
  (* translate to higher level ba *)
  raiseLLAut llba
