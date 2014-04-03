(* wrappers and types for interfacing with ltl3ba *)

open Format

module StringMap = Map.Make
    (struct
      t = string
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
  fprintf fmt "rankdir=LR;@,"
  List.iter 
    (fun nodename -> 
      fprintf fmt "%s [shape=ellipse,style=filled];@," nodename)
    initstates;
  Map.iter 
    (fun nodename nodeattr ->
      if (List.mem nodename initstates) then
        ()
      else
        begin
          let _, init, accept, edges = nodeattr in
          if accept then
            fprintf fmt "%s [shape=doublecircle];@," nodename
          else
            fprintf fmt "%s [shape=ellipse];@," nodename) autmap;
  (* draw out the edges *)
  Map.iter 
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
external ltl3ba : string -> bool -> bool -> ltl3baautomaton =
  "ltl3ba_native"
