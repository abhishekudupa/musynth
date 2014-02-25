(* Lexer for musynth *)

{
open MusynthParser
open MusynthAST
open Lexing

let incLineNum lexbuf = 
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- 
    { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum }
  
let musynthKeywords = 
  [ ("var", VAR);
    ("detautomaton", DETAUTOMATON);
    ("automaton", AUTOMATON);
    ("partialautomaton", PARTIALAUTOMATON);
    ("lossy", LOSSY);
    ("lossless", LOSSLESS);
    ("duplicating", DUPLICATING);
    ("nonduplicating", NONDUPLICATING);
    ("ordered", ORDERED);
    ("unordered", UNORDERED);
    ("transitions", TRANSITIONS);
    ("inputs", INPUTS);
    ("outputs", OUTPUTS);
    ("define", DEFINE);
    ("fairness", FAIRNESS);
    ("cansyncon", CANSYNCON);
    ("ctlspec", CTLSPEC);
    ("ltlspec", LTLSPEC);
    ("invariant", INVARIANT);
    ("incomplete", INCOMPLETE);
    ("complete", COMPLETE);
    ("state", STATE);
    ("forall", FORALL);
    ("foreach", FOREACH);
    ("exists", EXISTS);
  ]

let musynthKwTable = 
  let tbl = Hashtbl.create 32 in
  List.iter (fun (kw, token) -> Hashtbl.add tbl kw tok) musynthKeywords;
  tbl

let findKeyword kw = 
  Hashtbl.find musynthKwTable (String.lowercase kw)


}
