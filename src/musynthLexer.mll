(* Lexer for musynth *)

{
open MusynthParser
open MusynthAST
open Lexing
open Printf

let musynthKeywords = 
  [ ("var", VAR);
    ("detautomaton", DETAUTOMATON);
    ("symmetrictypes", SYMMETRICTYPES);
    ("automaton", AUTOMATON);
    ("partialautomaton", PARTIALAUTOMATON);
    ("channelautomaton", CHANNELAUTOMATON);
    ("initstate", INITSTATE);
    ("in", IN);
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
    ("true", BCTRUE);
    ("false", BCFALSE)
  ]

let musynthKwTable = 
  let tbl = Hashtbl.create 32 in
  List.iter (fun (kw, token) -> Hashtbl.add tbl kw tok) musynthKeywords;
  tbl

let findKeyword kw = 
  Hashtbl.find musynthKwTable (String.lowercase kw)
} 

let whitespace = [' ' '\t' '\r']
let newline = ['\n']
let uppercase = ['A' - 'Z']
let lowercase = ['a' - 'z']
let underscore = '_'
let prime = '\''
let idletter = (uppercase | lowercase | underscore)
let digit =['0' - '9']
let string = '"' [^ '"' '\n' ]* '"'

let identifier = idletter (idletter | digit)* prime?

rule token = parse
| ";" { SEMICOLON }
| "." { DOT }
| "=" { EQUALS }
| "!=" { NEQUALS }
| "!" { NOT }
| "[" { LSQUARE }
| "]" { RSQUARE }
| "{" { LBRACE }
| "}" { RBRACE }
| "(" { LPAREN }
| ")" { RPAREN }
| "&" { AND }
| "|" { OR }
| "AG" { TLAG }
| "AG" { TLAF }
| "EG" { TLEG }
| "EF" { TLEF }
| "AX" { TLAX }
| "EX" { TLEX }
| "A" { TLFORALL }
| "E" { TLEXISTS }
| "G" { TLGLOBAL }
| "F" { TLFUTURE }
| "X" { TLNEXT }
| "->" { IMPLIES }
| "<->" { IFF }
| identifier as name 
{
  try
    findKeyword name
  with
  | Not_found ->
}
| string as str { (STRINGCONST (String.sub str 1 (String.length str - 2))) }
| newline { Lexing.newline lexbuf; token lexbuf }
| whitespace { token lexbuf }
| "--" { linecomment lexbuf }
| "/*" { blockcomment lexbuf }
| "//" { linecomment lexbuf }
| _ as c 
{ 
  let buf = Buffer.create 16 in
  let startpos = lexeme_start_p lexbuf in
  let endpos = lexeme_end_p lexbuf in
  bprintf buf "Lexer: Unexpected token \'%c\'" c;
  let loc = 
    (startpos.pos_fname, startpos.pos_lnum,
     (startpos.pos_cnum - startpos.pos_bol), endpos.pos_lnum,
     (endpos.pos_cnum - endpos.pos_bol)) in
  raise (ParseError buf.contents loc)
}

and linecomment = parse
| newline { Lexing.newline lexbuf; token lexbuf }
| _ { linecomment lexbuf }

and blockcomment = parse
| "*/" { token lexbuf }
| newline { Lexing.newline lexbuf; blockcomment lexbuf }
| _ { blockcomment lexbuf }
| eof 
{
  let buf = Buffer.create 16 in
  let startpos = lexeme_start_p lexbuf in
  let endpos = lexeme_end_p lexbuf in
  bprintf buf "Lexer: Unterminated block comment";
  let loc = 
    (startpos.pos_fname, startpos.pos_lnum,
     (startpos.pos_cnum - startpos.pos_bol), endpos.pos_lnum,
     (endpos.pos_cnum - endpos.pos_bol)) in
  raise (ParseError buf.contents loc)
} 
