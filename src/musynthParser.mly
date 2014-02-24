/** MuSynth Parser */

/* Keywords */
%token VAR EQUALS DOT MAIN
%token DETAUTOMATON AUTOMATON CHANNELAUTOMATON
%token LOSSY LOSSLESS DUPLICATING NONDUPLICATING
%token ORDERED UNORDERED PARTIALAUTOMATON TRANSITIONS
%token INPUTS OUTPUTS DEFINE FAIRNESS CANSYNCON CTLSPEC
%token LTLSPEC INVARIANT INCOMPLETE COMPLETE LBRACE RBRACE
%token LPAREN RPAREN LSQUARE RSQUARE STATE
%token OR AND NOT IMPLIES IFF TLFORALL TLEXISTS
%token TLFUTURE TLGLOBAL

/* Associativity for expressions */

%nonassoc IMPLIES IFF
%left OR
%left AND
%left NOT

%{
open Lexing
open MusynthAST
open Parsing
open Format
  
let errmsg item msg =
  let start_pos = if item <= 0 then symbol_start_pos () else rhs_start_pos item in
  let fname = start_pos.pos_fname in
  let lineno = start_pos.pos_lnum in
  let col = start_pos.pos_cnum - start_pos.pos_bol in
  Printf.eprintf "%s:%d.%d: Syntax error: %s\n%!" fname lineno col msg;
  raise Parse_error
    
let getrhsloc item =
  let endpos = rhs_end_pos item in
  let fname = startpos.pos_fname in
  let slinenum = startpos.pos_lnum in
  let scolnum = startpos.pos_cnum - startpos.pos_bol in
  let elinenum = endpos.pos_lnum in
  let ecolnum = endpos.pos_cnum - endpos.pos_bol in
  (fname, slinenum, scolnum, elinenum, ecolnum)
    
let getlhsloc () =
  let startpos = symbol_start_pos () in
  let endpos = symbol_end_pos () in
    (startpos.pos_fname, startpos.pos_lnum,
     startpos.pos_cnum - startpos.pos_bol,
     endpos.pos_lnum, endpos.pos_cnum - endpos.pos_bol)
    
%}

%%

