(* routines for parsing, etc *)

open MusynthParser
open MusynthAST
open Format
open Buffer
open Lexing

let locToStr loc =
  let buf = Buffer.create 16 in
  let sline, scol, eline, ecol = loc in
  let fmt = formatter_of_buffer buf in
  fprintf fmt "%d:%d - %d:%d" sline scol eline ecol;
  pp_print_flush fmt ();
  Buffer.contents buf

let pLoc fmt loc =
  let locstr = locToStr loc in
  fprintf fmt "%s" locstr;
  pp_print_flush fmt ()

let musynthParse filename =
  let inchan = 
    (match filename with
    | Some fname -> open_in fname
    | None -> stdin) in
  let lexbuf = Lexing.from_channel inchan in
  try 
    printf "Beginning parse\n"; flush stdout;
    MusynthParser.prog MusynthLexer.token lexbuf
  with
  | ParseError (errstr, loc) -> 
      printf "%s\n%a\n" errstr pLoc loc; 
      raise (ParseError (errstr, loc))
  | SemanticError (errstr, loc) -> 
      printf "%s\n%a\n" errstr pLoc loc;
      raise (SemanticError (errstr, loc))
  | Parsing.Parse_error ->
      begin
          let startpos = Lexing.lexeme_start_p lexbuf in
          let endpos = Lexing.lexeme_end_p lexbuf in
          let tok = Lexing.lexeme lexbuf in
          let buf = Buffer.create 16 in
          let fmt = formatter_of_buffer buf in
          fprintf fmt "Syntax Error: on token %s" tok;
          pp_print_flush fmt ();
          pp_print_flush std_formatter();
          let loc = (startpos.pos_lnum,
                     (startpos.pos_cnum - startpos.pos_bol),
                     endpos.pos_lnum, (endpos.pos_cnum - endpos.pos_bol)) in
          let newex = ParseError (Buffer.contents buf, loc) in
          printf "%s\n%a\n" (Buffer.contents buf) pLoc loc;
          pp_print_flush std_formatter ();
          raise newex
      end
