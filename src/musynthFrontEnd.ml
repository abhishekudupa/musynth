(* routines for parsing, etc *)

open MusynthParser
open MusynthAST
open MusynthASTChecker
open MusynthASTLower
open MusynthTypes
module ST = MusynthSymTab
open Format
open Buffer
open Lexing
module Enc = MusynthBDDEncoder
module LTL = MusynthLtl
module MC = MusynthMC
module Opts = MusynthOptions
module Utils = MusynthUtils
module Mgr = MusynthBDDManager

let musynthProcess filename =
  let inchan = 
    (match filename with
     | Some fname -> open_in fname
     | None -> stdin) in
  let lexbuf = Lexing.from_channel inchan in
  try 
    let prog = MusynthParser.prog MusynthLexer.token lexbuf in
    let symtab = ST.createSymTab () in
    checkProg symtab prog;
    fprintf err_formatter "Semantic checks complete\n";
    pProg std_formatter prog;
    let lprog = MusynthASTLower.lowerProg symtab prog in
    (* run low level checks *)
    checkLLProg lprog;

    if (!Opts.debugLevel >= 1) then
      begin
        match filename with
        | Some fname ->
           let oc, fmt = Utils.makeFormatterOfName (fname ^ ".lowered") in
           pLLProg fmt lprog;
           pp_print_flush fmt ();
           close_out oc
        | None -> 
           pLLProg std_formatter lprog;
           pp_print_flush std_formatter ()
      end
    else
      ();

    let mgr = new Mgr.bddManager in
    let transBDDs, initBDD, badStateBDD, dlfBDD = Enc.encodeProg mgr lprog in
    MC.synthFrontEnd mgr transBDDs initBDD badStateBDD dlfBDD
  with
  | ParseError (errstr, loc) -> 
     printf "%s\n%a\n" errstr pLoc loc; 
     raise (ParseError (errstr, loc))
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
       
