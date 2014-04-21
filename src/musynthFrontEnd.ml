(* routines for parsing, etc *)

open MusynthTypes
open Printf
open Buffer
open Lexing

module Lower = MusynthASTLower
module CK = MusynthASTChecker
module Parser = MusynthParser
module AST = MusynthAST
module ST = MusynthSymTab
module Enc = MusynthBDDEncoder
module MC = MusynthMC
module Opts = MusynthOptions
module Utils = MusynthUtils
module Mgr = MusynthBDDManager
module Debug = MusynthDebug

let musynthProcess filename =
  let inchan = 
    (match filename with
     | Some fname -> open_in fname
     | None -> stdin) in
  let lexbuf = Lexing.from_channel inchan in
  try
    printf "Parsing... "; flush stdout;
    let prog = Parser.prog MusynthLexer.token lexbuf in
    printf "Done!\n"; flush stdout;
    let symtab = ST.createSymTab () in
    printf "Performing Semantic Checks... "; flush stdout;
    CK.checkProg symtab prog; 
    printf "Done!\n"; flush stdout;
    Debug.dprintf 1 "Program:@,@,%a@," AST.pProg prog;
    printf "Lowering Program... "; flush stdout;
    let lprog = Lower.lowerProg symtab prog in
    printf "Done!\n"; flush stdout;
    (* run low level checks *)
    printf "Performing Checks on Lowered Program... "; flush stdout;
    CK.checkLLProg lprog;
    printf "Done!\n"; flush stdout;

    Debug.dprintf 1 "%a" AST.pLLProg lprog;

    let mgr = new Mgr.bddManager in
    printf "Encoding Program to BDDs... "; flush stdout;
    let transBDDs, initBDD, badStateBDD, dlfBDD = Enc.encodeProg mgr lprog in
    printf "Done!\n"; flush stdout;
    printf "Attempting to Synthesize... "; flush stdout;
    let solbdd = MC.synthFrontEnd mgr transBDDs initBDD badStateBDD dlfBDD in
    printf "Done!\n"; flush stdout;
    printf "\n\nSolutions:\n"; flush stdout;
    Format.printf "@[<v 0>%a@,@]" (mgr#printParamVars !Opts.numSolsRequested) solbdd; 
    Format.pp_print_flush Format.std_formatter ();
    printf "Peak BDD node count = %d nodes\n\n" (mgr#getPeakBDDSize ()); flush stdout
  with
  | ParseError (errstr, loc) -> 
     Format.printf "%s\n%a\n" errstr AST.pLoc loc; 
     raise (ParseError (errstr, loc))
  | Parsing.Parse_error ->
     begin
       let startpos = Lexing.lexeme_start_p lexbuf in
       let endpos = Lexing.lexeme_end_p lexbuf in
       let tok = Lexing.lexeme lexbuf in
       let buf = Buffer.create 16 in
       let fmt = Format.formatter_of_buffer buf in
       Format.fprintf fmt "Syntax Error: on token %s" tok;
       Format.pp_print_flush fmt ();
       Format.pp_print_flush Format.std_formatter ();
       let loc = (startpos.pos_lnum,
                  (startpos.pos_cnum - startpos.pos_bol),
                  endpos.pos_lnum, (endpos.pos_cnum - endpos.pos_bol)) in
       let newex = ParseError (Buffer.contents buf, loc) in
       Format.printf "%s\n%a\n" (Buffer.contents buf) AST.pLoc loc;
       Format.pp_print_flush Format.std_formatter ();
       raise newex
     end
       
