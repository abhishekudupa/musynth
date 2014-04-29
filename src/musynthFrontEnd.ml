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
module Trace = MusynthTrace

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
    Debug.dprintf "prog" "Program:@,@,%a@," AST.pProg prog;
    printf "Lowering Program... "; flush stdout;
    let lprog = Lower.lowerProg symtab prog in
    printf "Done!\n"; flush stdout;
    (* run low level checks *)
    printf "Performing Checks on Lowered Program... "; flush stdout;
    CK.checkLLProg lprog;
    printf "Done!\n"; flush stdout;

    Debug.dprintf "lprog" "Lowered Program:@,@,%a@,@," AST.pLLProg lprog;

    let mgr = new Mgr.bddManager in
    printf "Encoding Program to BDDs and LTL to BDDs via Tableau... "; flush stdout;
    let (transBDDs, initBDD, badStateBDD, dlfBDD, ltltableaulist) = 
      Enc.encodeProg mgr lprog
    in
    printf "Done!\n"; flush stdout;
    if ((mgr#getNumParamVars ()) <> 0) then
      begin
        printf "Attempting to Synthesize... "; flush stdout;
        let solbdd = 
          MC.synthFrontEnd mgr transBDDs initBDD badStateBDD dlfBDD ltltableaulist 
        in
        printf "Done!\n"; flush stdout;
        if (mgr#isFalse solbdd) then
          printf "\n\nNo Solutions Found!\n\n"
        else 
          begin
            printf "\n\n%e solutions found.\n" (mgr#getNumMinTermsParam solbdd);
            printf "\n\nSolutions:\n"; flush stdout;
            Format.printf "@[<v 0>%a@,@]" (mgr#printParamVars !Opts.numSolsRequested) solbdd
          end
      end
    else
      begin
        printf "Nothing to synthesize, switching to model checking mode.\nModel checking... ";
        flush stdout;
        let status = MC.check mgr transBDDs initBDD badStateBDD dlfBDD ltltableaulist in
        printf "Done!\n";
        match status with
        | MCSuccess _ ->
           printf "All properties hold\n"
        | MCFailureSafety trace ->
           printf "Safety violation found. Trace:\n\n"; flush stdout;
           Format.fprintf Format.std_formatter "@[<v 0>";
           Trace.printTraceSafety Format.std_formatter trace;
           Format.fprintf Format.std_formatter "@,@,@]";
           Format.pp_print_flush Format.std_formatter ();
        | MCFailureLiveness (name, prefix, loop) ->
           printf "Liveness violation of property \"%s\" found. Trace:\n\n" name; flush stdout;
           Format.fprintf Format.std_formatter "@[<v 0>";
           Trace.printTraceLiveness Format.std_formatter prefix loop;
           Format.fprintf Format.std_formatter "@,@,@]";
           Format.pp_print_flush Format.std_formatter ();
      end;
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
