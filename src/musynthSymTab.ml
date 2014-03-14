(* symbol table functions and types *)

open MusynthAST

type msgType = 
  | InputMsg
  | OutputMsg

type symtabEntry =
  | StateEntry of     
