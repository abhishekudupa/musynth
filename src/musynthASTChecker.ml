(* semantic checks on AST *)

open MusynthAST

let gatherQVarsInDesig desig =
  let retval = ref [] in
  let rec gatherQVarsInDesigRec desig =
    match desig with
    | SimpleDesignator _ -> ()
    | IndexDesignator (innerdesig, ident) ->
        retval := ident :: !retval;
        gatherQVarsInDesigRec innerdesig
    | FieldDesignator (innerdesig, ident) ->
        gatherQVarsInDesigRec innerdesig
  in
  gatherQVarsInDesigRec desig;
  !retval

let checkDecl 
