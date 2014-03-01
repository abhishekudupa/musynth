(* types and functions for the parse tree *)

(* file name * start line num * start col * end line num * end col *)
type sourcelocation = string * int * int * int * int

type musSymTypesDeclT = (string * (string list)) list

type musProgT = musSymTypesDeclT * (musAutomatonDeclT list) * 
      (musSpecificationT list)


