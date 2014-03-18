(* symbol table functions *)
open MusynthTypes

let createSymTab () =
  ref [ ref IdentMap.empty ]

let push (symtab : symTableT) =
  symtab := (ref IdentMap.empty) :: !symtab

let pushScope (symtab : symTableT) (scope : symTabScope) = 
  symtab := scope :: !symtab

let pop (symtab : symTableT) =
  let scope = 
    (match !symtab with
    | [] -> raise SymtabUnderflow
    | head :: rest -> symtab := rest; head) 
  in
  scope

let peek (symtab : symTableT) =
  match !symtab with
  | [] -> raise SymtabUnderflow
  | head :: rest -> head

let getNumScopes (symtab : symTableT) =
  List.length !symtab

let lookup (symtab : symTableT) (ident : identifierT) =
  let rec lookupRec symtab =
    match symtab with
    | [] -> None
    | head :: rest ->
        begin
          try 
            Some (IdentMap.find ident !head)
          with Not_found ->
            lookupRec rest
        end
  in
  lookupRec !symtab
    
let bind (symtab : symTableT) (ident : identifierT) (entry : symtabEntry) =
  let scope = peek symtab in
  try
    let _ = IdentMap.find ident !scope in
    raise (DuplicateSymbol ident)
  with
  | Not_found -> 
      scope := IdentMap.add ident entry !scope

let lookupOrFail symtab ident =
  let entry = lookup symtab ident in
  match entry with
  | None -> raise (UndeclaredIdentifier ident)
  | Some valu -> valu

let lookupVar symtab ident =
  let entry = lookupOrFail symtab ident in
  match entry with
  | VarEntry typ -> typ
  | _ -> raise (UndeclaredIdentifier ident)

let lookupType symtab ident =
  let entry = lookupOrFail symtab ident in
  match entry with
  | TypeEntry typ -> typ
  | _ ->  raise (UndeclaredIdentifier ident)
