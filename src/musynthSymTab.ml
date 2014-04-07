(* symbol table functions *)
open MusynthTypes

let createSymTab () =
  ref [ ref IdentMap.empty ]

let push symtab =
  symtab := (ref IdentMap.empty) :: !symtab

let pushScope symtab scope = 
  symtab := scope :: !symtab

let pop symtab =
  let scope = 
    (match !symtab with
    | [] -> raise SymtabUnderflow
    | head :: rest -> symtab := rest; head) 
  in
  scope

let peek symtab =
  match !symtab with
  | [] -> raise SymtabUnderflow
  | head :: rest -> head

let getNumScopes symtab =
  List.length !symtab

let lookup symtab ident =
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
    
let bind symtab ident entry =
  let scope = peek symtab in
  try
    let _ = IdentMap.find ident !scope in
    raise (DuplicateSymbol ident)
  with
  | Not_found -> 
      scope := IdentMap.add ident entry !scope

let bindGlobal symtab ident entry =
  let scope = List.nth !symtab ((List.length !symtab) - 1) in
  try
    let _ = IdentMap.find ident !scope in
    raise (DuplicateSymbol ident)
  with
  | Not_found -> scope := IdentMap.add ident entry !scope

let lookupOrFail symtab ident =
  let entry = lookup symtab ident in
  match entry with
  | None -> raise (UndeclaredIdentifier ident)
  | Some valu -> valu

let lookupVar symtab ident =
  let entry = lookupOrFail symtab ident in
  match entry with
  | SymVarName (_, typ) -> typ
  | _ -> raise (WrongTypeIdentifier ("Expected a quantified variable", ident))

let lookupType symtab ident =
  let entry = lookupOrFail symtab ident in
  match entry with
  | SymtypeName (_, typ) -> typ
  | _ ->  raise (WrongTypeIdentifier ("Expected a type name", ident))

let lookupGMsg symtab ident =
  let entry = lookupOrFail symtab ident in
  match entry with
  | GlobalMsgName _ -> entry
  | _ -> raise (WrongTypeIdentifier ("Expected a message name", ident))

let lookupAMsg symtab ident =
  let entry = lookupOrFail symtab ident in
  match entry with
  | AutomatonMsgName _ -> entry
  | _ -> raise (WrongTypeIdentifier ("Expected a message name", ident))

let lookupState symtab ident =
  let entry = lookupOrFail symtab ident in
  match entry with
  | StateName _ -> entry
  | _ -> raise (WrongTypeIdentifier ("Expected a state name", ident))

let lookupAutomaton symtab ident = 
  let entry = lookupOrFail symtab ident in
  match entry with
  | AutomatonName _ -> entry
  | _ -> raise (WrongTypeIdentifier ("Expected an automaton name", ident))

let lookupConst symtab ident =
  let entry = lookupOrFail symtab ident in
  match entry with
  | SymtypeConst _ -> entry
  | _ -> raise (WrongTypeIdentifier ("Expected a symmetric type constructor", ident))
