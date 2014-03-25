open MusynthTypes
(* utility functions *)

let crossProduct lstlst =
  let rec crossProductRec lstlst =
    match lstlst with
    | [] -> assert false
    | [ head ] -> List.map (fun elem -> [ elem ]) head
    | head :: rest ->
        let rlists = crossProductRec rest in
        List.concat 
          (List.map (fun i -> List.map (fun r -> i :: r) rlists) head)
  in
  let empty = List.fold_left 
      (fun found lst -> 
        if found then true else ((List.length lst) = 0)) false lstlst in
  let empty = empty || (lstlst = []) in
  if empty then
    []
  else
    crossProductRec lstlst

