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

module StringMap = Map.Make
    (struct
      type t = string
      let compare = Pervasives.compare
    end)

let makeEmptyMS () =
  StringMap.empty

let addToMS elem ms =
  let count =
    (try
      StringMap.find elem ms
    with
    | Not_found -> 0) 
  in
  StringMap.add elem (count + 1) ms

let rec splatList elem count =
  match count with
  | 0 -> []
  | n -> elem :: splatList elem (n - 1)

let msToList ms =
  let retval = ref [] in
  StringMap.iter 
    (fun key count -> 
      retval := !retval @ (splatList key count)) ms;
  !retval
  
