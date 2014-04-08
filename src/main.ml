open MusynthFrontEnd
open MusynthTypes

let debug = ref true

let _ =
  if !debug then Printexc.record_backtrace true else ();
  let filename = 
  try 
    Some (Sys.argv.(1))
  with
  | Invalid_argument _ ->
      None
  in
  try
    musynthProcess filename
  with
  | _ as ex ->
      Printf.fprintf stderr "Exception: %s\n" (exToString ex);
      Printf.fprintf stderr "Backtrace:\n%s" (Printexc.get_backtrace ())
