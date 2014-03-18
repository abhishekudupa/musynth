open MusynthFrontEnd

let _ =
  try 
    let filename = Sys.argv.(1) in
    musynthParse (Some filename)
  with
  | Invalid_argument _ ->
      musynthParse None
