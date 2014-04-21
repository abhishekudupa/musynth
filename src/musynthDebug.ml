open MusynthTypes
open Format

module Opts = MusynthOptions

let debugOC = ref None
let debugFmt = ref None

let getDebugFmt () =
  match !debugFmt with
  | Some fmt -> fmt
  | None -> assert false

let initDebugSubsys debugFileName =
  if (!Opts.debugLevel >= 0) then
    let oc = open_out debugFileName in
    debugOC := Some oc;
    let fmt = formatter_of_out_channel oc in
    debugFmt := Some fmt;
    (* open the first box anyway *)
    fprintf fmt "@[<v 0>"
  else
    ()

let shutDownDebugSubsys () =
  begin
    match !debugFmt with
    | Some fmt -> 
       fprintf fmt "@]";
       pp_print_flush fmt ();
       debugFmt := None
    | None -> ()
  end;
  match !debugOC with
  | Some f -> 
     close_out f;
     debugOC := None
  | None -> ()
  

(* functions for debug printing, etc. *)
let dprintf debugLevel =
  if debugLevel < 0 then
    ifprintf std_formatter
  else if debugLevel > !Opts.debugLevel then
    ifprintf std_formatter
  else
    begin
      let fmt = getDebugFmt () in
      fprintf fmt
    end

let dflush () =
  match !debugFmt with
  | Some fmt -> fprintf fmt "@]"; pp_print_flush fmt (); fprintf fmt "@[<v 0>"
  | None -> ()
