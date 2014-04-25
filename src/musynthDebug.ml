open MusynthTypes
open Format

module Opts = MusynthOptions

let debugOC = ref None
let debugFmt = ref None

let debugEnabled () =
  (((StringSet.cardinal !Opts.debugOptions) > 0) && 
     (not !Opts.debugDisabled))

let debugOptEnabled name =
  ((not !Opts.debugDisabled) &&
     ((StringSet.mem name !Opts.debugOptions) ||
        (StringSet.mem "all" !Opts.debugOptions)))

let getDebugFmt () =
  match !debugFmt with
  | Some fmt -> fmt
  | None -> assert false

let initDebugSubsys debugFileName =
  if (debugEnabled ()) then
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
let dprintf debugOpt =
  if debugOptEnabled debugOpt then
    fprintf (getDebugFmt ())
  else
    ifprintf std_formatter

let dflush () =
  match !debugFmt with
  | Some fmt -> fprintf fmt "@]"; pp_print_flush fmt (); fprintf fmt "@[<v 0>"
  | None -> ()
