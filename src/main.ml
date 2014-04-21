open MusynthTypes
open Printf

module Opts = MusynthOptions
module Front = MusynthFrontEnd
module Debug = MusynthDebug

let debug = ref true

let printUsage arg0 =
  fprintf stderr "Usage:\n";
  fprintf stderr "%s [options] filename\n" arg0;
  fprintf stderr "Permitted options:\n";
  fprintf stderr ("-v <num >= 0>   : Control verbosity. Output will be in \"<filename>.debug\" by default. " ^^ 
                    "Use the -df option to specify a different filename.\n");
  fprintf stderr "-df <filename>   : Record debugging information into <filename>.\n";
  fprintf stderr "-f <strong|weak> : Type of fairness to enforce.\n";
  fprintf stderr "-s               : Only synthesize for safety properties.\n";
  fprintf stderr "-c               : Use conjunctive partitioning of transition relation.\n";
  fprintf stderr "-n               : Number of solutions to print (default 1).\n";
  fprintf stderr "\n\n";
  ignore (exit 1)
          

let processOptions arglist =
  let arg0 = List.hd arglist in
  let arglist = List.tl arglist in

  let rec processOptionsRec arglist = 
    if arglist = [] then
      ()
    else
      begin
        let rest = 
          begin
            match arglist with
            | [] -> []
            | "-v" :: num :: rest ->
               begin
                 try
                   let dlevel = int_of_string num in
                   Opts.debugLevel := dlevel
                 with Failure "int_of_string" ->
                 begin
                   fprintf stderr "Expected an integer argument after -v\n";
                   printUsage arg0
                 end
               end;
               rest
            | "-f" :: ftype :: rest ->
               if ftype = "weak" then
                 Opts.fairnessType := FairnessTypeWeak
               else if ftype = "strong" then
                 Opts.fairnessType := FairnessTypeStrong
               else
                 begin
                   fprintf stderr "Fairnesstype must be \"weak\" or \"strong\" (without quotes)\n";
                   printUsage arg0
                 end;
               rest
            | "-s" :: rest ->
               Opts.onlySafety := true; rest
            | "-c" :: rest ->
               Opts.conjunctivePart := true; rest
            | "-n" :: num :: rest ->
               Opts.numSolsRequested := (int_of_string num); rest
            | "-df" :: fname :: rest ->
               Opts.debugFileName := fname; rest
            | str :: rest ->
               if !Opts.inputFileName <> "" then
                 begin
                   fprintf stderr "%s\n" !Opts.inputFileName;
                   fprintf stderr "Only one input file may be specified\n";
                   printUsage arg0
                 end
               else
                 Opts.inputFileName := str;
               rest
          end
        in
        processOptionsRec rest
      end
  in
  processOptionsRec arglist

let _ =
  Printexc.record_backtrace true;
  let arglist = Array.to_list Sys.argv in
  processOptions arglist;
  
  let filename = !Opts.inputFileName in
  if filename = "" then
    begin
      fprintf stderr "Input file name not specified!\n";
      printUsage (List.hd arglist)
    end
  else
    begin
      Debug.initDebugSubsys (if !Opts.debugFileName <> "" then 
                               !Opts.debugFileName 
                             else 
                               (filename ^ ".debug"));
      try
        Front.musynthProcess (Some filename);
        Debug.shutDownDebugSubsys ()
      with
      | _ as ex ->
         Printf.fprintf stderr "Exception: %s\n" (exToString ex);
         Printf.fprintf stderr "Backtrace:\n%s" (Printexc.get_backtrace ());
         Debug.shutDownDebugSubsys ()
    end
