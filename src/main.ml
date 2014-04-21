open MusynthTypes
open Printf

module Opts = MusynthOptions
module Front = MusynthFrontEnd
module Debug = MusynthDebug

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
  fprintf stderr "-dr <method>     : Enable Dynamic Reordering.\n";
  fprintf stderr "                   Where <method> is one of (defaults to linear):\n";
  List.iter 
    (fun meth ->
     fprintf stderr "                   %s\n" meth)
    Opts.reorderMethods;
  fprintf stderr "\n\n";
  ignore (exit 1)

let reorderMethOptionToMethod opt = 
  let meth = 
    match opt with
    | "random" -> Cudd.Man.REORDER_RANDOM
    | "randompivot" -> Cudd.Man.REORDER_RANDOM_PIVOT
    | "sift" -> Cudd.Man.REORDER_SIFT
    | "siftconverge" -> Cudd.Man.REORDER_SIFT_CONVERGE
    | "symmsift" -> Cudd.Man.REORDER_SYMM_SIFT
    | "symmsiftconverge" -> Cudd.Man.REORDER_SYMM_SIFT_CONV
    | "window2" -> Cudd.Man.REORDER_WINDOW2
    | "window3" -> Cudd.Man.REORDER_WINDOW3
    | "window4" -> Cudd.Man.REORDER_WINDOW4
    | "window2converge" -> Cudd.Man.REORDER_WINDOW2_CONV
    | "window3converge" -> Cudd.Man.REORDER_WINDOW3_CONV
    | "window4converge" -> Cudd.Man.REORDER_WINDOW4_CONV
    | "groupsift" -> Cudd.Man.REORDER_GROUP_SIFT
    | "groupsiftconverge" -> Cudd.Man.REORDER_GROUP_SIFT_CONV
    | "annealing" -> Cudd.Man.REORDER_ANNEALING
    | "genetic" -> Cudd.Man.REORDER_GENETIC
    | "linear" -> Cudd.Man.REORDER_LINEAR
    | "linearconverge" -> Cudd.Man.REORDER_LINEAR_CONVERGE
    | "lazysift" -> Cudd.Man.REORDER_LAZY_SIFT
    | "exact" -> Cudd.Man.REORDER_EXACT
    | _ -> raise (Invalid_argument "Not a valid reordering method")
  in
  meth

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
            | "-dr" :: dropt :: rest ->
               Opts.reorderEnabled := true;
               (try
                   let meth = reorderMethOptionToMethod dropt in
                   Opts.reorderMethod := meth;
                   rest
                 with 
                 | Invalid_argument "Not a valid reordering method" ->
                    Opts.reorderMethod := Cudd.Man.REORDER_LINEAR;
                    (dropt :: rest))
                 
            | str :: rest ->
               if !Opts.inputFileName <> "" then
                 begin
                   fprintf stderr "%s\n" !Opts.inputFileName;
                   fprintf stderr "Only one input file may be specified.\n";
                   fprintf stderr "Already have input file \"%s\".\n" !Opts.inputFileName;
                   fprintf stderr "Don't know what to do with argument \"%s\".\n" str;
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
