open MusynthTypes
open Printf

module Opts = MusynthOptions
module Front = MusynthFrontEnd
module Debug = MusynthDebug

let printUsage arg0 =
  fprintf stderr "Usage:\n";
  fprintf stderr "%s [options] filename\n" arg0;
  fprintf stderr "Permitted options:\n";
  fprintf stderr "-df <filename>   : Record debugging information into <filename>.\n";
  fprintf stderr "                   (defaults to <filename>.debug if not specified and debugging enabled)\n";
  fprintf stderr "-debug <option>+ : Options to enable logging specific debugging info.\n";
  fprintf stderr "                   Where option is one or more of the following:\n";
  fprintf stderr "                     prog   - Print the program.\n";
  fprintf stderr "                     lprog  - Print the lowered program.\n";
  fprintf stderr "                     trace  - Print counterexamples.\n";
  fprintf stderr "                     trans  - Print computed transition relations.\n";
  fprintf stderr "                     all    - Enable logging ALL debugging info.\n";
  fprintf stderr "                     bdd    - Enable logging info from the BDD manager.\n";
  fprintf stderr "                     none   - Disable logging ANY debugging info.\n";
  fprintf stderr "                     mc     - Enable logging from the model checker/synthesizer.\n";
  fprintf stderr "-tp <full|diff>  : Mode to print traces (full or delta from previous).\n";
  fprintf stderr "-j <num>         : Jump by <num> steps each iteration (default = 1, 0 = infinity).\n";
  fprintf stderr "-c               : Use conjunctive partitioning of transition relation.\n";
  fprintf stderr "-dr <method>     : Enable Dynamic Reordering.\n";
  fprintf stderr "                   Where <method> is one of (defaults to linear):\n";
  List.iter 
    (fun meth ->
     fprintf stderr "                       %s\n" meth)
    Opts.reorderMethods;
  fprintf stderr "-n               : Number of solutions to print (default 1).\n";
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

let rec processDebugOptions optlist = 
  match optlist with
  | "prog" :: rest -> 
    Opts.debugOptions := StringSet.add "prog" !Opts.debugOptions;
    Opts.debugDisabled := false;
    processDebugOptions rest
  | "lprog" :: rest -> 
    Opts.debugOptions := StringSet.add "lprog" !Opts.debugOptions; 
    Opts.debugDisabled := false;
    processDebugOptions rest
  | "trace" :: rest -> 
    Opts.debugOptions := StringSet.add "trace" !Opts.debugOptions; 
    Opts.debugDisabled := false;
    processDebugOptions rest
  | "trans" :: rest -> 
    Opts.debugOptions := StringSet.add "trans" !Opts.debugOptions; 
    Opts.debugDisabled := false;
    processDebugOptions rest
  | "all" :: rest -> 
    Opts.debugOptions := StringSet.add "all" !Opts.debugOptions; 
    Opts.debugDisabled := false;
    processDebugOptions rest
  | "bdd" :: rest -> 
    Opts.debugOptions := StringSet.add "bdd" !Opts.debugOptions; 
    Opts.debugDisabled := false;
    processDebugOptions rest
  | "mc" :: rest -> 
    Opts.debugOptions := StringSet.add "mc" !Opts.debugOptions; 
    Opts.debugDisabled := false;
    processDebugOptions rest
  | "ltl" :: rest -> 
    Opts.debugOptions := StringSet.add "ltl" !Opts.debugOptions; 
    Opts.debugDisabled := false;
    processDebugOptions rest
  | "none" :: rest -> 
    Opts.debugOptions := StringSet.add "none" !Opts.debugOptions; 
    Opts.debugDisabled := true; 
    processDebugOptions rest
  | _ -> optlist

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
            | "-debug" :: rest ->
              processDebugOptions rest
            | "-j" :: intval :: rest ->
              Opts.jumpStep := (int_of_string intval);
              if (!Opts.jumpStep = 0) then
                Opts.jumpStep := max_int
              else
                ();
              rest
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
                 Opts.reorderMethod := Cudd.Man.REORDER_SYMM_SIFT;
                 (dropt :: rest))
            | "-tp" :: "diff" :: rest ->
              Opts.tracePrintMode := "diff"; rest
            | "-tp" :: "full" :: rest -> 
              Opts.tracePrintMode := "full"; rest
            | "-tp" :: rest ->
              raise (Invalid_argument "Unknown trace printing mode!")
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
