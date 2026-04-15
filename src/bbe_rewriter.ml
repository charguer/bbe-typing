(* Main file. *)

open Typer_lib

(*#########################################################################*)

let output_filename = ref None

let spec =
  Arg.align (List.map (fun (c, a, d) -> (c, a, "\t" ^ d)) [
    ("-continue-on-error", Arg.Set Flags.continue_on_error, "Skip a top-level item if it does not typecheck.") ;
    (*("-clear-triggered", Arg.Set Flags.clear_triggered_after_loop, "After each resolution loop, clear the varid triggered during the iteration.") ; *)
    ("-debug", Arg.Set Flags.debug, "Print debugging information.") ;
    ("-print-counters", Arg.Set Flags.print_counters, "Print counters, for debugging.") ;
    ("-counters-only-for-resolutions-in-last-topdef", Arg.Set Flags.counters_only_for_resolutions_in_last_topdef, " Hack for gathering more specific statistics, for debugging.") ;
    ("-force-complete-resolution", Arg.Clear Flags.force_complete_resolution, "Stop when the resolution can't be completed.") ;
    ("-keep-failing", Arg.Clear Flags.remove_failing, "Keep definitions that are marked with a [@type_error].") ;
    ("-measure-time", Arg.Set Flags.measure_time, "Measure the type of the typing phase (ignoring parser and printer).") ;
    ("-no-output", Arg.Clear Flags.output, "Disable all output except for debugging and error.") ;
    ("-not-readable", Arg.Clear Flags.readable, "Do not try to produce code that is as readable as possible.") ;
    ("-o", Arg.String (fun s -> output_filename := Some s), "Set the output file name.") ;
    (* ("-only-type", Arg.Clear Flags.instantiate, "Disable replacing instance by their implementation.") ; *)
    ("-print-parsed", Arg.Set Flags.print_parsed, "Print raw parsing information in special *_parsed.ml and *_translated.ml files.") ;
    ("-print-raw-symbols", Arg.Set Flags.print_raw_symbols, "Print internal symbols encoding for records and constants.") ;
    ("-quiet", Arg.Set Flags.quiet, "Do not indicate when a file is being generated.") ;
    ("-relax-error-messages", Arg.Clear Flags.exact_error_messages, "Do not check wether [@type_error] annotations exactly produce the expected error message.") ;
    ("-weak-typer", Arg.Set Flags.weak_typer, "(default) Remove most of the typechecking steps") ;
    ("-strong-typer", Arg.Clear Flags.weak_typer, "Performs the complete typechecking process") ;
  ])

(*#########################################################################*)


let _ =
   Clflags.nopervasives := true;
   Flags.weak_typer := true;
   Flags.recompile := true;
   Flags.expand := true

let parse_command_line () : string =
   let files = ref [] in
   Arg.parse
     spec
     (fun f -> files := f::!files)
     ("usage: [-I dir] [..other options..] file.ml");
   if !Flags.debug
     then Printf.printf "debug flag on\n";
   if !Flags.continue_on_error
     then Printf.printf "mode continue_on_error\n";
   if List.length !files <> 1
     then failwith "Expects one argument: the filename of the ML source file";
   let sourcefile = List.hd !files in
   if not (Filename.check_suffix sourcefile ".ml") then
     failwith "The file name must be of the form *.ml";
   sourcefile

let gen_filename kind inputfile =
  let basename = Filename.chop_suffix (Filename.basename inputfile) ".ml" in
  let dirname = Filename.dirname inputfile in
  Filename.concat dirname (Printf.sprintf "%s_%s.ml" basename kind)

let gen_parsed inputfile = gen_filename "parsed" inputfile
let gen_compiled inputfile = gen_filename "compiled" inputfile
let gen_expanded inputfile = gen_filename "expanded" inputfile
let gen_typed inputfile = gen_filename "typed" inputfile

let get_parsed_filename (inputfile : string) : string =
  gen_parsed inputfile

let get_typed_filename (inputfile : string) : string =
  gen_typed inputfile

let get_compiled_filename (inputfile : string) : string =
  gen_compiled inputfile

let get_expanded_filename (inputfile : string) : string =
  gen_expanded inputfile

let get_output_filename (inputfile : string) : string =
  match !output_filename with
  | Some f -> f
  | None -> gen_filename "rewritten" inputfile

let generating_file inputfile file : unit =
  if not !Flags.quiet then (
    let str = Printf.sprintf "Generating file %s." file in
    let str =
      if get_output_filename inputfile = "-" then
        Printf.sprintf "(* %s *)" str
      else str in
    print_endline str
  )

(* /// *)

type ocaml_ast = Parsetree.structure

let _ =
  let inputfile = parse_command_line () in

  (* Parse OCaml file*)
  let infile = open_in inputfile in
  let lexbuf = Lexing.from_channel infile in
  Lexing.set_filename lexbuf inputfile ;
  let ocaml_ast : ocaml_ast =
    try Parse.implementation lexbuf
    with
    | Lexer.Error (_err, loc) ->
      Printf.printf "Lexer error in %s. Debug using: ocamlc %s\n"
        (Ast_print.print_loc loc) inputfile;
      exit 1
    | Syntaxerr.Error err ->
      let loc = Syntaxerr.location_of_error err in
      Printf.printf "Syntax error in %s. Debug using: ocamlc %s\n"
        (Ast_print.print_loc loc) inputfile;
      exit 1
    in

  Clflags.locations := false;

  (* Print raw ast *)
  if !Flags.print_parsed && !Flags.output then (
    let outputfile = get_parsed_filename inputfile in
    generating_file inputfile outputfile ;
    let outfile = open_out outputfile in
    let ppf = Format.formatter_of_out_channel outfile in
    Printast.implementation ppf ocaml_ast;
    close_out outfile
  ) ;

  (* For benchmarks *)
  let wrapper =
    if !Flags.measure_time then
      Some (fun f x ->
        let t_before = Sys.time () in
        let res = f x in
        let t_after = Sys.time () in
        Printf.printf "exectime %f\n" (t_after -. t_before);
        res)
    else None in


  let (typed_res, compiled_res, expanded_res)  =
    try
      Chain.full
        ~exact_error_messages:!Flags.exact_error_messages
        ~continue_on_error:!Flags.continue_on_error
        ~remove_failing:!Flags.remove_failing
        (* ~instantiate:!Flags.instantiate *)
        ~readable:!Flags.readable
        ~printing_styles:Ast_print.{
          style_types = !Flags.style_types ;
          style_resolution_full = !Flags.style_resolution_full ;
          style_resolution_base = !Flags.style_resolution_base ;
          style_resolution_args = !Flags.style_resolution_args ;
          style_debug = !Flags.style_debug ;
          style_print_symbols = !Flags.print_raw_symbols ;
          style_binds = !Flags.style_binds
        }
        ?wrapper
        (fun str -> ()
        (*
          let outputfile = get_parsed_and_converted_filename inputfile in
          generating_file inputfile outputfile ;
          let out = open_out outputfile in
          output_string out str ;
          close_out out *)
        )
        ocaml_ast
    with Failure e -> prerr_string e ; exit 1 in

  (* print *)
  if !Flags.output then (
    let outputfile = get_output_filename inputfile in
    if not !Flags.quiet then
      print_endline (Printf.sprintf "Translation successful. Generating file %s." outputfile) ;
    let out =
      if outputfile = "-" then stdout
      else open_out outputfile in
    output_string out expanded_res;
    close_out out
  ) ;

  if !Flags.output && not (!Flags.weak_typer) then (
    let outputfile = get_typed_filename inputfile in
    if not !Flags.quiet then
      print_endline (Printf.sprintf "Typing successful. Generating file %s." outputfile) ;
    let out =
      if outputfile = "-" then stdout
      else open_out outputfile in
    output_string out typed_res ;
    close_out out
  ) ;

  if !Flags.print_counters then
    Counters.print_counters () ;

  ()
