(* Main file. *)

open Typer_lib

(*#########################################################################*)

let no_mystd_include = ref false

let output_filename = ref None

let types_symbols, style_types = [
    "subterms" ; "all" ;
    "vars-binders" ;
    "vars" ;
    "vars-overloaded" ;
    "unresolved-overloaded" ;
    "none"
  ], let open Ast_print in function
  | "subterms" | "all" -> TypesSubterms
  | "vars-binders" -> TypesVarsAndBinders
  | "vars" -> TypesVars
  | "vars-overloaded" -> TypesVarsOverloaded
  | "unresolved-overloaded" -> TypesVarsOverloadedUnresolved
  | "none" -> TypesNone
  | _ -> assert false

let resolution_symbols, style_resolution = [
    "instance-or-symbol" ;
    "instance-or-error" ;
    "instance-or-magic" ;
    "symbol"
  ], let open Ast_print in function
  | "instance-or-symbol" -> ResolutionInstanceOrSymbol
  | "instance-or-error" -> ResolutionInstanceOrError
  | "instance-or-magic" -> ResolutionInstanceOrObjMagic
  | "symbol" -> ResolutionSymbol
  | _ -> assert false

let spec =
  Arg.align (List.map (fun (c, a, d) -> (c, a, "\t" ^ d)) [
    ("-continue-on-error", Arg.Set Flags.continue_on_error, "Skip a top-level item if it does not typecheck.") ;
    ("-clear-triggered", Arg.Set Flags.clear_triggered_after_loop, "After each resolution loop, clear the varid triggered during the iteration.") ;
    ("-debug", Arg.Set Flags.debug, "Print debugging information.") ;
    ("-print-counters", Arg.Set Flags.print_counters, "Print counters, for debugging.") ;
    ("-counters-only-for-resolutions-in-last-topdef", Arg.Set Flags.counters_only_for_resolutions_in_last_topdef, " Hack for gathering more specific statistics, for debugging.") ;
    ("-force-complete-resolution", Arg.Clear Flags.force_complete_resolution, "Stop when the resolution can't be completed.") ;
    ("-keep-failing", Arg.Clear Flags.remove_failing, "Keep definitions that are marked with a [@type_error].") ;
    ("-measure-time", Arg.Set Flags.measure_time, "Measure the type of the typing phase (ignoring parser and printer).") ;
    ("-no-output", Arg.Clear Flags.output, "Disable all output except for debugging and error.") ;
    ("-not-readable", Arg.Clear Flags.readable, "Do not try to produce code that is as readable as possible.") ;
    ("-o", Arg.String (fun s -> output_filename := Some s), "Set the output file name.") ;
    ("-only-type", Arg.Clear Flags.instantiate, "Disable replacing instance by their implementation.") ;
    ("-print-parsed", Arg.Set Flags.print_parsed, "Print raw parsing information in special *_parsed.ml and *_translated.ml files.") ;
    ("-print-raw-symbols", Arg.Set Flags.print_raw_symbols, "Print internal symbols encoding for records and constants.") ;
    ("-print-types", Arg.Symbol (types_symbols, fun s -> Flags.style_types := style_types s), "Specify where to print inferred types in the output.") ;
    ("-quiet", Arg.Set Flags.quiet, "Do not indicate when a file is being generated.") ;
    ("-relax-error-messages", Arg.Clear Flags.exact_error_messages, "Do not check wether [@type_error] annotations exactly produce the expected error message.") ;
    ("-res-full", Arg.Symbol (resolution_symbols, fun s -> Flags.style_resolution_full := style_resolution s), "Specify where how to print the main function of a fully resolved instance.") ;
    ("-res-base", Arg.Symbol (resolution_symbols, fun s -> Flags.style_resolution_base := style_resolution s), "Specify where how to print the main function of a partially resolved instance.") ;
    ("-res-args", Arg.Symbol (resolution_symbols, fun s -> Flags.style_resolution_args := style_resolution s), "Specify where how to print the assumptions of a partially resolved instance.") ;
    ("-res", Arg.Symbol (resolution_symbols, fun s ->
      let r = style_resolution s in
      Flags.style_resolution_full := r ;
      Flags.style_resolution_base := r ;
      Flags.style_resolution_args := r), "Equivalent to calling all -res-full, -res-base, and -res-args with this argument.") ;
    ("-trigger-max", Arg.Set_int Flags.max_cardinal_trigger, "Maximum cardinal of triggers associated to each varid.") ;
    ("-trigger-passes", Arg.Set_int Flags.number_of_trigger_passes, "Number of triggered varids considered at each loop iteration.")
  ])


(*#########################################################################*)
(* FOR FUTURE USE *)
(*
    ("-nostdlib", Arg.Set no_mystd_include, " do not include standard library");
    ("-nopervasives", Arg.Set Clflags.nopervasives, " do not include standard pervasives file");
    ("-I", Arg.String (fun i -> Clflags.include_dirs := i::!Clflags.include_dirs),
                      " includes a directory where to look for interface files");

    (* Then, [libdir] is obtained by appending "/lib/typer" to [basis]. *)

    let libdir =
      basis |> option_map (fun basis -> basis ^ "/lib/typer")

   if not !no_mystd_include then
     Cfml_config.libdir |> option_iter begin fun libdir ->
       Clflags.include_dirs := (libdir ^/ "stdlib") :: !Clflags.include_dirs
     end;

   if !Clflags.nopervasives
     && Filename.basename sourcefile <> "Pervasives.ml" then
      failwith "Option -nopervasives may only be used to compile file Pervasives";


*)

let _ =
   Clflags.nopervasives := true


(*#########################################################################*)

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

(*string -> string
Used as a small hack to generate a parsed file without the ".ml" extension*)
let gen_parsed inputfile =
  let basename = Filename.chop_suffix (Filename.basename inputfile) ".ml" in
  let dirname = Filename.dirname inputfile in
  Filename.concat dirname (Printf.sprintf "%s_%s.txt" basename "parsed")

let get_output_filename (inputfile : string) : string =
  match !output_filename with
  | Some f -> f
  | None -> gen_filename "typed" inputfile

let get_parsed_filename (inputfile : string) : string =
  gen_parsed inputfile

let get_parsed_and_converted_filename (inputfile : string) : string =
  gen_filename "translated" inputfile

let generating_file inputfile file : unit =
  if not !Flags.quiet then (
    let str = Printf.sprintf "Generating file %s." file in
    let str =
      if get_output_filename inputfile = "-" then
        Printf.sprintf "(* %s *)" str
      else str in
    print_endline str
  )

type ocaml_ast = Parsetree.structure

let _ =
  let inputfile = parse_command_line () in

  (* Parse *)
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

  (* TEMPORARY TEST for exploiting type
  let (ocaml_tast, _ : Typedtree.structure * Typedtree.module_coercion) =
  try
    let sourcefile = inputfile in
    let prefixname = Filename.chop_extension sourcefile in
    let modulename = String.capitalize_ascii (Filename.basename prefixname) in
    let env = Env.empty in
    Typemod.type_implementation sourcefile prefixname modulename env ocaml_ast

  with Typetexp.Error _ -> failwith "typecheck failed"
  in

  let tr_structure_item si : unit =
    match si.str_desc with
    | Tstr_value (rf, [vb]) ->
        let e = vb.vb_expr in
        begin match e.exp_desc with
        | Texp_function _ ->
            begin match e.exp_extra with
            | [ (Texp_newtype tname, _, _) ] -> failwith ("ok" ^ tname)
            | _ -> ()
            end
        | _ -> failwith "bad"
        end
    | _ -> failwith "other"
     in
  List.iter tr_structure_item ocaml_tast.str_items;
  *)

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

  let wrapper =
    if !Flags.measure_time then
      Some (fun f x ->
        let t_before = Sys.time () in
        let res = f x in
        let t_after = Sys.time () in
        Printf.printf "exectime %f\n" (t_after -. t_before);
        res)
    else None in

  let res =
    try
      Chain.full
        ~exact_error_messages:!Flags.exact_error_messages
        ~continue_on_error:!Flags.continue_on_error
        ~remove_failing:!Flags.remove_failing
        ~instantiate:!Flags.instantiate
        ~readable:!Flags.readable
        ~printing_styles:Ast_print.{
          style_types = !Flags.style_types ;
          style_resolution_full = !Flags.style_resolution_full ;
          style_resolution_base = !Flags.style_resolution_base ;
          style_resolution_args = !Flags.style_resolution_args ;
          style_debug = !Flags.style_debug ;
          style_print_symbols = !Flags.print_raw_symbols
        }
        ?wrapper
        (fun str ->
          let outputfile = get_parsed_and_converted_filename inputfile in
          generating_file inputfile outputfile ;
          let out = open_out outputfile in
          output_string out str ;
          close_out out
        )
        ocaml_ast
    with Failure e -> prerr_string e ; exit 1 in

  (* print *)
  if !Flags.output then (
    let outputfile = get_output_filename inputfile in
    if not !Flags.quiet then
      print_endline (Printf.sprintf "Typing successful. Generating file %s." outputfile) ;
    let out =
      if outputfile = "-" then stdout
      else open_out outputfile in
    output_string out res ;
    close_out out
  ) ;

  if !Flags.print_counters then
    Counters.print_counters () ;

  ()

