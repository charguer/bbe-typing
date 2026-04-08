open Typer_lib

let input_file = ref None
let output_dir = ref "."

let spec = [
  ("--output-dir", Arg.Set_string output_dir, "Directory where generated_ast.txt and generated_ml.ml are written");
]

let usage = "Usage: generate_artifacts.exe [--output-dir DIR] file.ml"

let parse_file filename =
  let infile = open_in filename in
  let lexbuf = Lexing.from_channel infile in
  Lexing.set_filename lexbuf filename;
  let ast =
    try Parse.implementation lexbuf
    with exn ->
      close_in_noerr infile;
      raise exn
  in
  close_in infile;
  ast

let transform_impl (str : Parsetree.structure) : Parsetree.structure =
  let ast : Ast_fix.program = Ocaml_to_ast.tr_structure str in
  let ast =
    Chain.chain
      ~exact_error_messages:!Flags.exact_error_messages
      ~continue_on_error:!Flags.continue_on_error
      ~remove_failing:!Flags.remove_failing
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
      ast
  in
  let compiled_ast = Ast_comp.comp_program ast in
  Ast_expand.expand_program compiled_ast

let () =
  Arg.parse spec (fun s -> input_file := Some s) usage;
  let input_file =
    match !input_file with
    | Some f -> f
    | None -> failwith usage
  in
  let structure = parse_file input_file in
  let cwd = Sys.getcwd () in
  Sys.chdir !output_dir;
  Fun.protect
    ~finally:(fun () -> Sys.chdir cwd)
    (fun () ->
      ignore (transform_impl structure))
