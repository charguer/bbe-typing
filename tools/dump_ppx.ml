open Typer_lib

let input_file = ref None
let output_file = ref None

let spec = [
  ("--output", Arg.String (fun s -> output_file := Some s), "Write generated OCaml to FILE instead of stdout");
]

let usage = "Usage: dump_ppx.exe [--output FILE] file.ml"

let parse_file filename =
  let infile = open_in filename in
  let lexbuf = Lexing.from_channel infile in
  Lexing.set_filename lexbuf filename;
  Fun.protect
    ~finally:(fun () -> close_in_noerr infile)
    (fun () -> Parse.implementation lexbuf)

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

let with_output_channel f =
  match !output_file with
  | None -> f stdout
  | Some file ->
      let out = open_out file in
      Fun.protect
        ~finally:(fun () -> close_out_noerr out)
        (fun () -> f out)

let () =
  Arg.parse spec (fun s -> input_file := Some s) usage;
  let input_file =
    match !input_file with
    | Some f -> f
    | None -> failwith usage
  in
  let structure = parse_file input_file in
  let transformed = transform_impl structure in
  with_output_channel (fun out ->
    let fmt = Format.formatter_of_out_channel out in
    Pprintast.structure fmt transformed;
    Format.pp_print_newline fmt ())
