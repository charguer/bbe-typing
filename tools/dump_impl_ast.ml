let report_err exn =
  Location.report_exception Format.std_formatter exn

let from_file parse_fun filename =
  Location.input_name := filename;
  let ic = if filename = "-" then stdin else open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf filename;
  let ast = parse_fun lexbuf in
  close_in ic;
  ast

let drop_first_items n l =
  let rec aux i l =
    if i <= 0 then l
    else
      match l with
      | [] -> []
      | _ :: l' -> aux (i - 1) l'
  in
  aux n l

let drop_first = ref 0

let spec = [
  ("--drop-first", Arg.Set_int drop_first,
    "Drop the first N structure items before printing");
]

let usage = "Usage: dump_impl_ast.exe [--drop-first N] file.ml"

let () =
  let input = ref None in
  Arg.parse spec (fun s -> input := Some s) usage;
  let input =
    match !input with
    | Some s -> s
    | None -> failwith usage
  in
  try
    let ast = from_file Parse.implementation input in
    let ast = drop_first_items !drop_first ast in
    let fmt = Format.std_formatter in
    Ocaml_common.Printast.implementation fmt ast;
    Format.pp_print_flush fmt ()
  with exn ->
    report_err exn;
    exit 1
