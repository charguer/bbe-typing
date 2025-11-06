(* TEST
 include ocamlcommon;
 readonly_files = "source.ml";
*)

(* (c) Alain Frisch / Lexifi *)
(* cf. PR#7200 *)

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

let to_string print_fun ast =
  Format.fprintf Format.str_formatter "%a@." print_fun ast;
  Format.flush_str_formatter ()

let test parse_fun print filename =
  match from_file parse_fun filename with
  | exception exn ->
      Printf.printf "%s: FAIL, CANNOT PARSE\n" filename;
      report_err exn;
      print_endline "====================================================="
  | ast ->
      let str = to_string print ast in
      print_endline str

let test parse_fun pprint filename =
  try test parse_fun pprint filename
  with exn -> report_err exn

let rec process path =
  if path <> "-" && Sys.is_directory path then
    let files = Sys.readdir path in
    Array.iter (fun s -> process (Filename.concat path s)) files
  else if path = "-" || Filename.check_suffix path ".ml" then
    test
      Parse.implementation
      (Printast.structure 2)
      path
  else if Filename.check_suffix path ".mli" then
    test
      Parse.interface
      Printast.interface
      path

let () =
  if Array.length Sys.argv < 2 then failwith "Usage: print_ast.exe file_to_print.ml" ;
  process Sys.argv.(1)

