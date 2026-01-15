(* Web interface. *)

open Js_of_ocaml
open Typer_lib

let parse str =
  let str = Js.to_string str in
  let lexbuf = Lexing.from_string str in
  lexbuf.Lexing.lex_curr_p <- {
    Lexing.pos_fname = "" ;
    Lexing.pos_lnum = 1 ;
    Lexing.pos_bol = 0 ;
    Lexing.pos_cnum = 0
  } ;
  match Parse.implementation lexbuf with
  | ast -> ast
  | exception Syntaxerr.Error err ->
    let loc = Syntaxerr.location_of_error err in
    failwith (
      Printf.sprintf "Syntax error in %s. Debug using ocamlc."
        (Ast_print.print_loc loc))

let _ =
  Js.export "typer"
    (object%js

      method compile str continue_on_error remove_failing (* instantiate *) readable =
        (* Parse *)
        let ast = parse str in
        let (res, _, _) =
          Chain.full
            ~exact_error_messages:false
            ~continue_on_error:(Js.to_bool continue_on_error)
            ~remove_failing:(Js.to_bool remove_failing)
            (* ~instantiate:(Js.to_bool instantiate) *)
            ~readable:(Js.to_bool readable)
            ~printing_styles:Ast_print.{
              style_types = !Flags.style_types ;
              style_resolution_full = !Flags.style_resolution_full ;
              style_resolution_base = !Flags.style_resolution_base ;
              style_resolution_args = !Flags.style_resolution_args ;
              style_debug = !Flags.style_debug ;
              style_print_symbols = !Flags.print_raw_symbols ;
              style_binds = !Flags.style_binds
            }
            (fun _str -> ())
            ast in
        Js.string res

    end)

