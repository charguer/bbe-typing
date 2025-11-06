
(* Perform the typing and compilation chain with these parameters. *)
val chain :
  exact_error_messages:bool ->
  continue_on_error:bool ->
  remove_failing:bool ->
  instantiate:bool ->
  readable:bool ->
  printing_styles:Ast_print.style ->
  Ast_fix.program ->
  Ast_fix.program

(* Same, but with the full chain, from an OCaml AST to an output string.
 The call-back function is used in order to print the raw parsed and converted AST, if needed.
 The wrapper is called with the [chain] function above and its argument, and can be used for
 instance to measure time. *)
val full :
  exact_error_messages:bool ->
  continue_on_error:bool ->
  remove_failing:bool ->
  instantiate:bool ->
  readable:bool ->
  printing_styles:Ast_print.style ->
  ?input_name:string ->
  ?wrapper:((Ast_fix.program -> Ast_fix.program) -> (Ast_fix.program -> Ast_fix.program)) ->
  (string -> unit) ->
  Ocaml_to_ast.ocaml_ast ->
  string

