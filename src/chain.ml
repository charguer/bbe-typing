
let chain
  ~exact_error_messages
  ~continue_on_error
  ~remove_failing
  ~instantiate
  ~readable
  ~printing_styles
  ast =

  (* As we are manipulating mutable fields within the AST, we want to enforce that there
    is no sharing within it. *)
  let ast = Ast_aux.program_clone ast in

  let ast =
    try
      Typecheck.typecheck_program
        ~exact_error_messages
        ~continue_on_error
        ~style:printing_styles
        ast
    with Typecheck.Typecheck_error msg when not (!Flags.halt_on_error) ->
      prerr_endline (Printf.sprintf "🟥 Type error: %s" msg) ;
      exit 1 in

  (* Removing lines meant to fail. *)
  let ast =
    if remove_failing then Small_compile.remove_failing ast
    else ast in

  (* Instantiate *)
  let ast =
    if instantiate then Small_compile.instantiate ast
    else ast in

  (* Making things readable. *)
  let ast =
    if readable then (
      (*let ast = Small_compile.inline_simple ast in*) (* FIXME: Temporary disabled as it seems that it has issues. *)
      (*let ast = Small_compile.remove_unnecessary_type_annotations ast in*)
      ast
    ) else ast in

  (* Making things compile by pluggin the external symbols into OCaml's internals. *)
  let ast =
    if instantiate then Small_compile.unfold_ocaml_external ast
    else ast in

  (* Avoid issues with overloaded constructors in OCaml by enforcing the type on pattern-matching. *)
  let ast = Small_compile.add_type_on_match ~always:(not readable) ast in

  ast

let full
  ~exact_error_messages
  ~continue_on_error
  ~remove_failing
  ~instantiate
  ~readable
  ~printing_styles
  ?(input_name = "-")
  ?(wrapper = fun f x -> f x)
  call_back_syntax
  ocaml_ast =

  (* Convert *)
  let ast : Ast_fix.program = Ocaml_to_ast.tr_structure ocaml_ast in

  if !Flags.debug then Printf.printf ("%s\n") (Debug.print_low_level_program ast);

  Debug.log "Tuples declared: %s."
    (String.concat ", " (List.map string_of_int (Ast_aux.all_seen_tuple_arity ()))) ;

  if !Flags.print_parsed then (
    let res = Ast_print.to_string ~style:{ (*Modify for BBE typing information, useful for debugging*)
      printing_styles with
      style_resolution_full = ResolutionInstanceOrSymbol ;
      style_resolution_base = ResolutionInstanceOrSymbol ;
      style_resolution_args = ResolutionInstanceOrSymbol ;
      style_types = TypesVarsAndBinders ;
      style_print_symbols = true
    } ast in
    if !Flags.verbose then
      Printf.printf "Raw ast :\n%s\n"
      (Debug.print_low_level_program ast);
      Printf.printf "Readable ast :\n%s\n" res;
    call_back_syntax res
  ) ;

  let ast =
    wrapper
      (chain
        ~exact_error_messages
        ~continue_on_error
        ~remove_failing
        ~instantiate
        ~readable
        ~printing_styles)
        ast in

  (* Print *)
  let out_str = Ast_print.to_string ~style:printing_styles ast in

  if readable then (
    let open Ocamlformat_lib in
    let conf = Conf.default in
    match
      Translation_unit.parse_and_format Syntax.Use_file conf
        ~input_name
        ~source:out_str
    with
    | Ok formatted -> formatted
    | Error e ->
      let buffer = Buffer.create 160 in
      Translation_unit.Error.print (Format.formatter_of_buffer buffer) e ;
      failwith ("Formating error: " ^ Buffer.contents buffer)
  ) else out_str

