
let chain
  ~exact_error_messages
  ~continue_on_error
  ~remove_failing
  (* ~instantiate *)
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
  (* let ast =
    if instantiate then Small_compile.instantiate ast
    else ast in *)

  (* Making things readable. *)
  let ast =
    if readable then (
      (*let ast = Small_compile.inline_simple ast in*) (* FIXME: Temporary disabled as it seems that it has issues. *)
      (*let ast = Small_compile.remove_unnecessary_type_annotations ast in*)
      ast
    ) else ast in

  (* Making things compile by pluggin the external symbols into OCaml's internals. *)
  (* let ast =
    if instantiate then Small_compile.unfold_ocaml_external ast
    else ast in *)

  (* Avoid issues with overloaded constructors in OCaml by enforcing the type on pattern-matching. *)
(*   let ast = Small_compile.add_type_on_match ~always:(not readable) ast in
 *)
  ast

let gen_compiled inputfile =
  let basename = Filename.chop_suffix (Filename.basename inputfile) ".ml" in
  let dirname = Filename.dirname inputfile in
  Filename.concat dirname (Printf.sprintf "%s_%s.ml" basename "compiled")
let get_compiled_filename (inputfile : string) : string =
  gen_compiled inputfile


let full
  ~exact_error_messages
  ~continue_on_error
  ~remove_failing
  (* ~instantiate *)
  ~readable
  ~printing_styles
  ?(input_name = "-")
  ?(wrapper = fun f x -> f x)
  call_back_syntax
  untyped_input_ocamlast =

  (* TODO YL:
     typed / untyped
     input / compiled
     ocamlast / bbeast
      *)

  let untyped_input_bbeast : Ast_fix.program = Ocaml_to_ast.tr_structure untyped_input_ocamlast in

  let typed_input_bbeast =
    wrapper
      (chain
        ~exact_error_messages
        ~continue_on_error
        ~remove_failing
        (* ~instantiate *)
        ~readable
        ~printing_styles)
        untyped_input_bbeast in

  if !Flags.print_parsed then (
    let res = Ast_print.to_string ~style:{
      printing_styles with
      style_resolution_full = ResolutionInstanceOrSymbol ;
      style_resolution_base = ResolutionInstanceOrSymbol ;
      style_resolution_args = ResolutionInstanceOrSymbol ;
      style_types = TypesVarsAndBinders ;
      style_print_symbols = true ;
      style_binds = !Flags.style_binds ;
    } typed_input_bbeast in
    if !Flags.verbose then
      begin
        Printf.printf "Raw ast :\n%s\n"
          (Debug.print_low_level_program typed_input_bbeast);
        Printf.printf "Readable ast :\n%s\n" res;
      end;
    call_back_syntax res
  ) ;


  (* simplified ast (compilation) *)
  let typed_compiled_bbeast =
    if !Flags.recompile then
      wrapper (Ast_comp.comp_program) typed_input_bbeast
    else []
  in

  (* OCaml ast *)
  let typed_compiled_ocamlast : Parsetree.structure =
    if !Flags.recompile && !Flags.expand then
        Ast_expand.expand_program typed_compiled_bbeast
    else []
  in

  (*
  let out_str_compiled = Ast_print.to_string ~style:printing_styles compiled_ast in

   if !Flags.debug && !Flags.verbose then (
    let outputfile = get_compiled_filename "test/unit_tests_bbe.ml" in
    if not !Flags.quiet then
      print_endline (Printf.sprintf "Compilation successful. Generating file %s." outputfile) ;
    let out =
      if outputfile = "-" then stdout
      else open_out outputfile in
    output_string out out_str_compiled ;
    close_out out
  ) ; *)

  (*  *)
  let typed_compiled_bbeast =
    if !Flags.recompile && false then
    Some (wrapper
        (chain
          ~exact_error_messages
          ~continue_on_error
          ~remove_failing
          (* ~instantiate *)
          ~readable
          ~printing_styles)
          typed_compiled_bbeast)
    else None
  in

  let out_str_ocamlast =
    if !Flags.recompile && !Flags.expand then
      begin
        let result = Printf.sprintf "%s" (Pprintast.string_of_structure typed_compiled_ocamlast) in
        if !Flags.verbose then Printf.printf "result : %s\n" result;
        result
      end
    else ""
  in

  (* Print *)
  let out_str_typed = Ast_print.to_string ~style:printing_styles typed_input_bbeast in
  (* The ast is either retyped, or not.  *)
  let out_str_compiled =
    Option.fold ~none:"" ~some:(Ast_print.to_string ~style:printing_styles) typed_compiled_bbeast
  in


(*
  let out_str_transformed = "" in
 *)
  let out_str_typed =
    if readable then (
      let open Ocamlformat_lib in
      let conf = Conf.default in
      match
        Translation_unit.parse_and_format Syntax.Use_file conf
          ~input_name
          ~source:out_str_typed
      with
      | Ok formatted -> formatted
      | Error e ->
        let buffer = Buffer.create 160 in
        Translation_unit.Error.print (Format.formatter_of_buffer buffer) e ;
        failwith ("Formating error: " ^ Buffer.contents buffer)
    ) else out_str_typed
  in
  (out_str_typed, out_str_compiled, out_str_ocamlast)

