open Typer_lib
open Ppxlib
open Parsetree
open Ast_fix

let _ =
   Flags.weak_typer := true;
   Flags.recompile := true;
   Flags.expand := true

let transform_impl (str : structure) : structure =

  let ast : program = Ocaml_to_ast.tr_structure str in

  let ast =
      (Chain.chain
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
        })
        ast in

  let compiled_ast = Ast_comp.comp_program ast in

  Ast_expand.expand_program compiled_ast

let () =
  Driver.register_transformation
    ~impl:transform_impl
    "ppx_bbe"
