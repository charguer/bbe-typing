
type ocaml_ast = Parsetree.structure

(** Convert an OCaml program into our own AST (Ast.trm).
  In particular the special [__overload] and [__instance] constructs
  (recognised as simple identifiers by OCaml's parser) will be
  converted into the corresponding constructs. *)
val tr_structure : ocaml_ast -> Ast_fix.topdefs

