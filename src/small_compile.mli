open Ast_fix

(* Small compilation phases, typically meant to be run after the type-checking phase. *)

(* Replacing an already-typed term with normal definitions instead of instances. *)
val instantiate : program -> program

(* Removing terms marked as failing. *)
val remove_failing : program -> program

(* Removing all [trm_typ] values except: the ones on let-bindings, pattern matchings, and
  functions (as they might be necessary to type-check the whole term).
  The non-[Ptyp_any] [Trm_annot] annotations are kept in place as these were present in
  the source and probably wanted by the user. *)
val remove_unnecessary_type_annotations : program -> program

(* Inline mosts of the simple expressions introduced by the [instantiate] function.
  The ones that won't be inlined will be because of a shadowing that might cause the program
  not to work as intended.
  Also rewrites simple computations like [float_of_int 1] to [1.0]. *)
(* val inline_simple : program -> program
 *)
(* Force the explicit marking of the type of the object being matched on a pattern-matching,
  to avoid issues with overloaded constructors.
  By default, it uses heuristics to sometimes avoid adding a type where it doesn't add any
  information. If the boolean [always] is set to [true], the type will always be added. *)
val add_type_on_match : ?always:bool -> program -> program

(* Some external declaration actually come from the OCaml standard library.
  This function unfold them. *)
val unfold_ocaml_external : program -> program

