
open Ast_fix


(** Error raised by the typer. *)
exception Typecheck_error of string

(** Error raised when no progress is made after a full pass of resolution.
  This means that there lack information to fully type-check the current program. *)
exception IncompleteResolution


(* Each of these type-checking function take a term/program and return it typechecked.
  The parser initially set-up dummy values here and there, hence the non-unit return type. *)

val typecheck_program : ?exact_error_messages:bool -> ?continue_on_error:bool -> style:Ast_print.style -> program -> program

val typecheck_ml : ?expected_typ:typ -> env -> trm -> trm

val ordered_resolution : ?max_traversals:int -> trm -> bool * int

