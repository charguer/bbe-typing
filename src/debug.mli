open Var
open Ast_fix

(** Whether we are currently in debug mode. *)
val debug : unit -> bool

(** Printing an error. *)
val log : ('a, unit, string, unit) format4 -> 'a

(** Debugging functions, to log information. *)

val env_add_tvar_rigid : tvar_rigid -> typ -> unit
val env_add_tconstr : tconstr -> typ -> unit
val env_add_item : style:Ast_print.style -> var -> env_item -> bool -> unit
val typecheck_up_start : style:Ast_print.style -> typ option -> trm -> unit
val typecheck_end : style:Ast_print.style -> trm -> unit
val typecheck_down_start : style:Ast_print.style -> typ -> trm -> unit

(** A low-level print, displaying the OCaml constructors, for debugging. *)
val print_low_level_typ : typ -> string
val print_low_level_trm : trm -> string

(** experimental *)
val print_low_level_program : program -> string

(** Print the types defined in the current environment. *)
val print_env_debug : env -> string

(** Print varid identifiers, to help debugging. *)
val print_current_top_level_resolving : varid -> unit
val print_current_top_level_resolved : varid -> varid list -> unit