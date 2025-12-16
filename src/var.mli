
(** Variables *)

(** * Types *)

(** The following types are left abtract to e.g. prevent a variable name to be mixed with a type
  variable name. *)

(** A [var] denotes a variable name *)
type var = string

(** [vars] is a shorthand for a list of variables *)
type vars = var list

(** A [tconstr] is an identifier for a type constructor (e.g. [list]). *)
type tconstr

(** A [constr] is an identifier for the constructor of a type (e.g. [Either.Left]). *)
type constr

(** A [field] denotes a field name *)
type field

(** [tvar] represents a name ['t] for a (flexible) type variable.
   A type variable may have a name coming form the source code, in which
   case we reuse it, but might also be anonymous.
   This type is compared using physical equality by the typechecker: two
   different instances of [no_name_tvar] will still be considered different. *)
type tvar

(** An instance identifier. *)
(* type instance_id *)

(** A unique integer for each varid as a key for maps. *)
type varid_unique_int


val string_to_tconstr : string -> tconstr

(** * Constructors *)

val var : string -> var

val tconstr : string -> tconstr

val constr : string -> constr

val field : string -> field

(** Create a new (flexible) type variable.
  If provided, the [raw] argument is used to state how it was exactly in the source. *)
(* FIXME: Remove? It seems that flexible type variables are only introduced with [no_name_tvar]. *)
val tvar : ?raw:string -> string -> tvar

(** Allocate a new anonymous variable. *)
val no_name_var : unit -> var

(** Allocate a new anonymous type variable.
 This is typically used from within the typechecker. *)
val no_name_tvar : unit -> tvar

(** Create a new type variable, making best effort to preserve a meaningful name. *)
val merge_tvar : tvar -> tvar -> tvar
(*
val instance_id : var -> Location.t -> instance_id
 *)
(* val new_varid_unique_int : unit -> varid_unique_int
 *)

(** * Printer *)

val print_var : var -> var
val print_tconstr : tconstr -> string
val print_constr : constr -> string
val print_field : field -> string

val print_tvar : tvar -> string

(* Return the raw name of the (flexible) type variable. *)
val tvar_raw : tvar -> string option
(*
val print_instance_id : instance_id -> string
 *)
val print_varid_unique_int : varid_unique_int -> string


(** * Miscellaneous *)

(** Given a seed to inspire from and a function stating whether a name is acceptable,
  generate a new name (recalling the function until a name is accepted).
  It tries to generate readable names. *)
val new_name_from_seed : string -> (string -> bool) -> string

(** Given a variable name, suggest possible close match. *)
val suggest : string -> string list


(** The (overloaded) variable associated to a constructor. *)
val constr_to_var : constr -> var

