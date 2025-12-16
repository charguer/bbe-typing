
open Var
open Ast_fix

(* Basic blocks to build typecheckers. *)

(*#########################################################################*)
(* ** Testing for cycles, and testing for partially/fully resolved types *)

(** Check whether a varid is already resolved. *)
(* val varid_is_resolved : varid -> bool
 *)
(* [check_fully_typed t x] checks that the term [t] is fully typed, in
   the sense that all overloading is resolved and no flexible variable
   remains. If not, produces an error message mentioning the name [x].
   It always checks in depth that all varids are fully resolved.
   If in_depth is true, it also checks in depths that no flexible is present. *)
(* val check_fully_typed : in_depth:bool -> test_acylic:bool-> trm -> symbol -> unit

 *)
(*#########################################################################*)
(* ** Instantiation of polymorphic types *)

module RigidMap : Map.S with type key = tvar_rigid

(** [apply_to_fresh_flexibles sch] takes a type scheme, and creates a copy of
    its body with fresh type variables. E.g.  [forall 'a. 'a -> 'a list] becomes
    [?v -> ?v list], where [?v] denotes a fresh flexible variable. *)
val apply_to_fresh_flexibles : sch -> typ

(** [replace_rigids_with m ty] is used when instantiating a type scheme whose
    body is described by [ty], and whose "forall" variables are the keys of
    the list [m].
    The occurence of these variables are replaced by the values from the map [m],
    throughout the structure of the type [ty]. *)
val replace_rigids_with : typ RigidMap.t -> typ -> typ

(** Assuming that the type [ty] corresponds to a record type in the environment [e],
   return the instantiated types for each of the record fields (that is, the rigid variables
   will be replaced by the associated type in argument of the record type. *)
(* val get_record_types : ?loc:loc -> env -> typ -> string -> (field * typ) list
 *)

(*#########################################################################*)
(* ** Unification of two types *)

(** [unify_or_error env t1 t2 m] attempts to unify the two types. Upon success,
    the changes made are kept. Upon failure, the changes made are undone,
    and the error [m] is raised. *)
val unify_or_error : ?loc:loc -> env -> typ -> typ -> Errors.error -> unit

(** [try_unify env t1 t2] attempts to unify the two types. Upon success, it returns [true].
    Upon failures, all modifications are rolled back, and the function returns [false]. *)
val try_unify : env -> typ -> typ -> bool

(** [unifys_or_error env t1s t2s m] attempts to unify the two list of types.
    Upon success, the changes made are kept. Upon failure, the changes made
    are undone, and the error [m] is raised. *)
val unifys_or_error : ?loc:loc -> env -> typs -> typs -> Errors.error (* LATER: A function to build the error instead of an error, to avoid computing it every time. *) -> unit

(** [try_unifys env t1s t2s] attempts to perform the unification of two types in
    two lists, pairwise. Upon success of all unifications, it returns [true].
    Upon failures, modifications are rolled back, and the function returns [false].
    The two lists must have the same size. *)
val try_unifys : env -> typs -> typs -> bool

(** [is_instance_unifiable env t1 t2] returns a boolean indicating whether the two types are
    unifiable, but does not leave any modification visible. This function includes a check
    that the two types unify without introducing a cyclic type. *)
val is_instance_unifiable : env -> typ -> typ -> bool


(*#########################################################################*)
(* ** Instance resolution *)

(** Given an instance and a type compatible with this instance, unify the type
  with the instance and return the list of varid corresponding to its assumptions. *)
(* val unify_with_instance : loc:loc -> env -> typ -> depth:int -> context:symbol -> instance -> assumptions
 *)

(*#########################################################################*)
(* ** Typechecking of function arguments *)

(** [get_typ_for_arg v] returns the type of a function argument
    [v], which may carry a syntactic annotation. *)
val get_typ_for_arg : varsyntyp -> typ

(** [get_typ_for_arg_with_expected_type env v] returns the type of a function argument
    [v], which may carry a syntactic annotation, and whose type expected by the
    context is [ret_ty].  The annotation, if provided, must unify with [ret_ty]. *)
val get_typ_for_arg_with_expected_type : env -> varsyntyp -> typ -> typ


(*#########################################################################*)
(* ** Extension of environments *)

(** Add an overload variable [x] into the environment, with no declared instances. *)
(* val env_add_empty_instance : ?loc:loc -> env -> symbol -> symbol_modes -> env
 *)
(** Add a local type variable as a tconstr (this is triggerred by [(type a)] parameters
  (that is, [Trm_forall]) from a function. *)
val env_add_tconstr_var : env -> tconstr -> typ -> env

val env_add_tconstr_vars : env -> tconstr list -> typs -> env

(** Add an instance of [x] into the environment.
  If the symbol [x] is a regular variable in the current environment, an error will be thrown. *)
(* val env_add_instance : ?loc:loc -> env -> symbol -> instance -> env
 *)
(** [env_add_recursive_var e v t] extends the environment [e] with a binding
    from [v] to the type scheme associated with the type of [t]. *)
val env_add_recursive_var : env -> var -> trm -> env

(** Add a dummy type declaration into the environment, ignoring its actual declaration.
  This is useful for recursively defined types. *)
val add_dummy_type : env -> Parsetree.type_declaration -> env

(** [env_add_type_declaration e td] processes the type declaration td and adds it into
  the environment.
  If the type declaration is recursive, then it will assume that it is already within
  the environment (at least as a dummy placeholder). *)
(* val env_add_type_declaration : env -> Parsetree.type_declaration -> env * tconstr_desc
 *)

(*#########################################################################*)
(* ** Typechecking of user-provided type annotations *)

(** Convert a user annotation into a type-scheme. *)
val sch_opt_of_styp : env -> styp -> sch option

(** Typechecking of a type annotation. *)
val typecheck_annot : env -> typ -> syntyp -> string -> unit

(* Internalise a syntactic type. *)
val syntyp_internalize : env -> syntyp -> syntyp

(** Converts a type annotation [ct] into a proper type.
  If [poly] is set to [true], it does nothing if the annotation is polymorphic
  (otherwise it complains). *)
val typ_of_styp : ?poly:bool -> env -> styp -> typ


(*#########################################################################*)
(* ** Miscellaneous *)

(** [mk_sch_of_forall] is called when type-checking a let-binding with "forall"s type
  parameters of the form [fun (type t)].
  These type parameters are declared as abstract types (as when declaring a gobal [type t]
  without definition) within the type-checking, but need to be replaced by rigid variables
  when building a proper type scheme.
  This last part is what this function is doing. *)
val mk_sch_of_forall :
  tconstr list -> typ -> sch

