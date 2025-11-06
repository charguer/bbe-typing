open Ast_fix

(** Computing of the representent of a type. *)

(** [get_repr t] returns the type object that describes the type [t].
    It corresponds to finding the root in the Union-Find data structure.
    Path compression is performed on the way.

    The traversal uses marks to make sure to not be trapped into a cycle.
    Cycle detection corresponds to an internal error, since cycle creation
    is supposed to be verified during the unification process.

    The [current_mark] is used to obtain in constant time a fresh mark that
    has never been previously used. The operation [get_repr] starts by
    requesting a fresh mark, then uses that mark throughout the traversal
    from the given type towards its root. *)

(** [get_repr t] returns the type object that describes the type [t].
  This function traverses the pointer-chains of [Unified] constructors.
  This function does not update deep [Unified] constructs. *)
val get_repr : typ -> typ

(** Check that no cycle are present in the union-find data-structure
  (this would be an internal error). *)
val check_no_cycle : typ -> unit

