
(** A rollback mechanism for the representation of types. *)
type history

(** * Running a function with a checkpoint and a rollback. *)

(** [with_rollback f] evaluates the function [f()], and undo any
    modification to the types that [f] might have performed.
    Important: the function [f] should not be used to compute a
    [typ] as result, else this [typ] would likely be invalid. *)
val with_rollback : (unit -> 'a) -> 'a

(** [with_rollback f] evaluates the function [f()], which is expected
    to return a boolean value indicating success. Upon success, the
    effects performed by [f] are kept. In case of failure, the effects
    performed by [f] are undone. *)
val with_rollback_on_error : (unit -> bool) -> bool

(** [make_modif_desc t td] changes the [desc] field of a type, saving
    the changes in the history stack. *)
val make_modif_desc : Ast_fix.typ -> Ast_fix.typ_desc -> unit

(** For debug purposes.
  Returns the list of recent changes (i.e., up to the fist checkpoint)
  within the current history *)
val current_modif : unit -> (Ast_fix.typ * Ast_fix.typ_desc) list

