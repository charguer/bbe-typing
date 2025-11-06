
(** Convert an error into an error message, meant for the user. *)
val string_of_error : style:Ast_print.style -> Errors.error -> string


(** Convert an error into a short error message, meant to mark a line as error. *)
val string_of_error_short : Errors.error -> string

