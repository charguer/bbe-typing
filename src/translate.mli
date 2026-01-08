open Asttypes
open Parsetree
open Var
open Ast_fix
open Ast_aux
open Tools
open Ast_print
open PPrint

(** Fresh variable generation state *)
module FreshVar : sig
  type t
  val create : unit -> t
  val generate : t -> string -> string
  val reset : t -> unit
end

(** Main entry point for translation.
    Translates a term from the extended DSL to the core subset. *)
val translate : trm -> trm

(** Entry point with custom fresh variable state.
    Useful when you need to maintain variable naming across multiple translations. *)
val translate_with_state : FreshVar.t -> trm -> trm