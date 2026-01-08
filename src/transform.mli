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

(** Main entry point for translation.*)

(** Transform a single top-level definition *)
val transform_topdef : FreshVar.t -> topdef -> topdef

(** Transform a complete program *)
val transform_program : program -> program
