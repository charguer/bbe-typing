open Asttypes
open Parsetree
open Var
open Ast_fix
open Ast_aux
open Tools
open Ast_print
open PPrint

(** [comp_topdef t] *)
val comp_topdef : topdef -> topdef

(** [comp_program p] *)
val comp_program : program -> program