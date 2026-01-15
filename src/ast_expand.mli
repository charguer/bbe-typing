open Ppxlib
open Ast_builder.Default
open Asttypes
open Parsetree
open Var
open Ast_fix
open Ast_aux
open Tools
open Ast_print
open PPrint

val expand_program : program -> structure