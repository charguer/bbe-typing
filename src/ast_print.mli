open Var
open Ast_fix


(* Levels of printing details, to which subterm should be explicitly typed. *)
type style_types =
  | TypesSubterms
  | TypesVarsAndBinders
  | TypesVars
  | TypesVarsOverloaded
  | TypesVarsOverloadedUnresolved
  | TypesNone

(* Levels of details about what to print for overloaded symbols, as well as their instances's arguments. *)
type style_resolution =
  | ResolutionInstanceOrSymbol (* If the instance is resolved, print it, otherwise leave it as an unresolved symbol with a comment listing all candidates. *)
  | ResolutionInstanceOrError (* Enforce that the instance is resolved (its assumptions may be unresolved). *)
  | ResolutionInstanceOrObjMagic (* For debugging purposes: prints [Obj.magic ()] if unresolved. *)
  | ResolutionSymbol (* Only prints the symbol associated to the varid. *)

type style_debug =
  | DebugNone
  | DebugResolving of varid (* For this particular varid, we do ResolutionInstanceOrSymbol and TypesVarsOverloaded. *)
  | DebugResolved of varid (* For this particular varid, we do TypesVarsOverloaded. *)

type style = {
  style_types : style_types ;
  style_resolution_full : style_resolution (* The style for the main function of a fully resolved instance. *) ;
  style_resolution_base : style_resolution (* The style for the main function of a partially resolved instance. *) ;
  style_resolution_args : style_resolution (* The style for the assumptions of a partially resolved instance. *) ;
  style_debug : style_debug ;
  style_print_symbols : bool (* Whether internal symbols (to encode record and constant constructs) should be printed as stored internally. *)
}

val style_debug : style


(** Print a program into a file, explicitely marked the types or not. *)
val to_file : style:style -> string -> program -> unit

(** Print a program, explicitely marked the types or not. *)
val to_string : style:style -> program -> string

val env_to_string : style:style -> env -> string

type doc = PPrint.document

val print_indent : out_channel -> unit -> unit

val print_loc : loc -> string

val typ_to_doc : typ -> doc
val put_parens : typ -> doc
val overload_to_doc : style:style -> candidates_and_modes -> doc
val sch_to_doc : sch -> doc
val sch_to_string : sch -> string
val varid_to_string : style:style -> varid -> string

val print_tvar_rigid : tvar_rigid -> string

val typ_to_string : typ -> string
val syntyp_to_string : syntyp -> string
val symbol_to_string : symbol -> string
val print_typ : out_channel -> typ -> unit
val print_typ_option : out_channel -> typ option -> unit
val cst_to_doc : cst -> doc
val var_to_doc : var -> doc
val put_parens_trm : trm -> doc -> doc
val pat_to_doc : pat -> doc
val trm_to_doc : style:style -> trm -> doc
val topdef_to_doc : style:style -> topdef -> doc
val topdef_to_string : style:style -> topdef -> string
val ast_to_doc : style:style -> program -> doc
val to_outchannel : style:style -> out_channel -> program -> unit
val print_styp : styp -> string

val trm_to_stdout : style:style -> trm -> unit
val print_trm : style:style -> out_channel -> trm -> unit
val print_item : style:style -> out_channel -> env_item -> unit
val mode_to_string : mode -> string
val modes_to_string : mode list -> string
val modes_io_to_string : mode list * mode -> string
val trm_to_string : style:style -> trm -> string
val pat_to_string : pat -> string
val insts_to_string : style:style -> candidates_and_modes -> string
val instance_to_string : style:style -> instance -> string

(* Like [symbol_to_string], but better suited for displaying error messages. *)
val symbol_to_string_message : symbol -> string

