open Var
open Ast_fix

(* * Syntax *)

(* ** Locations *)

val loc_none : loc

val get_line : loc -> int

val refine_loc : loc -> loc -> loc (* Return the last non-none location *)


(* ** Marks *)

val no_mark : mark

(* This function returns a fresh new mark, guaranteed to be larger than any previous mark. *)
val incr_mark : unit -> mark


(* ** Syntactic types *)

val mk_syntyp : ?typ:typ -> styp -> syntyp
val mk_syntyp_none : ?typ:typ -> ?loc:loc -> unit -> syntyp
val mk_syntyp_unit : ?loc:loc -> unit -> syntyp
val mk_syntyp_tconstr : ?loc:loc -> tconstr -> syntyp

(* Try to convert a type into a syntactic type.
  It returns Ptyp_any in case any variable in encountered. *)
val typ_to_styp : typ -> styp

(* Try to merge two syntactic types. *)
val merge_styp : styp -> styp -> styp option

(* The general [_] syntactic type. *)
val styp_any : styp

(** * Typing Environment *)

(* val mk_overloaded_symbol : var -> symbol
 *)
(* val overload_var : candidates_and_modes -> env_item
 *)
val env_add_tconstr : env -> tconstr -> tconstr_desc -> env
val env_add_tvar : env -> tvar_rigid -> typ -> env
val env_add_var : env -> var -> sch -> env
(* val env_add_symbol : env -> symbol -> env_item -> env
 *)
(** * Terms *)

val varsyntyp_loc : varsyntyp -> loc

(** * Smart Constructors *)

(** ** For Term Descriptors *)

val trm_desc_cst : cst -> trm_desc
val trm_desc_bool : bool -> trm_desc
val trm_desc_int : int -> trm_desc
val trm_desc_float : float -> trm_desc
val trm_desc_string : string -> trm_desc
val trm_desc_unit : unit -> trm_desc
val trm_desc_var : var -> trm_desc
(* val trm_desc_var_symbol : ?typ:typ0 -> ?resolution:varid_resolution -> symbol -> trm_desc
 *)
val trm_desc_funs : label option -> varsyntyps -> trm -> trm_desc
val trm_desc_constr : ?loc:loc -> ?typ:typ -> constr -> trms -> trm_desc
val trm_desc_if : label option -> trm -> trm -> trm -> trm_desc
val trm_desc_let : rec_flag -> varsynschopt -> trm -> trm -> trm_desc
val trm_desc_let_def : let_def -> trm -> trm_desc
val trm_desc_seq : trm -> trm -> trm_desc
val trm_desc_apps : trm -> trms -> trm_desc
val trm_desc_match : label option -> trm -> (pat * trm) list -> trm_desc
val trm_desc_tuple : trm list -> trm_desc
val trm_desc_not : trm -> trm_desc
val trm_desc_and : trm -> trm -> trm_desc
val trm_desc_or : trm -> trm -> trm_desc
val trm_desc_while : label option -> bbe -> trm -> trm_desc
val trm_desc_switch : label option -> (bbe * trm) list -> trm_desc
val trm_desc_block : label -> trm -> trm_desc
val trm_desc_exit : label -> trm -> trm_desc
val trm_desc_return : label -> trm -> trm_desc
val trm_desc_break : label -> trm_desc
val trm_desc_continue : label -> trm_desc
val trm_desc_next : label -> trm_desc
val trm_desc_try_with : trm -> pat -> trm -> trm_desc


(* val trm_desc_raise : except -> trm_desc
val trm_desc_try : trm -> except -> trm -> trm_desc *)

val trm_desc_bbe_is : trm -> pat -> trm_desc

val trm_desc_pat_var : var -> trm_desc
val trm_desc_pat_var_varid : varid -> trm_desc
val trm_desc_pat_wild : unit -> trm_desc
val trm_desc_pat_when : pat -> bbe -> trm_desc
val trm_desc_assert_false : unit -> trm_desc
(** ** For Terms *)

(* Warning: there are two ways to implement constants in the programs. One is the “low-level”
  representation of constant, and one is the encoded, as a class applied to a constant.
  These functions here build the first case. *)
val trm_cst : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) cst -> trm
val trm_bool : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) bool -> trm
val trm_int : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) int -> trm
val trm_float : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) float -> trm
val trm_string : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) string -> trm
val trm_unit : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) unit -> trm

val trm_var : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) var -> trm
(* val trm_var_symbol : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) ?resolution:varid_resolution -> symbol -> trm
 *)
val trm_var_varid : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) varid -> trm
val trm_tuple : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) trm list -> trm

val trm_funs : ?loc:loc -> ?typ:typ -> label option -> (* ?annot:annot -> *) varsyntyps -> trm -> trm (* Doesn't work if the list is empty: use [trm_funs_if_non_empty] in such cases. *)
val trm_constr : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) constr -> trms -> trm
val trm_if : ?loc:loc -> ?typ:typ -> label option -> (* ?annot:annot -> *) trm -> trm -> trm -> trm
val trm_let : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) rec_flag -> varsynschopt -> trm -> trm -> trm
val trm_let_def : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) let_def -> trm -> trm
val trm_seq : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) trm -> trm -> trm
val trm_apps : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) trm -> trms -> trm
val trm_annot : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) trm -> syntyp -> trm
val trm_forall : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) tvar_rigid -> trm -> trm
val trm_foralls : ?loc:loc -> ?typ:typ -> tvar_rigid list -> trm -> trm (* Works even if the list is empty. *)
val trm_match : ?loc:loc -> ?typ:typ -> label option -> (* ?annot:annot -> *) trm -> (pat * trm) list -> trm

val trm_not : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) trm -> trm
val trm_and : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) trm -> trm -> trm
val trm_or : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) trm -> trm -> trm
val trm_while : ?loc:loc -> ?typ:typ -> label option -> (* ?annot:annot -> *) bbe -> trm -> trm
val trm_switch : ?loc:loc -> ?typ:typ -> label option -> (* ?annot:annot -> *) (bbe * trm) list -> trm
val trm_block : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) label -> trm -> trm

val trm_exit : ?loc:loc -> ?typ:typ -> label -> trm -> trm
val trm_return : ?loc:loc -> ?typ:typ -> label -> trm -> trm
val trm_break : ?loc:loc -> ?typ:typ -> label -> trm
val trm_continue : ?loc:loc -> ?typ:typ -> label -> trm
val trm_next : ?loc:loc -> ?typ:typ -> label -> trm

(* val trm_raise : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) except -> trm
val trm_try : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) trm -> except -> trm -> trm *)


(* val trm_or : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) trm -> trm -> trm
val trm_or : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) trm -> trm -> trm
 *)

(* val trm_record_get : ?loc:loc -> ?typ:typ -> trm -> field -> trm
val trm_record_set : ?loc:loc -> ?typ:typ -> trm -> field -> trm -> trm
val trm_record_make : ?loc:loc -> ?typ:typ -> (field * trm) list -> trm
val trm_record_with : ?loc:loc -> ?typ:typ -> trm -> field -> trm -> trm
 *)
val trm_bbe_is : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) trm -> pat -> trm

val trm_pat_var : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) var -> trm
val trm_pat_var_varid : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) varid -> trm
val trm_pat_wild : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) unit -> trm
val trm_pat_when : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) pat -> bbe -> trm
val trm_assert_false : ?loc:loc -> ?typ:typ -> unit -> trm

val trm_magic : ?loc:loc -> ?typ:typ -> trm -> trm
val trm_try_with : ?loc:loc -> ?typ:typ -> trm -> pat -> trm -> trm

val trm_try_next : ?loc:loc -> ?typ:typ -> trm -> label -> trm -> trm
val trm_try_exit : ?loc:loc -> ?typ:typ -> trm -> label -> (* trm -> trm -> *) trm
val trm_raise_next : ?loc:loc -> ?typ:typ -> label -> trm
val trm_raise_exit : ?loc:loc -> ?typ:typ -> label -> trm -> trm

(* Like [trm_funs], but simply returns the body if no arguments are provided. *)
val trm_funs_if_non_empty : ?loc:loc -> ?typ:typ -> label option -> (* ?annot:annot -> *) varsyntyps -> trm -> trm
(* Like [trm_tuple], but accepts any list (return the one term if there is only one, and unit
   if there are none). *)
val trm_tuple_flex : ?loc:loc -> ?typ:typ -> (* ?annot:annot -> *) trm list -> trm

(** ** For Patterns *)

(* val pat_any : ?loc:loc -> ?typ:typ -> unit -> pat
val pat_var : ?loc:loc -> ?typ:typ -> var -> pat
val pat_alias : ?loc:loc -> ?typ:typ -> pat -> var -> pat
val pat_constant : ?loc:loc -> ?typ:typ -> cst -> pat
val pat_tuple : ?loc:loc -> ?typ:typ -> pats -> pat
val pat_construct : ?loc:loc -> ?typ:typ -> constr -> pats -> pat
val pat_constraint : ?loc:loc -> ?typ:typ -> pat -> syntyp -> pat
val pat_or : ?loc:loc -> ?typ:typ -> pat -> pat -> pat
val pat_ors : ?loc:loc -> ?typ:typ -> pats -> pat
 *)
(** ** For Toplevels *)

val topdef_desc_val : ?sch:sch -> rec_flag -> bind -> trm -> topdef_desc
val topdef_desc_val_let_def : ?sch:sch -> let_def -> topdef_desc
val topdef_desc_typ_def : ?typs:tconstr_desc list -> rec_flag -> Parsetree.type_declaration list -> topdef_desc
val topdef_desc_external : var -> synsch -> string list -> topdef_desc
val mktopdef : ?loc:loc -> ?error:string -> topdef_desc -> topdef
val topdef_val : ?loc:loc -> ?error:string -> rec_flag -> bind -> trm -> topdef
val topdef_val_let_def : ?loc:loc -> ?error:string -> let_def -> topdef
val topdef_typ_def : ?loc:loc -> ?error:string -> ?typs:tconstr_desc list -> rec_flag -> Parsetree.type_declaration list -> topdef
val topdef_external : ?loc:loc -> ?error:string -> var -> synsch -> string list -> topdef

(** ** For Types *)

(* Create new flexible types. *)
val typ_nameless : unit -> typ
val typ_namelesses : int -> typ list

val tvar_rigid : string -> tvar_rigid
val mktyp : typ_desc -> typ
val typ_tvar : tvar -> typ

val typ_rigid : tvar_rigid -> typ
val typ_constr : tconstr -> typ list -> typ
val typ_empty : unit -> typ
val typ_unit : unit -> typ
val typ_list : typ -> typ
val typ_int : unit -> typ
val typ_string : unit -> typ
val typ_float : unit -> typ
val typ_bool : unit -> typ
val typ_arrow : typ list -> typ -> typ (* Doesn't work if the list is empty: use [typ_arrow_flexible] in such cases. *)
val typ_tuple : typ list -> typ (* Works even if the list is empty. *)
val typ_tuple_flex : typ list -> typ (* Works only if the list is not empty. *)
val typ_option : typ -> typ
val the_typ_bool : typ
val the_typ_int : typ
val the_typ_float : typ
val the_typ_string : typ
val the_typ_unit : typ
val the_typ_bbe : typ
val the_typ_top : typ
(* FIXME: Do we still need these?
   val typ_overload : typ -> candidates_and_modes -> internal_type
  val typ_resolving_parameters : typ -> instance -> ?modes:symbol_modes -> symbol_type list -> internal_type
  val typ_resolved : typ -> instance -> internal_type *)
val mk_sch : tvar_rigid list -> typ -> sch
val sch_of_nonpolymorphic_typ : typ -> sch
val synsch_of_nonpolymorphic_typ : syntyp -> synsch

(* Like [typ_arrow], but if its first argument is empty, returns the second. *)
val typ_arrow_flexible : typ list -> typ -> typ

(* For replacing just one rigid variable by a type within a type.
  See Blocks.replace_rigids_with for a more general version. *)
val replace_rigid_with : tvar_rigid -> typ -> typ -> typ

(** Build the type scheme associated to an instance. *)
(* val instance_sch : instance_sig -> sch
 *)
(** Build an instance signature without assumption from a type scheme. *)
(* val instance_sig_from_sch : sch -> instance_sig
 *)
(** ** For Varid *)

val create_varid :  (* ?loc:loc -> ?env:env -> ?typ:typ -> ?resolution:varid_resolution -> ?depth:int -> ?context:symbol -> *) var -> varid

(** * Environment Initialisation *)

val env_empty : env
val env_builtin : env
(* val env_builtin_with_tuples : int list -> env (* Also add all the tuple constructors for these arities. *)
val env_builtin_tuples : unit -> env (* Calls [env_builtin_with_tuples] with the currently seen built-ints. *) *)
val env_item_var_nonpolymorphic : typ -> sch

(** * Tuples *)

(** Add a tuple of this arity to the [env_builtin_tuples] environment. *)
val add_tuple_arity : int -> unit

(** Return the list of all the arities seen for tuples up to now. *)
val all_seen_tuple_arity : unit -> int list

(** * Accessors/Setters *)

val save_trm_env : trm -> env -> trm
val get_env : trm -> env
val tvar_rigid_of_styp : styp -> tvar_rigid

(** * Iterators on typ *)

(* Iterate on the immediate subterms of the provided type. *)
val typ_iter : (typ -> unit) -> typ -> unit
val typ_map : (typ -> typ) -> typ -> typ
val typ_exists : (typ -> bool) -> typ -> bool

(** Compare two types, ignoring the [typ_mark] *)
val typ_compare : typ -> typ -> int

(** * Inversion functions for types *)

(* FIXME: Do we still need these?
   val internal_type_of : trm -> internal_type (* Fails if not yet typed. *)
  val internal_type_of_pat : pat -> internal_type (* Fails if not yet typed. *)
  val symbol_typ : symbol -> typ *)
val typ_of : trm -> typ
val typ_arrow_inv_opt : typ -> (typ list * typ) option
val typ_arrow_inv : typ -> typ list * typ
val typ_tuple_inv_opt : typ -> (typ list) option
val typ_option_inv_opt : typ -> typ option
val typ_option_inv : typ -> typ
val typ_matrix_inv_opt : typ -> typ option
val trm_foralls_inv : trm -> tvar_rigid list * trm

(** [contains_flexible ty] returns [true] if [ty] contains a flexible
    variable anywhere in depth in its structure. In other words,
    it returns [false] iff [ty] is a fully resolved type. *)
val contains_flexible : typ -> bool

(** Exception returned by [contains_flexible_exn]. *)
exception Contains_flexible of tvar

(** Does nothing if a type doesn't contain any flexible variable.
  Otherwise raises [Contains_flexible] of such a variable. *)
val contains_flexible_exn : typ -> unit

(** * Iterators on trm *)

(* Iterate on the immediate subterms of the provided term. *)
val trm_iter : (trm -> unit) -> trm -> unit
val trm_map : (trm -> trm) -> trm -> trm

val trm_clone : trm -> trm

(* Apply the function to all types in the term or pattern. *)
val trm_map_typ : (typ -> typ) -> trm -> trm
(* val pat_map_typ : (typ -> typ) -> pat -> pat
 *)
(** * Iterators on patterns *)

(* val pat_map : (pat -> pat) -> pat -> pat
val pat_iter : (pat -> unit) -> pat -> unit
 *)
(* Variables declared within a pattern. *)
(* val pat_vars : pat -> var list
 *)
(** * Iterators on program *)

(* Iterate on each top-level declaration of the provided program. *)
val program_iter : (topdef -> unit) -> program -> unit
val program_map : (topdef -> topdef) -> program -> program
val program_maps : (topdef -> topdef list) -> program -> program

val program_clone : program -> program

