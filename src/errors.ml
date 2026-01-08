
open Var
open Ast_fix

(** Errors for internal use *)
type error =
  | Error_constr_mismatch of tconstr * tconstr
  | Error_number_of_arguments_mismatch of typ * typ
  | Rigid_can_not_unify of tvar_rigid * typ
  | Cycle_creation of typ * typ
  | Polymorph_and_non_polymorph_types_can_not_unify (* Current unused *)
  | Polymorphic_type_annotation
  (* | Instance_all_mismatch of symbol * symbol option * typ * candidates_and_modes *)
  | Bad_annotation of string * typ * typ
  | Unexpected_annotation of styp
  | Conflict_with_context of trm * typ * typ
(*   | Conflict_with_context_pattern of pat * typ * typ *)
  | Boolean_condition of typ
  | Branches_mismatch_if of typ * typ
  | Branches_mismatch_match of string * typ * typ
  | Mismatch_type_is of typ * typ
  | Mismatch_type_switch of typ * typ
  | Sequence of typ
  | Application_mistyped of typ * typ
  | Unable_to_unify of typ * typ
  | Missing_information of string * typ
  (* | No_instance of symbol *)
  (* | Nonoverloaded_symbol_found_as_assumption of symbol * bool *)
  (*| Mixing_overloaded_symbols of overloaded_symbol * overloaded_symbol*) (* FIXME *)
  | No_annot_in_recusive_def
  (* | Flexible_in_definition of symbol * typ * tvar *)
  | Different_variables_number of int * int
  (* | Bad_arity of symbol * int *)
  | Unbound_variable of string
  | Unbound_variable_in_assumption of string
  | Not_an_arrow_type (* Current unused *)
  (* | Not_an_overloaded_symbol of symbol *)
  | Unbound_type_variable of tvar_rigid * string option
  | Unbound_type_constructor of tconstr * string option
  | Unbound_constructor of constr
  | Invalid_type_constructor_arity of tconstr * int * int
  (* | Conflicting_input_modes of symbol * (mode list * mode) * (mode list * mode) *)
  | Invalid_length_tuple of int * int
  | Mismatch_signature_for_constant_overload of var * typ * typ
  | Missing_constructors_in_match of typ
  | Mismatching_type_annotation_names_in_declaration of tconstr list * tconstr list
  | Variable_is_bound_several_times_in_pattern of var
  | Variable_most_occur_on_both_sides_of_this_pattern of var
  (* | Overload_of_a_regular_variable of symbol *)
  | Maximum_varid_depth_reached
  | Expected_bindings
  | Unsupported_term of string
  | Trying_to_unifying_bbe
  | Wrong_pattern_constructor of string
  | Mismatch_pattern_size of int * int

exception Error of (error * loc)

