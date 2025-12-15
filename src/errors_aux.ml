
open Printf
open Var
open Ast_print
open Errors

let symbol_to_string = symbol_to_string_message

let string_of_error ~style (e : error) : string =
  let suggest = function
    | None -> "."
    | Some str -> sprintf ", did you mean %s?" str in
  match e with
  | Error_constr_mismatch (i1, i2) ->
    sprintf "Unable to unify type %s with type %s." (print_tconstr i1) (print_tconstr i2)
  | Error_number_of_arguments_mismatch (t1, t2) ->
    sprintf "Unable to unify types with different numbers of arguments: %s and %s."
      (typ_to_string t1) (typ_to_string t2)
  | Rigid_can_not_unify (v, ty) ->
    sprintf "Unable to unify the rigid type variable %s with an other type %s."
      (print_tvar_rigid v) (typ_to_string ty)
  | Cycle_creation (t1, t2) ->
    sprintf "A cycle was created when trying to unify %s and %s."
      (typ_to_string t1) (typ_to_string t2)
  | Polymorph_and_non_polymorph_types_can_not_unify ->
    "Unable to unify a polymorph type with an other type."
  | Polymorphic_type_annotation -> "A non-polymorphic type annotation was expected."
  | Instance_all_mismatch (x, ctxt, ty, insts) ->
    sprintf "The type %s of %s %sdoes not match with any of the instances:\n %s."
      (typ_to_string ty) (symbol_to_string x)
      (Option.fold ~none:"" ~some:(fun y -> sprintf "(to resolve %s) " (symbol_to_string y)) ctxt)
      (Tools.doc_to_string (overload_to_doc ~style insts))
  | Bad_annotation (s, ty1, ty2) ->
    sprintf "%s has type %s but the annotation has type %s."
      s (typ_to_string ty1) (typ_to_string ty2)
  | Unexpected_annotation sty -> sprintf "Issue with annotation %s." (print_styp sty)
  | Conflict_with_context (t, ty1, ty2) ->
    sprintf "%s has type %s but the context expects type %s."
      (trm_to_string ~style t) (typ_to_string ty1) (typ_to_string ty2)
  | Conflict_with_context_pattern (p, ty1, ty2) ->
    sprintf "%s has type %s but the context expects type %s."
      (pat_to_string p) (typ_to_string ty1) (typ_to_string ty2)
  | Boolean_condition ty ->
    sprintf "The condition should have type bool but it has type %s." (typ_to_string ty)
  | Branches_mismatch_if (ty1, ty2) ->
    sprintf "The then branch has type %s but the else branch has type %s."
      (typ_to_string ty1) (typ_to_string ty2)
  | Mismatch_type_is (ty1, ty2) ->
    sprintf "The expression has type %s but the pattern takes type %s."
      (typ_to_string ty1) (typ_to_string ty2)
  | Branches_mismatch_match (x, ty1, ty2) ->
    sprintf "Two branches of the pattern-matching disagree on %s: in one case it is typed as %s, in the other as %s."
      x (typ_to_string ty1) (typ_to_string ty2)
  | Sequence ty ->
    sprintf "The first term of a sequence should have type unit, not %s." (typ_to_string ty)
  | Application_mistyped (ty1, ty2) ->
    sprintf "The function has type %s, but it was expected to have type %s." (typ_to_string ty1) (typ_to_string ty2)
  | Unable_to_unify (ty1, ty2) ->
    sprintf "Unable to unify type %s with type %s." (typ_to_string ty1) (typ_to_string ty2)
  | Missing_information (n, ty) ->
    sprintf "missing information %s. Current type: %s" n (typ_to_string ty)
  | No_instance s -> sprintf "There was no registered instance for %s." (symbol_to_string s)
  | Nonoverloaded_symbol_found_as_assumption (x, unify) ->
    sprintf "When looking for instance assumption %s, a local variable was encountered, but it wasn't declared as an instance.%s"
      (symbol_to_string x) (if unify then "" else " Its type wasn't unifiable anyway.")
  (*| Mixing_overloaded_symbols (symb1, symb2) ->
    sprintf "When looking for instance assumption %s of the symbol declared in %s, it was shadowed by the overloaded symbol %s declared in %s."
      (print_var symb1.symbol_var) (print_loc symb1.symbol_loc) (print_var symb2.symbol_var) (print_loc symb2.symbol_loc) *)
  | No_annot_in_recusive_def -> "In a recursive definition, the definition must have a type annotation."
  | Flexible_in_definition (v, ty, ty_fl) ->
    let additionnal_comment =
      match tvar_raw ty_fl with
      | None -> ""
      | Some x ->
        if x = print_tvar ty_fl then ""
        else sprintf " — originally called %s in the source file —" x in
    sprintf "The type %s of the variable %s is not fully resolved (flexible variable %s present after resolution); perhaps missing a type annotation for a polymorphic definition?"
      (typ_to_string ty) (symbol_to_string v) (print_tvar ty_fl ^ additionnal_comment)
  | Different_variables_number (i1, i2) ->
    sprintf "The term has a type with %i variables but the annotation has %i variables" i1 i2
  | Bad_arity (x, i) -> sprintf "The input for %s needs an instance with arity %i." (symbol_to_string x) i
  | Unbound_variable x -> sprintf "Unbound variable %s." (symbol_to_string x)
  | Unbound_variable_in_assumption x -> sprintf "Unbound symbol %s as an instance parameter." (symbol_to_string x)
  | Not_an_arrow_type -> "The function does not have a function type."
  | Not_an_overloaded_symbol x ->
    sprintf "The instance assumption %s is not a declared overloaded symbol." (symbol_to_string x)
  | Unbound_type_variable (x, suggestion) ->
    sprintf "Unbound type variable %s%s" (print_tvar_rigid x)
      (suggest (Option.map (fun x -> "the type " ^ x) suggestion))
  | Unbound_type_constructor (tconstr, suggestion) ->
    sprintf "Unbound type %s%s" (print_tconstr tconstr)
      (suggest (Option.map (fun x -> "the type " ^ x) suggestion))
  | Unbound_constructor c -> sprintf "Unbound constructor %s." (print_constr c)
  | Invalid_type_constructor_arity (tconstr, expected_arity, provided_arity) ->
    sprintf "The type constructor %s expects %d argument(s), but is here applied to %d argument(s)."
      (print_tconstr tconstr) expected_arity provided_arity
  | Conflicting_input_modes (x, ms1, ms2) ->
    sprintf "Conflicting modes for %s: expected %s but got %s."
      (symbol_to_string x) (modes_io_to_string ms1) (modes_io_to_string ms2)
  | Invalid_length_tuple (expected, given) ->
    sprintf "This tuple has %i subterms, but its type indicates that it has %i." expected given
  | Mismatch_signature_for_constant_overload (x, ty, expected) ->
    sprintf "The type %s doesn't fit the constant class %s: a type of the form %s -> _ is expected." (typ_to_string ty) (print_var x) (typ_to_string expected)
  | Missing_constructors_in_match ty ->
    sprintf "Missing constructor in pattern-matching for type %s." (typ_to_string ty)
  | Mismatching_type_annotation_names_in_declaration (xs1, xs2) ->
    sprintf "When a let-binding has a type annotation and a fun(type) as body it must use the same list of names on both sides. Got %s and %s."
      (String.concat "," (List.map print_tconstr xs1))
      (String.concat "," (List.map print_tconstr xs2))
  | Variable_is_bound_several_times_in_pattern x ->
    sprintf "The variable %s is bound several times in the same pattern." (print_var x)
  | Variable_most_occur_on_both_sides_of_this_pattern x ->
    sprintf "The variable %s must occur on both sides of this pattern." (print_var x)
  | Overload_of_a_regular_variable x ->
    sprintf "The regular variable %s is being shadowed by an overloaded symbol." (symbol_to_string x)
  | Maximum_varid_depth_reached -> "maximum length of a dependency chain of varid has been exceeded"
  | Expected_bindings -> "expected bindingss"
  | Unsupported_term s -> sprintf "The term %s is not supported." s
  | Trying_to_unifying_bbe -> "trying to unify a bbe"
  | Wrong_pattern_constructor s ->
    sprintf "Expected to match against a %s" s
  | Mismatch_pattern_size (i1,i2) -> sprintf "Expected a pattern of size %n but got a pattern of size %n\n" i1 i2

let string_of_error_short (e : error) : string =
  match e with
  | Error_constr_mismatch (i1, i2) ->
    sprintf "different constructors %s and %s" (print_tconstr i1) (print_tconstr i2)
  | Error_number_of_arguments_mismatch (_t1, _t2) -> "types with different numbers of arguments"
  | Rigid_can_not_unify (_v, _ty) -> "unification of rigid variable"
  | Cycle_creation (_t1, _t2) -> "cycle in types"
  | Polymorph_and_non_polymorph_types_can_not_unify -> "polymorph unified with non-polymorph"
  | Polymorphic_type_annotation -> "polymorphic annotation"
  | Instance_all_mismatch (x, _ctxt, _ty, _insts) ->
    sprintf "instances all mismatch for %s" (symbol_to_string x)
  | Bad_annotation (s, ty1, ty2) -> sprintf "bad annotation for %s" s
  | Unexpected_annotation sty -> sprintf "Issue with annotation %s." (print_styp sty)
  | Conflict_with_context (t, _ty1, _ty2) -> "term conflicts with context"
  | Conflict_with_context_pattern (p, _ty1, _ty2) -> "pattern conflicts with context"
  | Boolean_condition _ty -> "non-boolean condition"
  | Branches_mismatch_if (_ty1, _ty2) -> "branch mismatch in if"
  | Branches_mismatch_match (x, _ty1, _ty2) -> sprintf "branch mismatch in match on %s" x
  | Mismatch_type_is (ty1, ty2) -> "type mismatch in is"
  | Sequence _ty -> "non-unit in sequence"
  | Application_mistyped (_ty1, _ty2) -> "mistyped application"
  | Unable_to_unify (_ty1, _ty2) -> "unable to unify"
  | Missing_information (n, _ty) -> sprintf "missing information %s" n
  | No_instance s -> sprintf "no instance for %s" (symbol_to_string s)
  | Nonoverloaded_symbol_found_as_assumption (x, _unify) ->
    sprintf "non-overloaded symbol %s as instance assumption" (symbol_to_string x)
  (*| Mixing_overloaded_symbols (symb1, symb2) ->
    sprintf "Shadowed overloaded symbol %s" (print_var symb1.symbol_var)*)
  | No_annot_in_recusive_def -> "missing type annotation in recursive definition"
  | Flexible_in_definition (v, _ty, _ty_fl) ->
    sprintf "not fully resolved symbol %s" (symbol_to_string v)
  | Different_variables_number (_i1, _i2) -> "annotation with wrong variable number"
  | Bad_arity (x, i) -> sprintf "bad arity for %s" (symbol_to_string x)
  | Unbound_variable x -> sprintf "unbound variable %s" (symbol_to_string x)
  | Unbound_variable_in_assumption x -> sprintf "unbound instance parameter %s" (symbol_to_string x)
  | Not_an_arrow_type -> "not an arrow type"
  | Not_an_overloaded_symbol x -> sprintf "%s not an overloaded symbol" (symbol_to_string x)
  | Unbound_type_variable (x, _suggest) -> sprintf "Unbound type variable %s" (print_tvar_rigid x)
  | Unbound_type_constructor (tconstr, _suggest) ->
    sprintf "unbound type %s" (print_tconstr tconstr)
  | Unbound_constructor c -> sprintf "unbound constructor %s" (print_constr c)
  | Invalid_type_constructor_arity (tconstr, expected_arity, provided_arity) ->
    sprintf "invalid type arity for %s" (print_tconstr tconstr)
  | Conflicting_input_modes (x, _ms1, _ms2) -> sprintf "conflicting modes for %s" (symbol_to_string x)
  | Invalid_length_tuple (_expected, _given) -> "invalid tuple length"
  | Mismatch_signature_for_constant_overload (x, _ty, _expected) ->
    sprintf "invalid type for %s" (print_var x)
  | Missing_constructors_in_match _ty -> "Missing constructor in pattern-matching"
  | Mismatching_type_annotation_names_in_declaration (_xs1, _xs2) ->
    "different type variable names in both sides of a let-binding"
  | Variable_is_bound_several_times_in_pattern x ->
    sprintf "%s is bound several times in this pattern" (print_var x)
  | Variable_most_occur_on_both_sides_of_this_pattern x ->
    sprintf "%s must occur on both sides of this pattern" (print_var x)
  | Overload_of_a_regular_variable x ->
    sprintf "Overload of a non-overloaded variable %s" (symbol_to_string x)
  | Maximum_varid_depth_reached -> "maximum varid depth exceeded"
  | Expected_bindings -> "expected bindings"
  | Unsupported_term _s -> "unsupported term"
  | Trying_to_unifying_bbe -> "trying to unify a bbe"
  | Wrong_pattern_constructor s ->
    sprintf "Expected to match against a %s" s
  | Mismatch_pattern_size (i1,i2) -> sprintf "Expected a pattern of size %n but got a pattern of size %n\n" i1 i2

