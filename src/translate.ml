(* Pass of syntax simplification for the AST.  *)

(* ============================================================================ *)
(* Fresh Variable Generation API *)
(* ============================================================================ *)

(* TODO: remove later the useless libraries *)
open Asttypes
open Parsetree
open Var
open Ast_fix
open Ast_aux
open Tools
open Ast_print
open PPrint

module FreshVar : sig
  type t
  val create : unit -> t
  val generate : t -> string -> string
  val reset : t -> unit
end = struct
  type t = (string, int ref) Hashtbl.t

  let create () = Hashtbl.create 10 (* TODO: see of this is enough *)

  let generate state prefix =
    let counter =
      try Hashtbl.find state prefix
      with Not_found ->
        let c = ref 0 in
        Hashtbl.add state prefix c;
        c
    in
    incr counter;
    prefix ^ "_" ^ string_of_int !counter

  let reset state = Hashtbl.clear state
end

(* ============================================================================ *)
(* Helper to create a dummy syntyp for fresh variables *)
(* ============================================================================ *)

(* TODO: Implement proper syntyp creation based on your parser *)
let dummy_syntyp () : syntyp =
  failwith "TODO: create dummy syntyp with fresh flexible type"

(* Helper to construct 'raise Switch_failure' - dummy for now *)
let mk_raise_switch_failure loc : trm =
  (* TODO: Replace with actual exception representation *)
  (* Placeholder: creating a variable named "raise_Switch_failure" *)
  trm_var ~loc "raise_Switch_failure"

(* ============================================================================ *)
(* Main Translation Functions *)
(* ============================================================================ *)

(* Main translation function for terms *)
let rec translate_trm (fresh_state : FreshVar.t) (t : trm) : trm =
  let loc = t.trm_loc in
  match t.trm_desc with

  (* x ==> x *)
  | Trm_var v ->
      trm_var ~loc v

  (* c ==> c *)
  | Trm_cst c ->
      trm_cst ~loc c

  (* fun (x1 : T1) ... (xn : Tn) -> t ==> fun (x1:T1) ... (xn:Tn) -> [[t]] *)
  | Trm_funs (params, body) ->
      trm_funs ~loc params (translate_trm fresh_state body)

  (* if b then t1 else t2 ==> [[b]] ([[t1]]) ([[t2]]) *)
  | Trm_if (cond, then_branch, else_branch) ->
      let then_translated = translate_trm fresh_state then_branch in
      let else_translated = translate_trm fresh_state else_branch in
      translate_bbe fresh_state cond then_translated else_translated

  (* let _ = t1 in t2 ==> let _ = [[t1]] in [[t2]] *)
  (* let x = t1 in t2 ==> let x = [[t1]] in [[t2]] *)
  | Trm_let (let_def, body) ->
      let translated_rhs = translate_trm fresh_state let_def.let_def_body in
      let translated_body = translate_trm fresh_state body in
      let new_let_def = {
        let_def with
        let_def_body = translated_rhs;
      } in
      trm_let_def ~loc new_let_def translated_body

  (* f (t1, ..., tn) ==> ([[f]] [[t1]] ... [[tn]]) *)
  | Trm_apps (f, args) ->
      let f' = translate_trm fresh_state f in
      let args' = List.map (translate_trm fresh_state) args in
      trm_apps ~loc f' args'

  (* (t : T) ==> ([[t]] : T) *)
  | Trm_annot (t, ty) ->
      trm_annot ~loc (translate_trm fresh_state t) ty

  (* fun (type a) -> t ==> fun (type a) -> [[t]] *)
  | Trm_forall (tvar, body) ->
      trm_forall ~loc tvar (translate_trm fresh_state body)

  (* match v with | p1 -> t1 | ... | pn -> tn ==> [[switch ...]] *)
  | Trm_match (scrutinee, cases) ->
      translate_match fresh_state scrutinee cases loc

  (* (t1, ..., tn) ==> ([[t1]], ..., [[tn]]) *)
  | Trm_tuple ts ->
      trm_tuple ~loc (List.map (translate_trm fresh_state) ts)

  (* not t ==> not [[t]] *)
  | Trm_not t ->
      trm_not ~loc (translate_trm fresh_state t)

  (* t1 && t2 ==> [[t1]] && [[t2]] *)
  | Trm_and (t1, t2) ->
      trm_and ~loc (translate_trm fresh_state t1) (translate_trm fresh_state t2)

  (* t1 || t2 ==> [[t1]] || [[t2]] *)
  | Trm_or (t1, t2) ->
      trm_or ~loc (translate_trm fresh_state t1) (translate_trm fresh_state t2)

  (* switch (case b then t) :: case_list ==> [[b]] ([[t]]) ([[switch case_list]]) *)
  | Trm_switch cases ->
      translate_switch fresh_state cases loc

  (* while b then t ==> let rec __loop_N () = [[b]] ([[t]]; __loop_N ()) (()) in __loop_N () *)
  | Trm_while (cond, body) ->
      translate_while fresh_state cond body loc

  (* BBE constructions should not appear in term position during parsing *)
  | Trm_bbe_is (t, p) ->
      (* This might appear if the DSL allows it in term position *)
      (* We treat it as a boolean expression returning true/false *)
      failwith "Trm_bbe_is in term position - not yet implemented"

  (* Pattern constructions should not appear in term position *)
  | Trm_pat_var _
  | Trm_pat_wild
  | Trm_pat_when _ ->
      failwith "Pattern construct in term position - invalid input"

(* Translation for BBE (branching boolean expressions) with continuations
   [[b]] (on_success) (on_failure) *)
and translate_bbe (fresh_state : FreshVar.t)
                  (b : trm)
                  (on_success : trm)
                  (on_failure : trm) : trm =
  let loc = b.trm_loc in
  match b.trm_desc with

  (* [[y is p]] (u) (u') ==> (y |> [[p]] (u) (u')) *)
  | Trm_bbe_is ({ trm_desc = Trm_var y; _ }, p) ->
      translate_pattern fresh_state y p on_success on_failure loc

  (* [[t is p]] (u) (u') ==> let y = t in (y |> [[p]] (u) (u')) *)
  | Trm_bbe_is (t, p) ->
      let fresh_var = FreshVar.generate fresh_state "__match" in
      let pattern_check = translate_pattern fresh_state fresh_var p on_success on_failure loc in
      let t' = translate_trm fresh_state t in
      trm_let ~loc Nonrecursive (fresh_var, None) t' pattern_check

  (* [[b1 && b2]] (u) (u') ==> [[b1]] ([[b2]] (u) (u')) (u') *)
  | Trm_and (b1, b2) ->
      let inner = translate_bbe fresh_state b2 on_success on_failure in
      translate_bbe fresh_state b1 inner on_failure

  (* [[b1 || b2]] (u) (u') ==> [[b1]] (u) ([[b2]] (u) (u')) *)
  | Trm_or (b1, b2) ->
      let inner = translate_bbe fresh_state b2 on_success on_failure in
      translate_bbe fresh_state b1 on_success inner

  (* [[not b]] (u) (u') ==> [[b]] (u') (u) *)
  | Trm_not b ->
      translate_bbe fresh_state b on_failure on_success

  (* [[t]] (u) (u') ==> if [[t]] then u else u' where t is a boolean term *)
  | _ ->
      let t' = translate_trm fresh_state b in
      trm_if ~loc t' on_success on_failure

(* Translation for patterns with continuations
   (y |> pattern (on_success) (on_failure))

   Note: scrutinee_var is always a variable name, not a term *)
and translate_pattern (fresh_state : FreshVar.t)
                      (scrutinee_var : varid)
                      (p : trm_pat)
                      (on_success : trm)
                      (on_failure : trm)
                      (loc : loc) : trm =

  (* Since trm_pat is an alias for trm, we pattern match on trm_desc *)
  match p.trm_desc with

  (* (y |> _ (u) (u')) ==> u *)
  | Trm_pat_wild ->
      on_success

  (* (y |> ??x (u) (u')) ==> let x = y in u *)
  | Trm_pat_var x ->
      let y_var = trm_var ~loc scrutinee_var in
      trm_let ~loc Nonrecursive (x, None) y_var on_success

  (* (y |> (p when b) (u) (u')) ==> let k () = u' in (y |> [[p]] ([[b]] (u) (k ())) (k ())) *)
  | Trm_pat_when (inner_pat, guard) ->
      let k_name = FreshVar.generate fresh_state "__cont" in
      let k_var = trm_var ~loc k_name in
      let unit_arg = trm_unit ~loc () in
      let k_call = trm_apps ~loc k_var [unit_arg] in

      (* Create thunk: fun () -> on_failure *)
      let unit_param : varsyntyps = [("_", dummy_syntyp ())] in
      let k_thunk = trm_funs ~loc unit_param on_failure in

      (* [[b]] (u) (k ()) *)
      let guarded_success = translate_bbe fresh_state guard on_success k_call in

      (* (y |> [[p]] (guarded_success) (k ())) *)
      let pattern_match = translate_pattern fresh_state scrutinee_var inner_pat guarded_success k_call loc in

      (* let k () = on_failure in pattern_match *)
      trm_let ~loc Nonrecursive (k_name, None) k_thunk pattern_match

  (* Handle structured patterns *)
  | _ ->
      translate_pattern_structure fresh_state scrutinee_var p on_success on_failure loc

(* Helper to translate structured patterns
   This handles patterns like:
   - C
   - C (p1, ..., pn)
   - f (p1, ..., pn)
   - (p1 & p2)
   - (p1 | p2)
   - (not p)
*)
and translate_pattern_structure (fresh_state : FreshVar.t)
                                (scrutinee_var : varid)
                                (p : trm_pat)
                                (on_success : trm)
                                (on_failure : trm)
                                (loc : loc) : trm =

  (* Analyze the pattern structure based on the term representation *)
  match p.trm_desc with

  (* Pattern conjunction: (p1 & p2) *)
  (* (y |> (p1 & p2) (u) (u')) ==> (y |> [[p1]] (y |> [[p2]] (u) (u')) (u')) *)
  | Trm_and (p1, p2) ->
      let inner = translate_pattern fresh_state scrutinee_var p2 on_success on_failure loc in
      translate_pattern fresh_state scrutinee_var p1 inner on_failure loc

  (* Pattern disjunction: (p1 | p2) *)
  (* (y |> (p1 | p2) (u) (u')) ==> (y |> [[p1]] (u) (y |> [[p2]] (u) (u'))) *)
  | Trm_or (p1, p2) ->
      let inner = translate_pattern fresh_state scrutinee_var p2 on_success on_failure loc in
      translate_pattern fresh_state scrutinee_var p1 on_success inner loc

  (* Pattern negation: (not p) *)
  (* (y |> (not p) (u) (u')) ==> (y |> [[p]] (u') (u)) *)
  | Trm_not p ->
      translate_pattern fresh_state scrutinee_var p on_failure on_success loc

  (* Constructor without arguments: C *)
  (* (y |> C (u) (u')) ==> match y with | C -> u | _ -> u' *)
  | Trm_var constr_name ->
      let y_var = trm_var ~loc scrutinee_var in
      let success_case = (p, on_success) in
      let wildcard = trm_pat_wild ~loc () in
      let failure_case = (wildcard, on_failure) in
      trm_match ~loc y_var [success_case; failure_case]

  (* Constructor with arguments: C (p1, ..., pn) *)
  (* (y |> C (p1, ..., pn) (u) (u')) ==>
       match y with
       | C (x1, ..., xn) -> [[(x1 is p1) && ... && (xn is pn)]] (u) (u')
       | _ -> u' *)
  | Trm_apps ({ trm_desc = Trm_var constr_name; _ }, pattern_args) ->
      let y_var = trm_var ~loc scrutinee_var in

      (* Generate fresh variables for each argument *)
      let fresh_vars = List.map (fun _ ->
        FreshVar.generate fresh_state "__arg"
      ) pattern_args in

      (* Build conjunction: (x1 is p1) && (x2 is p2) && ... *)
      let build_conjunction vars pats =
        match List.combine vars pats with
        | [] -> on_success (* No patterns to check *)
        | (v, p) :: rest ->
            let first_check = trm_bbe_is ~loc (trm_var ~loc v) p in
            List.fold_left (fun acc (var, pat) ->
              let check = trm_bbe_is ~loc (trm_var ~loc var) pat in
              trm_and ~loc acc check
            ) first_check rest
      in

      let combined_check = build_conjunction fresh_vars pattern_args in
      let check_result = translate_bbe fresh_state combined_check on_success on_failure in

      (* Create match pattern: C (x1, ..., xn) *)
      let var_patterns = List.map (fun v -> trm_pat_var ~loc v) fresh_vars in
      let match_pattern = trm_apps ~loc (trm_var ~loc constr_name) var_patterns in

      let success_case = (match_pattern, check_result) in
      let wildcard = trm_pat_wild ~loc () in
      let failure_case = (wildcard, on_failure) in
      trm_match ~loc y_var [success_case; failure_case]

  (* Function pattern: f (p1, ..., pn) *)
  (* (y |> f (p1, ..., pn) (u) (u')) ==>
       let x = [[f]] y in (x |> Some (p1, ..., pn) (u) (u')) *)
  | Trm_apps (func, pattern_args) ->
      let y_var = trm_var ~loc scrutinee_var in
      let func' = translate_trm fresh_state func in
      let app = trm_apps ~loc func' [y_var] in

      let fresh_var = FreshVar.generate fresh_state "__view" in

      (* Create pattern: Some (p1, ..., pn) *)
      let some_constr = trm_var ~loc "Some" in
      let tuple_pattern = trm_tuple ~loc pattern_args in
      let some_pattern = trm_apps ~loc some_constr [tuple_pattern] in

      let inner_check = translate_pattern fresh_state fresh_var some_pattern on_success on_failure loc in
      trm_let ~loc Nonrecursive (fresh_var, None) app inner_check

  (* Tuple pattern: (p1, ..., pn) - represented as Trm_tuple *)
  | Trm_tuple pattern_list ->
      (* Similar to constructor with arguments *)
      let y_var = trm_var ~loc scrutinee_var in

      (* Generate fresh variables for each component *)
      let fresh_vars = List.map (fun _ ->
        FreshVar.generate fresh_state "__tuple"
      ) pattern_list in

      (* Build conjunction of checks *)
      let build_conjunction vars pats =
        match List.combine vars pats with
        | [] -> on_success
        | (v, p) :: rest ->
            let first_check = trm_bbe_is ~loc (trm_var ~loc v) p in
            List.fold_left (fun acc (var, pat) ->
              let check = trm_bbe_is ~loc (trm_var ~loc var) pat in
              trm_and ~loc acc check
            ) first_check rest
      in

      let combined_check = build_conjunction fresh_vars pattern_list in
      let check_result = translate_bbe fresh_state combined_check on_success on_failure in

      (* Create match pattern: (x1, ..., xn) *)
      let var_patterns = List.map (fun v -> trm_pat_var ~loc v) fresh_vars in
      let tuple_match_pattern = trm_tuple ~loc var_patterns in

      let success_case = (tuple_match_pattern, check_result) in
      let wildcard = trm_pat_wild ~loc () in
      let failure_case = (wildcard, on_failure) in
      trm_match ~loc y_var [success_case; failure_case]

  (* Constant pattern: 42, true, "hello", etc. *)
  | Trm_cst c ->
      let y_var = trm_var ~loc scrutinee_var in
      let const_pattern = trm_cst ~loc c in
      let success_case = (const_pattern, on_success) in
      let wildcard = trm_pat_wild ~loc () in
      let failure_case = (wildcard, on_failure) in
      trm_match ~loc y_var [success_case; failure_case]

  | _ ->
      failwith "Unsupported pattern structure"

(* Helper: translate match to switch
   match v with | p1 -> t1 | ... | pn -> tn ==> [[switch ...]] *)
and translate_match (fresh_state : FreshVar.t)
                    (scrutinee : trm)
                    (cases : (trm_pat * trm) list)
                    (loc : loc) : trm =
  (* The scrutinee must be bound to a variable first *)
  let scrutinee_var =
    match scrutinee.trm_desc with
    | Trm_var v -> v
    | _ -> FreshVar.generate fresh_state "__scrutinee"
  in

  (* Convert each (pattern, body) to (bbe, translated_body) *)
  let switch_cases = List.map (fun (pattern, body) ->
    (* Create BBE: scrutinee_var is pattern *)
    let scrutinee_trm = trm_var ~loc scrutinee_var in
    let bbe = trm_bbe_is ~loc scrutinee_trm pattern in
    let translated_body = translate_trm fresh_state body in
    (bbe, translated_body)
  ) cases in

  let switch_trm = translate_switch fresh_state switch_cases loc in

  (* If we created a fresh variable, wrap in let binding *)
  match scrutinee.trm_desc with
  | Trm_var _ -> switch_trm
  | _ ->
      let scrutinee' = translate_trm fresh_state scrutinee in
      trm_let ~loc Nonrecursive (scrutinee_var, None) scrutinee' switch_trm

(* Helper: translate switch statements
   switch (case b then t) :: case_list ==> [[b]] ([[t]]) ([[switch case_list]]) *)
and translate_switch (fresh_state : FreshVar.t)
                     (cases : (bbe * trm) list)
                     (loc : loc) : trm =
  match cases with
  | [] ->
      (* switch [] ==> raise Switch_failure *)
      mk_raise_switch_failure loc

  | (cond, body) :: rest ->
      (* switch (case b then t) :: case_list ==> [[b]] ([[t]]) ([[switch case_list]]) *)
      let rest_switch = translate_switch fresh_state rest loc in
      let body' = translate_trm fresh_state body in
      translate_bbe fresh_state cond body' rest_switch

(* Helper: translate while loops
   while b then t ==>
     let rec __loop_N () =
       [[b]] ([[t]]; __loop_N ()) (())
     in __loop_N () *)
and translate_while (fresh_state : FreshVar.t)
                    (cond : bbe)
                    (body : trm)
                    (loc : loc) : trm =
  let loop_name = FreshVar.generate fresh_state "__loop" in
  let loop_var = trm_var ~loc loop_name in
  let unit_arg = trm_unit ~loc () in

  (* __loop_N () *)
  let loop_call = trm_apps ~loc loop_var [unit_arg] in

  (* [[t]] *)
  let body' = translate_trm fresh_state body in

  (* [[t]]; __loop_N () *)
  let body_seq = trm_seq ~loc body' loop_call in

  (* [[b]] (body_seq) (()) *)
  let loop_body = translate_bbe fresh_state cond body_seq unit_arg in

  (* fun () -> loop_body *)
  let unit_param : varsyntyps = [("_", dummy_syntyp ())] in
  let loop_fun = trm_funs ~loc unit_param loop_body in

  (* let rec __loop_N () = ... in __loop_N () *)
  trm_let ~loc Recursive (loop_name, None) loop_fun loop_call

(* ============================================================================ *)
(* Public API *)
(* ============================================================================ *)

(* Main entry point for translation *)
let translate (t : trm) : trm =
  let fresh_state = FreshVar.create () in
  translate_trm fresh_state t

(* Entry point with custom fresh variable state *)
let translate_with_state (fresh_state : FreshVar.t) (t : trm) : trm =
  translate_trm fresh_state t