open Var
open Ast_fix
open Ast_aux
open Tools
open Ast_print
open PPrint
open History
open Errors
open Errors_aux

(*#########################################################################*)
(* ** Testing for cycles, and testing for partially/fully resolved types *)

(** [is_flexible t] returns [true] if [t] is a flexible variable
    (as explained about [typ_desc] in ast.ml).
    Beware that this function returns [false] if [t] is a [Unified]
    pointing at a flexible type variable. *)

let is_flexible (t : typ) : bool =
  match t.typ_desc with
  | Flexible _ -> true
  | _ -> false

(* [check_flexible_not_occuring_typ tflexible ty] raises an error if
   [tflexible] occurs anywhere in depth in the structure of the type [ty].
   Cyclic types are indeed not accepted by our typechecker
   (unlike [ocamlc -rectypes]). This function is only used when
   [!Flags.check_cycles_at_every_unification] is on. Otherwise, cycles
   are detected only at the end of an instance unifiability test,
   and at the very end of the whole typechecking. *)

let check_flexible_not_occuring_typ (tflexible : typ) (ty : typ) : unit =
  assert (is_flexible tflexible);
  let rec visit ty =
    if tflexible == ty
       then raise (Error (Cycle_creation (tflexible, ty), loc_none))
       else typ_iter visit ty
    in
    visit ty

let varid_is_resolved (varid:varid) : bool =
  match varid.varid_resolution with
  | VarRegular (* | VarResolved _ *) -> true
  | _ -> false

exception Contains_unresolved of varid

let (* rec *) all_varid_in_varid_are_resolved_exn (x : varid) : unit =
  match x.varid_resolution with
  | VarRegular -> ()
  (* | VarResolved (_, xs) -> List.iter all_varid_in_varid_are_resolved_exn xs
   *)
   | _ -> raise (Contains_unresolved x)

let rec all_varid_in_trm_are_resolved_exn (t : trm) : unit =
  match t.trm_desc with
  | Trm_var x -> all_varid_in_varid_are_resolved_exn x
  | _ -> trm_iter all_varid_in_trm_are_resolved_exn t

let all_varid_in_trm_are_resolved (t : trm) : bool =
  try
    all_varid_in_trm_are_resolved_exn t ;
    true
  with Contains_unresolved _ -> false

let check_fully_typed ~in_depth ~test_acylic (t : trm) (s : symbol) : unit =
  let debug () =
    let style_debug = { style_debug with style_types = TypesSubterms } in
    Debug.log "++Here is the currently typed term:\n%s\n"
      (*Debug.print_low_level_trm t*)
      (trm_to_string ~style:style_debug t) in
  let check_type_for_flexible_and_cycle (ty : typ) : unit =
    contains_flexible_exn ty ;
    Repr.check_no_cycle ty in
  let check_contains_flexible t =
    let ty = typ_of t in
    try
      check_type_for_flexible_and_cycle ty ;
      (* As a special case, we also look for patterns within [t]. *)
      begin match t.trm_desc with
      | Trm_match (_, pts) ->
        List.iter (fun (pat, _) ->
          let rec aux p =
            check_type_for_flexible_and_cycle p.pat_typ ;
            pat_iter aux p in
          aux pat) pts
      | _ -> ()
      end
    with Contains_flexible ty_flex_name ->
      debug () ;
      raise (Error (Flexible_in_definition (s, ty, ty_flex_name), t.trm_loc)) in
  if in_depth then begin
    let rec aux t =
      check_contains_flexible t ;
      trm_iter aux t in
    aux t
  end else begin
     check_contains_flexible t;
  end;
  try all_varid_in_trm_are_resolved_exn t
  with Contains_unresolved x ->
    debug () ;
    (* begin match x.varid_resolution with
    | VarUnresolved candidates ->
      let candidates = candidates.candidates_and_modes_candidates in
      Debug.log "++Here are the candidates for symbol %s:\n - %s"
        (symbol_to_string_message x.varid_symbol)
        (String.concat "\n - " (List.map (Ast_print.instance_to_string ~style:style_debug) candidates))
    | _ -> ()
    end ; *)
    (* raise (Error (Missing_information
      (Printf.sprintf "for resolving symbol %s within %s"
        (symbol_to_string_message x.varid_symbol) (symbol_to_string_message s),
      x.varid_typ), x.varid_loc)) *)
    raise (Error (Unsupported_term "Unresolved inside \"check_fully_typed\". Temporary error message. modify [Blocks.check_full_typed] to remove this", x.varid_loc))
  (* TODO: handle a specific message for cycle detection at the very end? *)


(*#########################################################################*)
(* ** Instantiation of polymorphic types *)

module RigidMap =
  Map.Make (struct
    type t = tvar_rigid
    let compare = compare
  end)

let replace_rigids_with (map_rigid : typ RigidMap.t) (ty : typ) : typ =
  if RigidMap.is_empty map_rigid then ty
  else begin
    let rec aux (ty : typ) : typ =
      let ty = Repr.get_repr ty in
      match ty.typ_desc with
      | Typ_constr (x, tys) ->
          begin match RigidMap.find_opt x map_rigid with
          | None -> typ_map aux ty
          | Some tyf ->
            assert (tys = []) ;
            tyf
          end
      | _ -> typ_map aux ty in
    aux ty
  end

let make_map_rigid_flexible (vs:tvar_rigid list) : typ RigidMap.t =
  List.fold_left (fun m x -> RigidMap.add x (typ_nameless ()) m) RigidMap.empty vs

(** [apply_to_fresh_flexibles_with_map sch] is like [apply_to_fresh_flexibles sch],
   but it also returns the correspondance map. *)
let apply_to_fresh_flexibles_with_map (sch : sch) =
  let ty = sch.sch_body in
  let map_rigid_flexible = make_map_rigid_flexible sch.sch_tvars in
  (replace_rigids_with map_rigid_flexible ty, map_rigid_flexible)

let apply_to_fresh_flexibles (sch : sch) : typ =
  fst (apply_to_fresh_flexibles_with_map sch)

let get_record_types ?(loc = loc_none) (e : env) (ty : typ) msg : (field * typ) list =
  let ty = Repr.get_repr ty in
  match ty.typ_desc with
  | Typ_constr (c, ty_args) ->
    begin match Env.read_option e.env_tconstr c with
    | None -> assert false
    | Some tcd ->
      match tcd.tconstr_def with
      | Tconstr_record ftys ->
        assert (List.length tcd.tconstr_tvars = List.length ty_args) ;
        let map =
          List.fold_left2 (fun m v ty ->
            RigidMap.add v ty m) RigidMap.empty tcd.tconstr_tvars ty_args in
        List.map (fun (f, ty) -> (f, replace_rigids_with map ty)) ftys
      | _ -> assert false
    end
  | _ -> raise (Error (Missing_information (msg, ty), loc))

module TConstrMap =
  Map.Make (struct
    type t = tconstr
    let compare = compare
  end)

(** ...internal function,
    [replace_tconstr_with m ty] is used once the type [ty] of a term parameterised
    by [(type t)] annotations has been inferred: we now have to replace each of these
    types [t] by a fresh rigid variable.
    We assume that none of these types [t] have any argument. *)
let replace_tconstr_with (map_tconstr : typ TConstrMap.t) (ty : typ) : typ =
  let rec aux (ty : typ) : typ =
    let ty = Repr.get_repr ty in
    match ty.typ_desc with
    | Typ_constr (tc, args) ->
        begin match TConstrMap.find_opt tc map_tconstr with
        | None -> { ty with typ_desc = Typ_constr (tc, List.map aux args) }
        | Some ty' ->
          assert (args = []) ;
          ty'
        end
    | _ -> typ_map aux ty in
  aux ty



(*#########################################################################*)
(* ** Unification of two types *)

(** [result] denotes the result of a unification operation between two types *)
type result =
  | Success
  | Failure of (error * loc)

(* let is_unresolved x =
  match x.varid_resolution with
  | VarUnresolved _ -> true
  | _ -> false *)

(** [unify_flexible t_flexible t2] performs the unification of the flexible variable
  [t_flexible] with the type [t2], with a best effort to preserve a meaningful name.
  Both [t_flexible] and [t2] are roots (results of [Repr.get_repr]).
  Note that as [t_flexible] is part of the Union-Find, it can't be duplicated without
  breaking some invariants (typically the cycle search): this is why this function
  has to take [t_flexible] as an argument and not just [v_flexible1]. *)
let unify_flexible (t_flexible : typ) (t_other : typ) : unit =
  assert (is_flexible t_flexible);
  if !Flags.check_cycles_at_every_unification then
    check_flexible_not_occuring_typ t_flexible t_other; (* Check acyclicity of the flexible graph. *)
  make_modif_desc t_flexible (Unified t_other);
  ;; (*Temporary fix, I had a syntax error because it thought this function's declaration was not finished for some reason.*)

(** Given a type, this function will unfold any occurences of type aliases until getting
  a type (whose top-level construct) is not an alias.
  Guarantees that it returns a root. *)
let rec unfold_alias env t =
  let t = Repr.get_repr t in
  match t.typ_desc with
  | Typ_constr (id, ts) ->
    begin match Env.read_option env.env_tconstr id with
      | None -> assert false
      | Some tc ->
        begin match tc.tconstr_def with
        | Tconstr_special_nary | Tconstr_abstract | Tconstr_def_sum _ | Tconstr_record _ -> t
        | Tconstr_def_alias t1 ->
            assert (List.length tc.tconstr_tvars = List.length ts) ;
            let t2 =
              (* We have to replace all the variables of [t1] with their actual values [ts] *)
              let map_rigid =
                List.fold_left2 (fun m x t -> RigidMap.add x t m) RigidMap.empty
                  tc.tconstr_tvars ts in
              replace_rigids_with map_rigid t1 in
            let r = unfold_alias env t2 in
            (* Debug.log "Debug: unrolling %s as %s." (typ_to_string t) (typ_to_string r) ; *)
            Repr.get_repr r
      end
    end
  | _ -> t


(* ... auxiliary function for unification *)

(*Error: the file does not compile, said to have a syntax error here. Debug next week.*)

(* Question: what is this function for? This is a factorized code to test two types.
If both roots of their respective unions are not the same, then force a unification.*)
let rec unify_exn_aux ?loc env (t1 : typ) (t2 : typ) : unit =
  Counters.(compute_count_and_time counter_unify time_unify (fun () ->
    let tr1 = Repr.get_repr t1 in
    let tr2 = Repr.get_repr t2 in
    if tr1 != tr2 then unify_desc ?loc env tr1 tr2))

(* [unify_desc ty1 ty2] assumes that [ty1] and [ty2] are roots (results of [Repr.get_repr]). *)
and unify_desc ?(loc = loc_none) env (t1 : typ) (t2 : typ) : unit =
  let t1 = unfold_alias env t1 in
  let t2 = unfold_alias env t2 in (* replaces the find operation for Union-find algorithm *)
  match t1.typ_desc, t2.typ_desc with
    | Unified _, _
    | _, Unified _ -> assert false (* t1 and t2 must be roots, so they can't be [Unified]. *)
    | Flexible _, _ -> unify_flexible t1 t2
    | _, Flexible _ -> unify_flexible t2 t1
    | Typ_constr (id1, ts1), Typ_constr (id2, ts2) ->
        if id1 <> id2 then
          (* At this stage, id1 and id2 can't be aliases: the function unfold_alias has already
            unfolded them. *)
          raise (Error (Error_constr_mismatch (id1, id2), loc)) ;
        if List.length ts1 <> List.length ts2 then
          raise (Error (Error_number_of_arguments_mismatch (t1, t2), loc)) ;
        List.iter2 (unify_exn_aux ~loc env) ts1 ts2

(** [unify_exn env t1 t2] unifies [t1] and [t2], and raises an exception if the process fails. *)

let unify_exn ?loc env (ty1 : typ) (ty2 : typ) : unit =
  unify_exn_aux ?loc env ty1 ty2;
  if !Flags.check_cycles_at_every_unification then begin
    Repr.check_no_cycle ty1;
    Repr.check_no_cycle ty2;
    (* TODO: check that this code actually catches all cycles that might have been produced *)
  end

(* ... auxiliary function for unification *)

let unify_res env (t1 : typ) (t2 : typ) : result =
  try unify_exn env t1 t2; Success
  with Error e -> Failure e

let try_unify env (t1 : typ) (t2 : typ) : bool =
  with_rollback_on_error (fun () ->
    match unify_res env t1 t2 with
    | Success -> true
    | Failure _ -> false)

let try_unifys env (ts1 : typs) (ts2 : typs) : bool =
  assert (List.length ts1 = List.length ts2);
  with_rollback_on_error (fun () ->
    let rec unif ts1 ts2 =
      match ts1, ts2 with
      | [], [] -> true
      | t1 :: q1, t2 :: q2 ->
              unify_res env t1 t2 = Success
           && unif q1 q2
      | _ -> false in
    unif ts1 ts2)

let unify_or_error ?(loc = loc_none) env (t1 : typ) (t2 : typ) (m : error) : unit =
  Debug.log "Forced unification of %s with %s." (typ_to_string t1) (typ_to_string t2) ;
  if !Flags.verbose then Printf.printf "Forcing unification of %s with %s\n" (typ_to_string t1) (typ_to_string t2);
  if not (try_unify env t1 t2)
  then raise (Error (m, loc))

let unifys_or_error ?(loc = loc_none) env (ts1 : typs) (ts2 : typs) (m : error) : unit =
  List.iter2 (fun t1 t2 ->
    Debug.log "Forced unification of %s with %s." (typ_to_string t1) (typ_to_string t2)) ts1 ts2 ;
  if not (try_unifys env ts1 ts2)
  then raise (Error (m, loc))

let is_instance_unifiable env (ty1 : typ) (ty2 : typ) : bool =
  with_rollback (fun () ->
    match unify_res env ty1 ty2 with
    | Success ->
        if not !Flags.disable_check_cycle_on_resolution_attempts
          then Repr.check_no_cycle ty1;
        true
    | Failure _ -> false)


(*#########################################################################*)
(* ** Instance resolution *)

(* let unify_with_instance ~loc (env : env) (ty1 : typ) ~depth ~context (inst : instance) : varid list =
  let inst_sig = inst.instance_sig in
  let ty_map = make_map_rigid_flexible inst_sig.instance_tvars in
  let ty2 = replace_rigids_with ty_map inst_sig.instance_typ in
  begin
    try unify_exn env ty1 ty2
    with Error _ -> assert false
  end;
  let instantiate_assumption (asmpt : assumption_desc) : varid =
    let { assumption_symbol = symbol ; assumption_typ = sty } = asmpt in
    let ty = replace_rigids_with ty_map sty.syntyp_typ in
    (* let instances =
      match Env.read_option env.env_var symbol with
      | None -> raise (Error (Unbound_variable_in_assumption symbol, loc))
      | Some (Env_item_var _) -> raise (Error (Not_an_overloaded_symbol symbol, loc))
      | Some (Env_item_overload register_instances) -> register_instances in *)
    let varid =
      create_varid ~loc ~env symbol ~typ:ty ~depth ~resolution:(VarUnresolved instances) ~context in
    varid in
  List.map instantiate_assumption inst_sig.instance_assumptions
 *)

(*#########################################################################*)
(* ** Typechecking of function arguments *)

let get_typ_for_arg (v : varsyntyp) : typ =
  let (_x, aty) = v in
  aty.syntyp_typ

let get_typ_for_arg_with_expected_type env (v : varsyntyp) (ret_ty : typ) : typ =
  let ty = get_typ_for_arg v in
  let (x, aty) = v in
  let loc = aty.syntyp_syntax.Parsetree.ptyp_loc in
  unify_or_error ~loc env ret_ty ty (Conflict_with_context (trm_var x, ty, ret_ty));
  ty


(*#########################################################################*)
(* ** Typechecking of user-provided type annotations *)

let rec arrow_of_styp (ct : styp) : styp list * styp =
  match ct.ptyp_desc with
  | Ptyp_arrow (Nolabel, ct1, ct2) ->
    let ct_args, ct_res = arrow_of_styp ct2 in
    ct1 :: ct_args, ct_res
  | _ -> [] , ct


(* Redefinition of env_add_tvar to include debugging information. *)
let env_add_tvar (e : env) (x : tvar_rigid) (ty : typ) : env =
  Debug.env_add_tvar_rigid x ty ;
  env_add_tvar e x ty

let env_add_tvars (e : env) (vs : tvar_rigid list) (tys : typs) : env =
  assert (List.length vs = List.length tys) ;
  List.fold_left2 env_add_tvar e vs tys

(* It is frequent to mix [t] with ['t].  This function tries to make relevant suggestion
  of correction for a type. *)
let suggest_env (e : env) x =
  let candidates = Var.suggest x in
  List.find_opt (fun x -> Env.mem e.env_tconstr (tconstr x)) candidates


let rec typs_of_styps (e : env) (cts : styp list) : typ list =
  match cts with
  | [] -> []
  | ct :: cts ->
      let ty = typ_of_styp e ct in
      let ts = typs_of_styps e cts in
      ty :: ts

and sch_opt_of_styp (e : env) (ct : styp) : sch option =
  let auxs = typs_of_styps e in
  let aux = typ_of_styp e in
  let sch_of_typ ty = Some (mk_sch [] ty) in
  let loc = ct.ptyp_loc in
  match ct.ptyp_desc with
  | Ptyp_any -> sch_of_typ (typ_nameless ())
  | Ptyp_arrow (Nolabel, ct1, ct2) ->
      let ct_args, ct_res = arrow_of_styp ct in
      let args = auxs ct_args in
      let res = aux ct_res in
      sch_of_typ (typ_arrow args res)
  | Ptyp_tuple cts ->
      let ts = auxs cts in
      sch_of_typ (typ_tuple ts)
  | Ptyp_constr ({ txt = Lident c; _ }, cts) ->
      let c = tconstr c in
      begin
        let tconstr_desc = Env.read_option e.env_tconstr c in
        match tconstr_desc with
        | None ->
          raise (Error (Unbound_type_constructor (c, suggest_env e (print_tconstr c)), loc))
        | Some tcd ->
          match tcd.tconstr_typ with
          | Some ty ->
            assert (tcd.tconstr_tvars = []) ;
            (* If the type is associated with a fixed type variable, we use it. *)
            if cts <> [] then (
              raise (Error (Invalid_type_constructor_arity (c, 0, List.length cts), loc))
            ) ;
            sch_of_typ ty
          | None ->
              (* Otherwise we introduce a new type variable. *)
              let tys = auxs cts in
              sch_of_typ (typ_constr c tys)
      end
  | Ptyp_poly (locs, ct1) ->
      let vs = List.map (fun (l : string Location.loc) -> "'" ^ l.txt) locs in
      let vs = List.map tvar_rigid vs in
      let tys = List.map typ_rigid vs in
      let e' = env_add_tvars e vs tys in
      Option.map (fun sch -> { sch with sch_tvars = vs @ sch.sch_tvars }) (sch_opt_of_styp e' ct1)
  | Ptyp_var s ->
      let s = tvar_rigid ("'" ^ s) in
      begin match Env.read_option e.env_tconstr s with
      | None -> raise (Error (Unbound_type_variable (s, suggest_env e (print_tvar_rigid s)), loc))
      | Some tcd ->
        match tcd.tconstr_typ with
        | Some ty ->
          assert (tcd.tconstr_tvars = []) ;
          sch_of_typ ty
        | None -> assert false
      end
  | _ -> failwith "To be completed: sch_opt_of_styp."

and typ_of_styp ?(poly = false) (e : env) (ct : styp) : typ =
  let sch_opt = sch_opt_of_styp e ct in
  match sch_opt with
  | None -> typ_nameless ()
  | Some sch ->
      if sch.sch_tvars <> [] && not poly
        then raise (Error (Polymorphic_type_annotation, ct.ptyp_loc)) ;
      sch.sch_body

let syntyp_internalize (e:env) (sty:syntyp) : syntyp =
  { sty with syntyp_typ = typ_of_styp e sty.syntyp_syntax }

let get_annot_sch ?(loc=loc_none) (e : env) (aty : syntyp) : sch option =
  match sch_opt_of_styp e aty.syntyp_syntax with
  | None -> None
  | Some sch -> Some sch

(* FIXME: deprecated? *)
let typecheck_annot env (ty : typ) (aty : syntyp) (msg_head : string) : unit =
  let ty2 = aty.syntyp_typ in
  unify_or_error env ty2 ty (Bad_annotation (msg_head, ty2, ty))


(*#########################################################################*)
(* ** Auxiliary functions  TEMPORARY *)

(** [check_arity env x sch insts] -- LATER: will change to support multiple arities *)

let check_arity env (x : symbol) (sch : sch) (insts : candidates_and_modes) : unit =
  match insts.candidates_and_modes_modes with
  | None -> ()
  | Some (modes, _return_mode) ->
      let arity_exp = List.length modes in
      let flexibles = List.init arity_exp (fun _ -> typ_nameless ()) in
      let env =
        List.fold_left (fun e x -> env_add_tvar e x (mktyp (Typ_constr (x, []))))
          env sch.sch_tvars in
      unify_or_error env sch.sch_body
        (typ_arrow_flexible flexibles (typ_nameless ())) (Bad_arity (x, arity_exp))


(*#########################################################################*)
(* ** Extension of environments *)

(* let env_find_overload ?(loc = loc_none) (e : env) (x : symbol) : candidates_and_modes =
  match Env.read_option e.env_var x with
  | None -> { candidates_and_modes_candidates = []; candidates_and_modes_modes = None }
  | Some (Env_item_overload candidates_and_modes) -> candidates_and_modes
  | Some (Env_item_var _) -> raise (Error (Overload_of_a_regular_variable x, loc))
 *)
(* We add checks into [env_add_var]. *)
let env_add_var (e : env) (x : var) (s : sch) : env =
(*   let alt_check_fully_resolved (sch : sch) =
    (* It could be tempting to add [assert (not (contains_flexible sch.sch_body))] here,
      but this would prevent local unannotated let-bindings. *)
    () in
  begin match it with
    | Env_item_var sch -> alt_check_fully_resolved sch
    | Env_item_overload { candidates_and_modes_candidates = [] ; _ } -> ()
    | Env_item_overload { candidates_and_modes_candidates = i :: _ ; _ } ->
      alt_check_fully_resolved (instance_sch i.instance_sig)
  end;
 *)
  Debug.env_add_item ~style:style_debug x s false;
  env_add_var e x s

let env_add_tconstr_var (e : env) (x : tconstr) (ty : typ) : env =
  Debug.env_add_tconstr x ty ;
  let desc = {
    tconstr_tvars = [] ;
    tconstr_def = Tconstr_abstract ;
    tconstr_typ = Some ty
  } in
  { e with env_tconstr = Env.add e.env_tconstr x desc }

let env_add_tconstr_vars (e : env) (vs : tconstr list) (tys : typs) : env =
  assert (List.length vs = List.length tys) ;
  List.fold_left2 env_add_tconstr_var e vs tys

(* Generate a fresh new name of rigid variable in the current context. *)
let fresh_tvar_rigid ?(also = TConstrMap.empty) (e : env) : tvar_rigid =
  tvar_rigid
    ("'" ^ new_name_from_seed "" (fun n ->
      not (TConstrMap.mem (tconstr n) also)
      && not (Env.mem e.env_tconstr (tconstr n))))

(* let env_add_instance ?(loc=loc_none) (e : env) (x : symbol) (inst : instance) : env =
  let sch = instance_sch inst.instance_sig in
  let insts = env_find_overload ~loc e x in
  check_arity e x sch insts;
  let it =
    Env_item_overload { insts with candidates_and_modes_candidates =
      inst :: insts.candidates_and_modes_candidates } in
  env_add_symbol e x it
 *)

(* let env_add_empty_instance ?(loc = loc_none) (e : env) (x : symbol) (modes : symbol_modes) : env =
  let it = Env_item_overload { candidates_and_modes_candidates = [] ; candidates_and_modes_modes = modes } in
  env_add_symbol e x it
 *)
(* Extract each type parameter (e.g. [type ('a, 'b) t]) of a type declaration.
  We ignore variance and injectivity here. *)
let get_vars_params td =
  let params = List.map fst td.Parsetree.ptype_params in
  let var_names = List.map tvar_rigid_of_styp params in
  let vars = List.map typ_rigid var_names in
  (var_names, vars)

let add_dummy_type (e : env) (td : Parsetree.type_declaration) : env =
  let id = tconstr td.ptype_name.txt in
  let (var_names, _vars) = get_vars_params td in
  let dummy = {tconstr_tvars = var_names; tconstr_typ = None; tconstr_def = Tconstr_abstract} in
  env_add_tconstr e id dummy

(* Process a constructor declaration within an inductive type definition. *)
let type_constructor e overall_type (c : Parsetree.constructor_declaration) : constr * typ =
  let name = constr c.Parsetree.pcd_name.txt in
  let ty =
    match c.Parsetree.pcd_args with
    | Pcstr_tuple [] ->
      (* No explicit type was provided: it is the type of the overall type. *)
      (* LATER: make use of pcd_res for GADTs. *)
      overall_type
    | Pcstr_tuple [ty] -> typ_arrow [typ_of_styp e ty] overall_type
    | Pcstr_tuple tys ->
      typ_arrow [typ_tuple (List.map (typ_of_styp e) tys)] overall_type
    | Pcstr_record _labels -> failwith "Not implemented: records within constructors." in
  (name, ty)

(* Adding an implicitely overloaded variable. *)
(* let add_implicit_overloaded_instance ?(loc = loc_none) ?modes e var_names x ty =
  (* The expected modes for this variable: they are implicitely all [in]. *)
  let modes =
    match modes with
    | Some modes -> modes
    | None ->
      match typ_arrow_inv_opt (Repr.get_repr ty) with
      | None -> ([], Mode_in)
      | Some (ts, _tr) -> (List.map (fun _ -> Mode_in) ts, Mode_in) in
  (* We first look for the current state of the constructor name within the environnment. *)
  let (instances, modes') =
    match Env.read_option e.env_var x with
    | None -> ([], modes)
    | Some (Env_item_var _sch) ->
      (* In this case, we just shadow the previous variable. *)
      ([], modes)
    | Some (Env_item_overload is) ->
      let input = Option.fold ~none:modes ~some:(fun modes -> modes) is.candidates_and_modes_modes in
      (is.candidates_and_modes_candidates, input) in
  if modes <> modes' then raise (Error (Conflicting_input_modes (x, modes, modes'), loc)) ;
  let inst_sig = {
    instance_tvars = var_names ;
    instance_assumptions = [] ;
    instance_typ = ty
  } in
  let inst = {
    instance_value = trm_var_symbol ~loc ~typ:ty ~resolution:VarRegular x ;
    instance_sig = inst_sig ;
    instance_loc = loc ;
    instance_symbol = x
  } in
  env_add_instance ~loc e x inst
 *)
(** [env_add_type_declaration e td]: processes the OCaml type declarations and adds them to the environment  *)
(* let env_add_type_declaration (e : env) (td : Parsetree.type_declaration) : env * tconstr_desc =
  let loc = td.Parsetree.ptype_loc in
  let id = tconstr td.ptype_name.txt in
  let (var_names, vars) = get_vars_params td in
  let ty_overall = typ_constr id vars in
  let e_with_locals = env_add_tvars e var_names vars in
  let tconstr_def =
    match td.ptype_kind, td.ptype_manifest with
    | Parsetree.Ptype_abstract, None -> Tconstr_abstract
    | Parsetree.Ptype_abstract, Some t -> Tconstr_def_alias (typ_of_styp e_with_locals t)
    | Parsetree.Ptype_variant constrs, None ->
      Tconstr_def_sum (List.map (type_constructor e_with_locals ty_overall) constrs)
    | Parsetree.Ptype_record labels, None ->
      let fs =
        List.map (fun ld ->
          let ty = typ_of_styp e_with_locals ld.Parsetree.pld_type in
          (field ld.Parsetree.pld_name.txt, ty)) labels in
      let fs = List.sort (fun (f1, _ty1) (f2, _ty2) -> compare f1 f2) fs in
      Tconstr_record fs
    | _, _ ->
      (* I didn't understand what these correspond to. *)
      failwith "Not implemented: type declaration" in
  let tconstr_typ =
    match vars with
    | [] -> Some ty_overall
    | _ -> None in
  let tdesc = { tconstr_tvars = var_names; tconstr_typ; tconstr_def } in
  (* Adding the type into the environment. *)
  let e = env_add_tconstr e id tdesc in
  (* Adding the constructors into the environment, if any. *)
  let e =
    match tconstr_def with
    | Tconstr_def_sum cs ->
      (* We add each constructor as an implicit overloaded instance. *)
      List.fold_left (fun e (c, ty) ->
        add_implicit_overloaded_instance ~loc e var_names (SymbolName (constr_to_var c)) ty) e cs
    | Tconstr_record fs ->
      let e =
        let ty = typ_arrow (List.map snd fs) ty_overall in
        add_implicit_overloaded_instance ~loc e var_names (SymbolMakeRecord (List.map fst fs)) ty in
      (* We add each field as its projection function, each time implicitly overloaded.
        We also add special [f_with] projections for the [{record with f = v}] symtax. *)
      List.fold_left (fun e (proj, ty) ->
        let e =
          let modes = ([Mode_in], Mode_out) in
          add_implicit_overloaded_instance ~loc ~modes e var_names (SymbolGetField proj)
            (typ_arrow [ty_overall] ty) in
        let e =
          let modes = ([Mode_in; Mode_out], Mode_out) in
          add_implicit_overloaded_instance ~loc ~modes e var_names (SymbolSetField proj)
            (typ_arrow [ty_overall; ty] the_typ_unit) in
        let e =
          add_implicit_overloaded_instance ~loc e var_names (SymbolRecordWith proj)
            (typ_arrow [ty_overall; ty] ty_overall) in
        e) e fs
    | Tconstr_abstract
    | Tconstr_def_alias _ -> e
    | Tconstr_special_nary -> assert false (* These can't be defined by the user for now. *) in
  (e, tdesc)
 *)


(*#########################################################################*)
(* ** Auxiliary functions  TEMPORARY *)

(** [get_annot_sch_rec] is a rebinding of the function [get_annot_sch] to improve error messages *)
(* TODO: relax the constraint? *)
let get_annot_sch_rec ?(loc=loc_none) (e : env) (aty : syntyp) : sch =
  match get_annot_sch ~loc e aty with
  | Some sch -> sch
  | None -> raise (Error (No_annot_in_recusive_def, loc))

let mk_sch_of_forall vs ty1 =
  let map_tconstr =
    let e =
      (* We create an empty environment containing all the type constructor present within ty1. *)
      let e = ref env_empty in
      typ_iter (fun ty ->
        match ty.typ_desc with
        | Typ_constr (c, _) ->
          (* We define a dummy value: it doesn't matter, as [fresh_tvar_rigid] only checks whether
            the type constructor is defined or not, and it ignores to which value it is associated. *)
          let dummy = {
            tconstr_tvars = [] ;
            tconstr_typ = Some ty ;
            tconstr_def = Tconstr_abstract } in
          e := env_add_tconstr !e c dummy
        | _ -> ()) ty1 ;
      !e in
    List.fold_left (fun m v ->
      TConstrMap.add v (fresh_tvar_rigid ~also:m e) m) TConstrMap.empty vs in
  if vs <> [] then
    Debug.log "Introducing new rigid type variables to represent local types: %s."
      (String.concat ", " (List.map (fun v ->
        let ty = TConstrMap.find v map_tconstr in
        Printf.sprintf "%s => %s" (print_tconstr v) (print_tvar_rigid ty)) vs)) ;
  mk_sch (List.map (fun v ->
    match TConstrMap.find_opt v map_tconstr with
    | Some t -> t
    | None -> assert false) vs) (replace_tconstr_with (TConstrMap.map typ_rigid map_tconstr) ty1)


(** [sch_of_let_body e t] computes, in environment [e], the type scheme
    associated with the type of the term [t] from the user annotations. *)
(* TODO: cleanup this code *)
(* TODO: Move? It feels in the wrong section. *)
let sch_of_let_body (e : env) (t : trm) : sch =
  let (vs, t) = trm_foralls_inv t in
  let tys = List.map (fun t -> typ_constr t []) vs in
  (* Adding all the type variables [vs] into the local environnment. *)
  let e = env_add_tconstr_vars e vs tys in
  let sch1 =
    match t.trm_desc with
    | Trm_annot (t, aty) -> (* case of polymorphic let_value *)
        get_annot_sch_rec ~loc:t.trm_loc e aty
    | Trm_funs (vs2, {trm_desc = Trm_annot (t, aty); _}) -> (* case of polymorphic function *)
        let ty = aty.syntyp_typ in
        let tys = List.map get_typ_for_arg vs2 in
        let ty_arrow = typ_arrow tys ty in
        mk_sch [] ty_arrow
    | _ -> raise (Error (No_annot_in_recusive_def, t.trm_loc)) in
  let sch2 = mk_sch_of_forall vs sch1.sch_body in
  { sch2 with sch_tvars = sch2.sch_tvars @ sch1.sch_tvars }

(* TODO: Move? It feels in the wrong section. *)
let env_add_recursive_var (e : env) (v : var) (t : trm) : env =
  let sch = sch_of_let_body e t in
  env_add_var e v sch


