open Ast_aux
open Ast_fix
open Blocks
open Errors

module List = struct
  include List

  let rec update_nth i f l =
    match i, l with
    | 0, x :: l' -> f x :: l'
    | _, [] -> invalid_arg "update_nth"
    | i, x :: l' -> x :: update_nth (i - 1) f l'

  let find_mapi f =
    let rec aux i = function
    | [] -> None
    | x :: l ->
      match f i x with
      | Some r -> Some r
      | None -> aux (i + 1) l in
    aux 0

end

exception Typecheck_error of string
exception IncompleteResolution

let raise_typecheck_error loc msg =
  let msg = Printf.sprintf "%s\n  %s" (Ast_print.print_loc loc) msg in
  raise (Typecheck_error msg)


let trm_to_string t =
  let open Ast_print in
  trm_to_string ~style:{
      style_types = !Flags.style_types ;
      style_resolution_full = !Flags.style_resolution_full ;
      style_resolution_base = !Flags.style_resolution_base ;
      style_resolution_args = !Flags.style_resolution_args ;
      style_debug = !Flags.style_debug ;
      style_print_symbols = !Flags.print_raw_symbols
    } t


(*******************************************)
(** * ML Typechecking *)

let synsch_internalize ~loc (e:env) (synsch:synsch) : synsch =
  let (tvars, sty) = synsch.synsch_syntax in
  let e' = List.fold_left (fun e v -> env_add_tvar e v (typ_rigid v)) e tvars in
  match sch_opt_of_styp e' sty with
  | Some sch ->
    let sch = { sch with sch_tvars = tvars @ sch.sch_tvars } in
    { synsch with synsch_sch = sch }
  | None -> raise (Error (Unexpected_annotation (snd synsch.synsch_syntax), loc))

let typeof (t : trm) : typ =
  t.trm_typ

let typ_constant = function
  | Cst_bool _ -> the_typ_bool
  | Cst_int _ -> the_typ_int
  | Cst_float _ -> the_typ_float
  | Cst_string _ -> the_typ_string
  | Cst_unit _ -> the_typ_unit

(* Checking that two environments as computed by typecheck_pat below are the same. *)
let check_same_domains_in_pattern trigger_call (e : env) ~loc (e1 : (Var.var, typ) Env.t) (e2 : (Var.var, typ) Env.t) : unit =
  let (e1, e2) =
    if Env.size e1 > Env.size e2 then
      (* This will return an error, but we would like to first find a variable in the domain
        of one and not the other. *)
      (e1, e2)
    else (e2, e1) in
  Env.fold e1 (fun () x ty1 ->
    match Env.read_option e2 x with
    | None -> raise (Error (Variable_most_occur_on_both_sides_of_this_pattern x, loc))
    | Some ty2 ->
      unify_or_error trigger_call ~loc e ty1 ty2
        (Conflict_with_context (trm_var ~loc ~typ:ty1 x, ty1, ty2))
  ) ()

let typecheck_variable (trigger_call : trigger_call) loc (e : env) (sym : symbol) : varid =
  let env_item =
    match Env.read_option e.env_var sym with
    | None -> raise (Error (Unbound_variable sym, loc))
    | Some it -> it in
  (* We compute the type of the variable, based on the item associated
    with [sym] in the environment.
    - If the item is a conventional type scheme, then the type of [sym]
      is obtained as the instantiation of this type scheme on fresh variables.
    - If the item describes it as an overloaded symbol, the type of [sym]
      is of the form [Env_item_overload instances], where [instances] is the
      list of possible instances. *)
  let (typ, resolution) =
    match env_item with
    | Env_item_var sch ->
      let typ = apply_to_fresh_flexibles sch in
      (typ, VarRegular)
    | Env_item_overload instances ->
      let typ = typ_nameless () in
      (typ, VarUnresolved instances) in
  let varid = create_varid ~loc ~env:e sym ~typ ~resolution in
  add_triggers_into_typ (VaridSet.singleton varid) typ ;
  Option.iter (fun f -> f Trigger_Create varid) trigger_call ;
  varid


(* For each pattern, we returned the typed pattern and the environment it introduces. *)
let rec typecheck_pat (trigger_call : trigger_call) ?(expected_typ:typ option) (e : env) (p : pat) : pat * (Var.var, typ) Env.t =
  let loc = p.pat_loc in
  let aux ?(env : env = e) ?(expected_typ:typ option) (p : pat) : pat * (Var.var, typ) Env.t =
    typecheck_pat trigger_call ?expected_typ env p in
  let return (typ : typ) (p_d : pat_desc) ex : pat * (Var.var, typ) Env.t =
    (* Unify typ with expected type if provided *)
    Option.iter (fun typ' ->
      unify_or_error trigger_call ~loc e typ typ'
        (Conflict_with_context_pattern (p, typ, typ'))) expected_typ;
    ({ pat_desc = p_d;
      pat_loc = loc;
      pat_typ = typ }, ex) in
  let check_disjoint e1 e2 =
    let (e1, e2) = if Env.size e1 > Env.size e2 then (e1, e2) else (e2, e1) in
    Env.fold e2 (fun () x ty2 ->
      if Env.mem e1 x then
        raise (Error (Variable_is_bound_several_times_in_pattern x, loc))) () in
  let rec check_all_disjoint = function
    | [] | [_] -> ()
    | e1 :: e2 :: es ->
      check_disjoint e1 e2 ;
      check_all_disjoint (e2 :: es) in

  match p.pat_desc with
  | Pat_any -> return (typ_nameless ()) Pat_any (Env.empty ())

  | Pat_var x ->
    let ty = typ_nameless () in
    let e = Env.add (Env.empty ()) x ty in
    return ty (Pat_var x) e

  | Pat_alias (p, x) ->
    let (p, e) = aux p in
    if Env.mem e x then
      raise (Error (Variable_is_bound_several_times_in_pattern x, loc)) ;
    let ty = p.pat_typ in
    let e = Env.add e x ty in
    return ty (Pat_alias (p, x)) e

  | Pat_constant c ->
    (* In pattern, we don't consider that an implicit class is applied to them: we directly
      manipulate pure constants. *)
    return (typ_constant c) (Pat_constant c) (Env.empty ())

  | Pat_tuple ps ->
    let pes = List.map aux ps in
    check_all_disjoint (List.map snd pes) ;
    let e =
      List.fold_left (fun e1 (_p, e2) ->
        Env.fold e2 Env.add e1) (Env.empty ()) pes in
    let ps = List.map fst pes in
    return (typ_tuple (List.map (fun p -> p.pat_typ) ps)) (Pat_tuple ps) e

  | Pat_construct (c, ps) ->
    let pes = List.map aux ps in
    check_all_disjoint (List.map snd pes) ;
    let e' =
      List.fold_left (fun e1 (_p, e2) ->
        Env.fold e2 Env.add e1) (Env.empty ()) pes in
    let ps = List.map fst pes in
    let x = typecheck_variable trigger_call loc e (SymbolName (Var.constr_to_var c)) in
    let tyr = typ_nameless () in
    let ty = typ_arrow_flexible (List.map (fun p -> p.pat_typ) ps) tyr in
    unify_or_error trigger_call ~loc e ty x.varid_typ
      (Conflict_with_context (trm_var_varid ~loc ~typ:x.varid_typ x, ty, x.varid_typ)) ;
    return tyr (Pat_construct (c, ps)) e'

  | Pat_constraint (p, sty) ->
    let sty = syntyp_internalize e sty in
    let (p, e) = aux ~expected_typ:sty.syntyp_typ p in
    return p.pat_typ (Pat_constraint (p, sty)) e

  | Pat_or (p1, p2) ->
    let (p1, e1) = aux p1 in
    let (p2, e2) = aux p2 in
    check_same_domains_in_pattern trigger_call e ~loc e1 e2 ;
    unify_or_error trigger_call ~loc e p1.pat_typ p2.pat_typ
      (Conflict_with_context_pattern (p2, p1.pat_typ, p2.pat_typ)) ;
    return p1.pat_typ (Pat_or (p1, p2)) e1

(* FIXME: trm_desc should be u not t nor e *)

(** [typecheck_ml_let_sch] typechecks its provided term [t] and returns its type scheme.
    Examples:
    - [let f = (fun (x:int) -> int)]
    - [let f = (fun (type a) (x:a) -> x)]
    - [let f = (fun (type a) x -> (x:a))]
    - [let f (type a) (x:a) = x]  (parsed exactly like the previous one)
    - [let f : (type a. a -> a) = fun (type a) (x:a) -> x] (with the same name "a" twice)
    - [let f : (type a. a -> a) = g] where [g] has type [type b. b -> b] in the environment,
                                     but has type [a->a] as an occurrence of a variable [g].
    - [let f : (type a. a -> a) = fun x -> x]

    Not allowed for now, even though OCaml might allow it:
    - [let f : (type a. a -> a) = fun (type b) (x:b) -> x]

    The explanation is that [Trm_forall] can only appear on the RHS of a let-binding,
    and if it does, it must the same names for the type variables as the polymorphic
    type annotation (if any) on the bound variable.

    The [syntyp] arguments correspond to the type annotation on [f], e.g. [(type a. a -> a)].
    The [x] corresponds to the bound name [x].
    The [t] corresponds to the body, e.g. [(fun (type a) (x:a) -> x)].

    The synschopt must be internalized before the call. *)
let rec typecheck_ml_let_sch (trigger_call : trigger_call) ~loc ?(fully_resolved=false) rf (e : env) ((x, synschopt) : varsynschopt) (t : trm) : trm * sch =
  (* First, we extract the "forall" quantifiers at the head of [t]. *)
  let (tvars_rhs, t1) = trm_foralls_inv t in
  (* Add the universally quantified types into the environment. *)
  let e1 = env_add_tconstr_vars e tvars_rhs (List.map (fun tvar -> typ_rigid tvar) tvars_rhs) in
  (* Compute the type scheme for [x] if missing, or check its coherence with the RHS if provided
     and is a [Trm_forall]. In both cases, get the tvars that scope over [t1].
     Details;
     - if synschopt is [None], then compute [sch] from the arguments of [t] or import it from an
       annotation.
       Note that one can't just use a flexible variable here, as the rigid variables in [tvars_rhs]
       are replaced at each recursive call to [t] before being unified with its usage.
       For instance if a recursive function of type ['a. 'a -> 'a] would be associated the type
       scheme ['a. ?ty] for a flexible variable [?ty], then its usage would be typed as [?ty] instead
       of [?ty[?b/'a]].
     - else synschopt is [Some synsch], then
        + if the RHS is a [Trm_forall] then it must quantify the exact same tvars as [syntyp]
        + otherwise, the tvars from the syntyp are scoping over [t1]. *)
  let (sch, tvars_for_typing_t1, t1, e2) =
    match synschopt with
    | None ->
        let (body, t1) =
          if rf = Asttypes.Recursive then (
            (* Due to the internalisation, we need to update the term. *)
            let rec aux t =
              let loc = t.trm_loc in
              match t.trm_desc with
              | Trm_funs (xs, t0) ->
                let xs =
                  List.map (fun (x, sty) ->
                    let sty = syntyp_internalize e1 sty in
                    (x, sty)) xs in
                begin match aux t0 with
                | None -> None
                | Some (tyr, t0) ->
                  let ty = typ_arrow (List.map (fun (_x, sty) -> sty.syntyp_typ) xs) tyr in
                  Some (ty, trm_funs ~loc ~typ:ty xs t0)
                end
              | Trm_annot (t0, sty) ->
                let sty = syntyp_internalize e1 sty in
                let typ = sty.syntyp_typ in
                Some (typ, trm_annot ~loc ~typ t0 sty)
              | Trm_match (t0, pts) ->
                (* In a pattern-matching, we just look for any type annotation in the branches. *)
                begin match
                  List.find_mapi (fun i (_p, t) ->
                    Option.map (fun r -> (i, r)) (aux t)) pts with
                | None -> None
                | Some (i, (typ, t')) ->
                  Some (typ, trm_match ~loc ~typ t0 (List.update_nth i (fun (p, _t) -> (p, t')) pts))
                end
              | Trm_if (t0, t1, t2) ->
                (* Similarly, we just need one type annotation for if-statements. *)
                begin match aux t1 with
                | Some (typ, t1') -> Some (typ, trm_if ~loc ~typ t0 t1' t2)
                | None ->
                  match aux t2 with
                  | None -> None
                  | Some (typ, t2') -> Some (typ, trm_if ~loc ~typ t0 t1 t2')
                end
              | Trm_let (let_def, t0) ->
                begin match aux t0 with
                | None -> None
                | Some (typ, t0') -> Some (typ, trm_let_def ~loc ~typ let_def t0')
                end
              | _ -> None in
            match aux t1 with
            | None -> raise (Error (No_annot_in_recusive_def, t1.trm_loc))
            | Some r -> r
          ) else (
            (* In the non-recursive case, there is no need to force the type to be annotated in the
              term. *)
            (typ_nameless (), t1)
          ) in
        let sch = { sch_tvars = tvars_rhs; sch_body = body } in
        (sch, tvars_rhs, t1, e1)
    | Some syntyp ->
        let sch = syntyp.synsch_sch in
        if tvars_rhs <> [] then begin
          let xs1 = tvars_rhs in
          let xs2 = sch.sch_tvars in
          let error = Error (Mismatching_type_annotation_names_in_declaration (xs1, xs2), loc) in
          if List.length xs1 <> List.length xs2 then raise error;
          List.iter2 (fun x1 x2 -> if x1 <> x2 then raise error) xs1 xs2
        end;
        let e2 =
          if tvars_rhs <> [] then e1 (* We have already added these variables, based on [tvars_rhs]. *)
          else (
            (* Add the universally quantified types into the environment. *)
            let tconstrs_for_typing_t1 = List.map (fun tvar -> typ_rigid tvar) sch.sch_tvars in
            env_add_tconstr_vars e sch.sch_tvars tconstrs_for_typing_t1
          ) in
        (sch, sch.sch_tvars, t1, e2) in
  (* If the definition is recursive, we add an entry in the environment
     before processing the body of the definition *)
  let e3 =
    if rf = Asttypes.Recursive then (
      env_add_var e2 x (Env_item_var sch)
    ) else e2 in
  (* Now we are ready to typecheck the body of the type scheme in that environment *)
  let t1' = typecheck_ml trigger_call e3 ~expected_typ:sch.sch_body t1 in
  let external_typ =
    let m =
      List.fold_left (fun m tv ->
        let n =
          let str = Var.print_tconstr tv in
          assert (str <> "") ;
          if str.[0] = '\'' then str
          else Printf.sprintf "'%s" str in
        RigidMap.add tv (typ_rigid (tvar_rigid n)) m) RigidMap.empty sch.sch_tvars in
    replace_rigids_with m sch.sch_body in
  let t' = trm_foralls ~typ:external_typ tvars_for_typing_t1 t1' in
  (t', sch)


(** [typecheck_let] typechecks a let-binding, both in a local [let _ = _ in _] and in a
  top-level declaration.
  It returns the new environment, as well as an updated let-binding and its associated type scheme. *)
and typecheck_let (trigger_call : trigger_call) ~loc (e : env) (b : let_def) : let_def * env * sch =
  let rf = b.let_def_rec in
  let t1 = b.let_def_body in
  let bind = b.let_def_bind in
  let return bind e t1 sch = ({ b with let_def_bind = bind ; let_def_body = t1 }, e, sch) in
  match bind with
  | Bind_anon ->
      let t1 = typecheck_ml trigger_call ~expected_typ:the_typ_unit e t1 in
      return bind e t1 (sch_of_nonpolymorphic_typ the_typ_unit)

  | Bind_var (x, synschopt) ->
      (* This case handles a binding of the form [let x = t1 in t2].
         It can be monomorphic or polymorphic (e.g. [let x : type a. a list = [] in t2]).
         Note that the [let[@instance]] form is a sequence of such a let-binding, with a
        [let[@register]. *)
      let synschopt = Option.map (synsch_internalize ~loc e) synschopt in
      let (t1, sch) = typecheck_ml_let_sch trigger_call ~loc rf e (x, synschopt) t1 in
      (* If no annotation was provided, we add one in order to be able to display the inferred
        type scheme. *)
      let synsch =
        match synschopt with
        | Some synsch -> synsch
        | None -> {
            synsch_syntax = (sch.sch_tvars, styp_any) ;
            synsch_sch = sch
          } in
      let e2 = env_add_var e x (Env_item_var sch) in
      return (Bind_var (x, Some synsch)) e2 t1 sch

  | Bind_register_instance (x, inst_sig) ->

      (* Declaration of an instance, e.g.
         [let[@instance (+)] matrix_add (type a) (op : a -> a -> a) = ...], which is compiled into:
         [let matrix_add (type a) (op:a->a->a) = ...
          let[@register (+)] _ = matrix_add]. *)

      (* First, internalising syntactic types, and preparing the environment to typecheck [t1]. *)
      let e1 =
        List.fold_left (fun e c ->
          let tc = {
            tconstr_tvars = [] ;
            tconstr_def = Tconstr_abstract ;
            tconstr_typ = Some (typ_rigid c)
          } in
          env_add_tconstr e c tc) e inst_sig.instance_tvars in
      let instance_assumptions =
        List.map (fun asmpt ->
          { asmpt with assumption_typ =
              syntyp_internalize e1 asmpt.assumption_typ }) inst_sig.instance_assumptions in
      let inst_sig = { inst_sig with instance_assumptions } in
      let ty_overall =
        typ_arrow_flexible (List.map (fun asmpt ->
          asmpt.assumption_typ.syntyp_typ) instance_assumptions) inst_sig.instance_typ in
      Debug.log "DEBUG: %s" (Ast_print.typ_to_string ty_overall) ;
      (* Typechecking [t1]. *)
      let t1 = typecheck_ml trigger_call ~expected_typ:ty_overall e1 t1 in
      let inst = {
        instance_value = t1 ;
        instance_sig = inst_sig ;
        instance_loc = loc ;
        instance_symbol = x
      } in
      (* Preparing the environment after the registering of the instance. *)
      let e' = env_add_instance ~loc e x inst in
      return (Bind_register_instance (x, inst_sig)) e' t1 (instance_sch inst_sig)


(** [typecheck_ml e t] typechecks [t] in [e] and returns the type of [t].
    If an expected type is provided, the type computed for [t] is unified
    with it at the very end of the process. There is no propagation
    downwards into the term of the expected type. *)

and typecheck_ml (trigger_call : trigger_call) ?(expected_typ:typ option) (e : env) (t : trm) : trm =
  let loc = t.trm_loc in
  let aux ?(env : env = e) ?(expected_typ:typ option) (t : trm) : trm =
    typecheck_ml trigger_call ?expected_typ env t in
  let return ?(annot = t.trm_annot) (typ : typ) (u : trm_desc) : trm =
    (* Unify typ with expected type if provided *)
    Option.iter (fun typ' ->
      unify_or_error trigger_call ~loc e typ typ'
        (Conflict_with_context (t, typ, typ'))) expected_typ;
    (* We also unify with the previously stored type in place, for the rare cases in which the parser
      shares types between terms. *)
    (* FIXME TODO: This fails when an instance take parameters as arguments: maybe the parser isn't
      generating the types the right way? *)
    unify_or_error trigger_call ~loc e t.trm_typ typ (Conflict_with_context (t, t.trm_typ, typ)) ;
    { trm_desc = u;
      trm_loc = loc;
      trm_typ = typ;
      trm_env = e;
      trm_annot = annot } in

  match t.trm_desc with
  | Trm_var x ->
      let varid = typecheck_variable trigger_call loc e x.varid_symbol in
      let ty = varid.varid_typ in
      return ty (Trm_var varid)

  | Trm_cst c ->
      (* At this stage, the syntactic constants have already been translated as an application
        of a symbol to the actual constant.
        We are here examining the constant argument of this symbol. *)
      return (typ_constant c) (Trm_cst c)

  | Trm_funs (args, t1) ->
      (* First, we refine the syntactic type annotations into typechecker types. *)
      let args = List.map (fun (arg, sty) -> (arg, syntyp_internalize e sty)) args in
      (* Second, we build the extended environment *)
      let env_items = List.map (fun (arg, sty) -> (arg, env_item_var_nonpolymorphic sty.syntyp_typ)) args in
      let e2 = List.fold_left (fun e (x, it) -> env_add_var e x it) e env_items in
      (* Third, we type the body, in the extended environment *)
      let ty_res =
        let ty =
          match expected_typ with
          | None -> t.trm_typ
          | Some ty -> ty in
        match typ_arrow_inv_opt ty with
        | None -> typ_nameless ()
        | Some (ty_args, ty_res) ->
          if List.length ty_args <> List.length args then
            raise (Error (Different_variables_number (List.length ty_args, List.length args), loc)) ;
          List.iter2 (fun ty1 (_x, ty_arg) ->
            let ty2 = ty_arg.syntyp_typ in
            unify_or_error trigger_call ~loc e ty1 ty2 (Unable_to_unify (ty1, ty2))) ty_args args ;
          ty_res in
      let t1 = aux ~env:e2 ~expected_typ:ty_res t1 in
      let typ = typ_arrow (List.map (fun (arg, sty) -> sty.syntyp_typ) args) ty_res in
      (* here we avoid the creation of a flexible that is immediately unified *)
      return typ (Trm_funs (args, t1))

  | Trm_if (t1, t2, t3) ->
      let t1 = aux ~expected_typ:the_typ_bool t1 in
      let t2 = aux t2 in
      let t3 = aux t3 in
      unify_or_error trigger_call ~loc e (typeof t2) (typeof t3) (Branches_mismatch_if (typeof t2, typeof t3));
      return (typeof t2) (Trm_if (t1,t2,t3))

  | Trm_let (b, t2) ->
      let (b, e, _sch) = typecheck_let trigger_call ~loc e b in
      let t2 = aux ~env:e t2 in
      return (typeof t2) (Trm_let (b, t2))

  | Trm_apps (t0, ts) ->
      let t0 = aux t0 in
      let ts = List.map aux ts in
      begin
        (* If [t0] is an overloaded varid, we register it again. *)
        match t0.trm_desc with
        | Trm_var ({ varid_resolution = VarUnresolved _ ; _ } as x) ->
          Option.iter (fun f -> f Trigger_App x) trigger_call
        | _ -> ()
      end ;
      let ty_ret = typ_nameless () in
      let ty_fun = typ_arrow (List.map typeof ts) ty_ret in
      unify_or_error trigger_call ~loc e (typeof t0) ty_fun (Application_mistyped (typeof t0, ty_fun));
      return ty_ret (Trm_apps (t0, ts))

  | Trm_annot (t1, sty) ->
      let sty = syntyp_internalize e sty in
      let t1 = aux ~expected_typ:sty.syntyp_typ t1 in
      return (typeof t1) (Trm_annot (t1, sty))

  | Trm_forall (_a, _t) ->
    (* This constructor should only appear at let-definitions. *)
    raise (Error (Unsupported_term, loc))

  | Trm_match (t0, pts) ->
      let t0 = aux t0 in
      let tyr = typ_nameless () in
      let pts =
        List.map (fun (pi, ti) ->
          let (pi, ei) = typecheck_pat trigger_call ~expected_typ:(typeof t0) e pi in
          let e' =
            Env.fold ei (fun e' x ty ->
              env_add_var e' x (Env_item_var (sch_of_nonpolymorphic_typ ty))) e in
          let ti = aux ~env:e' ~expected_typ:tyr ti in
          (pi, ti)) pts in
      return tyr (Trm_match (t0, pts))


(*******************************************)
(*******************************************)
(** * Ordered resolution *)

let varid_assumptions (varid:varid) : varid list =
  match varid.varid_resolution with
  | VarResolved (_, assumptions) -> assumptions
  | _ -> []

type 'a result =
  | Success of 'a
  | Fail

(* Given an unresolved varid, consider all its possible instances and filter the ones that are
  still compatible with the varid's type.
  The [trigger_call] argument is the trigger_call to be used for unifying with the unique candidate
  of a resolution, as well capturing the newly created varids of assumptions. *)
let try_resolve (trigger_call : trigger_call) varid : unit =
  Counters.(compute_count_and_time counter_resolution_attempt time_resolution_attempt (fun () ->
  let insts =
    match varid.varid_resolution with
    | VarUnresolved candidates -> candidates
    | _ -> assert false in
  let ty = Repr.get_repr varid.varid_typ in
  let is_instantiable (i : instance) : bool =
    incr Counters.counter_candidate_instantiable ;
    let sch = instance_sch i.instance_sig in
    (* For the [is_instance_unifiable] tests, [trigger_call] is [None]. *)
    is_instance_unifiable varid.varid_env (apply_to_fresh_flexibles sch) ty in
  let remaining_correct_instances =
    (* We stop at the second instantiable instance, as this implies that this symbol
     won't be resolved right away, and thus that we won't get new information now.
     This also means that this function will be passed later on, with more information. *)
    (* We could do an unoptimised version:
     [List.filter is_instantiable insts.candidates_and_modes_candidates]. *)
    let rec aux instantiable = function
      | [] -> Option.to_list instantiable
      | i :: l ->
        if is_instantiable i then (
          match instantiable with
          | None -> aux (Some i) l
          | Some i' ->
            (* Both [i] and [i'] are instantiable, so we stop the search here. *)
            i :: i' :: l
        ) else aux instantiable l in
    let candidates = insts.candidates_and_modes_candidates in
    match ty.typ_desc, candidates with
    | Flexible _, _ :: _ :: _ -> candidates (* The flexible variable is always instantiable. *)
    | _, _ -> aux None candidates in
  match remaining_correct_instances with
  | [] ->
    raise (Error (Instance_all_mismatch (varid.varid_symbol, varid.varid_context, ty, insts), varid.varid_loc))
  | [i] ->
    if varid.varid_depth = !Flags.max_varid_depth then
      raise (Error (Maximum_varid_depth_reached, varid.varid_loc)) ;
    let assumptions =
      unify_with_instance trigger_call ~loc:varid.varid_loc varid.varid_env ty
        ~depth:(1 + varid.varid_depth) ~context:varid.varid_symbol i in
    Debug.log "Deduce that %s resolves to the instance declared at %s : %s."
      (Ast_print.symbol_to_string varid.varid_symbol)
      (Ast_print.print_loc i.instance_loc)
      (Ast_print.typ_to_string i.instance_sig.instance_typ) ;
    varid.varid_resolution <- VarResolved (i, assumptions)
  | _ ->
    varid.varid_resolution <- VarUnresolved { insts with candidates_and_modes_candidates = remaining_correct_instances }
  ))

(** Performs resolution in AST order, with handling of overloaded
    function before and after resolution inside arguments.
    Returns the number of resolved symbols at this step and
    the number of remaining unresolved symbols. *)
let ordered_resolve_in (t:trm) : int * int =
  let nb_resolved_at_this_traversal = ref 0 in
  let nb_unresolved = ref 0 in
  let rec process varid : unit =
    let was_resolved = varid_is_resolved varid in
    (* With this kind of syntax-guided resolution, we ignore triggers. *)
    if not was_resolved then try_resolve None varid;
    List.iter process (varid_assumptions varid); (* more of the trm_map *)
    if varid_is_resolved varid then begin
      if not was_resolved
        then incr nb_resolved_at_this_traversal;
    end else begin
      (* not resolved *)
      incr nb_unresolved
    end in
  let rec resolve_in t =
    match t.trm_desc with
    | Trm_var varid -> process varid
    | Trm_apps ({ trm_desc = Trm_var varid; _ }, _ts) ->
        trm_iter resolve_in t; (* includes a call on varid *)
        process varid
    | _ -> trm_iter resolve_in t in
  resolve_in t;
  (!nb_resolved_at_this_traversal, !nb_unresolved)

(** Iterate ordered resolution. Returns the number of steps until
    convergence. Returns a boolean indicating the success of
    resolution for all varids. *)
let ordered_resolution ?(max_traversals = max_int) (t:trm) : bool * int =
  let nb_traversals = ref 0 in
  let converged = ref false in
  let success = ref false in
  while not !converged && !nb_traversals < max_traversals do
    incr nb_traversals;
    let (nb_resolved_at_this_traversal, nb_unresolved) = ordered_resolve_in t in
    if nb_resolved_at_this_traversal = 0
      then converged := true;
    success := (nb_unresolved = 0);
  done;
  (!success, !nb_traversals)

let try_resolve_with_result (trigger_call : trigger_call) varid : bool =
  match varid.varid_resolution with
  | VarUnknown | VarRegular -> assert false
  | VarResolved _ -> true
  | VarUnresolved _ ->
    Debug.print_current_top_level_resolving varid ;
    try_resolve trigger_call varid;
    match varid.varid_resolution with
    | VarUnknown | VarRegular -> assert false
    | VarResolved (_instance, assumptions) ->
      Debug.print_current_top_level_resolved varid assumptions ;
      (* FIXME: should we do add_triggers_into_typ into the type of assumptions? *)
      true
    | VarUnresolved _ -> false


(* A stack in which all elements are at most present once. *)
module StackUnique (E : sig
      (* Elements are assumed to be able to carry a boolean, to store whether
        they are already within the stack.
        This boolean should only be used by this module. *)
      type t
      val get : t -> bool
      val set : t -> bool -> unit
      val print : t -> string
      val stack_name : string
    end) : sig
    val is_empty : unit -> bool
    val add : E.t -> unit
    val adds : E.t list -> unit
    val pop : unit -> E.t (* Assumes a non-empty stack as an argument. *)
    val clear : unit -> unit
  end = struct

  type t = E.t list ref

  let s : t = ref []

  let is_empty () = !s = []

  let add e =
    if not (E.get e) then (
      Debug.log "Pushing %s to the stack %s." (E.print e) E.stack_name ;
      E.set e true ;
      s := e :: !s
    )

  let adds es = List.iter add es

  let pop () =
    match !s with
    | [] -> assert false
    | e :: l ->
      assert (E.get e) ;
      Debug.log "Popping %s from the stack %s." (E.print e) E.stack_name ;
      E.set e false ;
      s := l ;
      e

  let clear () =
    Debug.log "Clearing stack %s." E.stack_name ;
    List.iter (fun e ->
      assert (E.get e) ;
      E.set e false) !s ;
    s := []

end

(* Instead of storing the boolean within the elements, one can use an
  external set instead. *)
module StackUniqueSet (E : sig
      include Map.OrderedType
      val print : t -> string
      val stack_name : string
    end) =
  StackUnique (struct
    include E
    module ESet = Set.Make (E)

    let s = ref ESet.empty

    let get e = ESet.mem e !s
    let set e = function
      | true -> s := ESet.add e !s
      | false -> s := ESet.remove e !s
  end)

let iterative_resolution (all_varids : varid list) : unit =
  Debug.log "Starting iterative resolution." ;
  let is_resolved x =
    match x.varid_resolution with
    | VarRegular | VarResolved _ -> true
    | _ -> false in
  let print_varid = Ast_print.(varid_to_string ~style:style_debug) in
  let module StronglyTriggered =
    StackUnique (struct
      type t = varid
      let get x = x.varid_marker_strong
      let set x b = x.varid_marker_strong <- b
      let print = print_varid
      let stack_name = "strong"
    end) in
  let module WeaklyTriggered =
    StackUnique (struct
      type t = varid
      let get x = x.varid_marker_weak
      let set x b = x.varid_marker_weak <- b
      let print = print_varid
      let stack_name = "weak"
    end) in
  let current = Stack.create () in
  let next = Stack.create () in
  let get_next_triggered () : varid option =
    if not (StronglyTriggered.is_empty ()) then Some (StronglyTriggered.pop ())
    else if not (WeaklyTriggered.is_empty ()) then Some (WeaklyTriggered.pop ())
    else None in
  (* Pushing all the varid in reverse into the current stack. *)
  List.iter (fun x -> Stack.push x current) all_varids ;
  let success = ref false in
  let stuck = ref false in
  let trigger_call =
    Some (fun priority x ->
      match priority with
      | Trigger_Strong -> StronglyTriggered.add x
      | Trigger_App -> assert false
      | Trigger_Weak -> WeaklyTriggered.add x
      | Trigger_Create ->
        if not (is_resolved x) then (
          StronglyTriggered.add x ;
          Stack.push x current
        )) in
  let finally () =
    StronglyTriggered.clear () ;
    WeaklyTriggered.clear () in
  try (
    while not !success && not !stuck do
      incr Counters.counter_passes ;
      let progress = ref false in
      let attempt_resolution failure_case x =
        Debug.log "Attempting resolution of %s." (print_varid x) ;
        if try_resolve_with_result trigger_call x then (
          progress := true
        ) else failure_case () in
      while not (Stack.is_empty current) do
        let x = Stack.pop current in
        (* The current symbol could be resolved via a trigger. *)
        if not (is_resolved x) then begin
          attempt_resolution (fun () -> Stack.push x next) x ;
          (* We then progress using triggers. *)
          let nb_triggers_passes_left = ref !Flags.number_of_trigger_passes in
          while !nb_triggers_passes_left > 0 do
            match get_next_triggered () with
            | None -> nb_triggers_passes_left := 0
            | Some y -> attempt_resolution (fun () -> decr nb_triggers_passes_left) x ;
          done ;
          if !Flags.clear_triggered_after_loop then (
            StronglyTriggered.clear () ;
            WeaklyTriggered.clear ()
          )
        end ;
      done ;
      (* Detect termination, or prepare next phase. *)
      if Stack.is_empty next then success := true
      else if not !progress then stuck := true
      else (
        (* We reverse the stack next into current. *)
        assert (Stack.is_empty current) ;
        while not (Stack.is_empty next) do
          Stack.push (Stack.pop next) current
        done
      )
    done ;
    finally () ;
    if !stuck && !Flags.force_complete_resolution then raise IncompleteResolution
  ) with e -> (
    finally () ;
    raise e
  )


(* Return two error messages: one meant for the term annotation (in [@type_error "msg"],
  and the other meant for the user. *)
let get_error_messages ~style ?(loc = loc_none) error =
  let open Errors_aux in
  let serror = string_of_error ~style error in
  let serror_full = Printf.sprintf "%s\n  %s" (Ast_print.print_loc loc) serror in
  (string_of_error_short error, serror_full)

(* Run the function [f] on the top-level declaration [td] and checks that
  the kind of result (error or normal result) matches what was expected
  (with indications like [let[@type_error "expected message"] foo]). *)
let check_error ?(exact_error_messages = true) ~style fallback_result td f =
  let open Printf in
  let open Ast_print in
  (* Expected error, for tests, within let[@type_error "…"] declarations. *)
  let err = td.topdef_expected_error in
  let name =
    match td.topdef_desc with
    | Topdef_val_def { let_def_bind = Bind_var (x, _aty) ; _ } -> Var.print_var x
    | Topdef_external { external_def_var = n ; _ } -> Var.print_var n
    | Topdef_typ_def { typ_def_td = { Parsetree.ptype_name = n ; _ } :: _ ; _ } -> n.Asttypes.txt
    | _ -> "<no name in context>" in
  let context_msg =
    let loc = td.topdef_loc in
    sprintf "Unexpected behavior in the declaration of %s (%s):\n" name (print_loc loc) in
  match f td with
  | r ->
    begin match err with
    | None -> r
    | Some msg ->
      eprintf "%s" context_msg ;
      eprintf "Error expected: %s\n" msg ;
      eprintf "Actual result: no error.\n%!" ;
      Debug.log "🟧 Unexpected pass." ;
      fallback_result
    end
  | exception (Error (error, loc)) ->
    let loc = refine_loc td.topdef_loc loc in
    begin
      let (serror, serror_full) = get_error_messages ~style ~loc error in
      match err with
      | None ->
          raise (Typecheck_error serror_full)
      | Some msg ->
        if exact_error_messages && msg <> serror then begin
          eprintf "%s" context_msg ;
          eprintf "Error expected: %s\n" msg ;
          eprintf "Error obtained: %s\n" serror ;
          eprintf "Full error message: %s\n%!" serror_full ;
          Debug.log "🟧 Unexpected error message."
        end else (
          Debug.log "Error obtained: %s\n" serror ;
          Debug.log "🟨 Skipping expected error in def of: %s." name ;
        ) ;
        fallback_result
    end

(* Print the types of terms after typing them. *)
let print_types = ref false

let typecheck_topdef ?exact_error_messages ~style (env : env) (td : topdef) : topdef * env =
  let open Printf in
  let open Ast_print in
  check_error ?exact_error_messages ~style (td, env) td (fun td ->
    Debug.log "\n=Typechecking the top-level declaration:\n%s" (topdef_to_string ~style td) ;
    let loc = td.topdef_loc in
    match td.topdef_desc with

    (* TODO FACTORIZE *)

    (* Switch the printing mode: [let __print = false] or [let __print = true] *)
    | Topdef_val_def { let_def_bind = Bind_var (c, _) ; let_def_body = t ; _ } when c = Var.var "__print" ->
        begin match t.trm_desc with
        | Trm_cst (Cst_bool b) ->
            print_types := b;
            (td, env)
        | _ -> raise_typecheck_error loc "a [let __print =] must be followed by a boolean"
        end

    (* Switch to the debug mode: [let __debug = 0] or [let __debug = 1],
       or [let __debug = -1] to exit the program *)
    | Topdef_val_def { let_def_bind = Bind_var (c, _) ; let_def_body = t ; _ } when c = Var.var "__debug" ->
        begin match t.trm_desc with
        | Trm_cst (Cst_int n) ->
            if n = -1 then begin
              printf "Interrupted by [let __debug = -1]\n";
              exit 1
            end else begin
              Flags.debug := (n <> 0);
            end;
            (td, env)
        | _ -> raise_typecheck_error loc "a [let __debug =] must be followed by an integer"
        end

    (* Process a value definition *)
    | Topdef_val_def d ->
        (* Then we process the body of the definition, and obtain an environment
           extended with the binding that corresponds to the current value definition.
           We request for a top-level definition that its type be fully resolved. *)
        let ((d, env', sch), varids) =
          let varids = ref [] in
          let trigger_call =
            Some (fun event x ->
              if event = Trigger_Create then
                varids := x :: !varids) in
          let r =
            Counters.(compute_and_time time_ml_constraints (fun () -> typecheck_let trigger_call ~loc:td.topdef_loc env d))
            in
          (r, !varids) in
        if !Flags.counters_only_for_resolutions_in_last_topdef
          then Counters.reset_stats_except_time_ml_constraints ();
        Counters.(compute_and_time time_symbol_resolution (fun () ->
          iterative_resolution varids));
        Debug.log "🟩 Conclude that this definition has the type scheme %s." (sch_to_string sch) ;
        let sym =
          match d.let_def_bind with
          | Bind_anon -> SymbolName (Var.var "<anonymous>")
          | Bind_var (x, _) -> SymbolName x
          | Bind_register_instance (sym, _) -> sym in
        Counters.(compute_and_time time_check_fully_typed (fun () ->
          check_fully_typed ~in_depth:true ~test_acylic:true (snd (trm_foralls_inv d.let_def_body)) sym));
        ({ td with topdef_desc = Topdef_val_def d }, env')

    (* Process the declaration of an external function, e.g.
       [external ( = ) : 'a. 'a -> 'a -> bool = "%equal"] *)
    | Topdef_external tde ->
        let synsch = synsch_internalize ~loc:td.topdef_loc env tde.external_def_syntyp in
        let tde = { tde with external_def_syntyp = synsch } in
        let env = env_add_var env tde.external_def_var (Env_item_var synsch.synsch_sch) in
        Debug.log "🟩 Internalised as %s." (sch_to_string synsch.synsch_sch) ;
        ({ td with topdef_desc = Topdef_external tde }, env)


    (* Process a type definition *)
    | Topdef_typ_def tdt ->
        let tds = tdt.typ_def_td in
        (* First, we add dummy definition for all the types. *)
        let env =
          if tdt.typ_def_rec = Asttypes.Recursive then
            List.fold_left add_dummy_type env tds
          else env in
        (* Then, we actually process the type declarations. *)
        let (env, tcs) =
          List.fold_left (fun (env, tcs) td ->
            try
              let (env, tc) = env_add_type_declaration env td in
              (env, tc :: tcs)
            with Failure msg ->
              raise_typecheck_error loc msg) (env, []) tds in
        let tdt = { tdt with typ_def_typs = List.rev tcs } in
        Debug.log "🟩 Done." ;
        ({ td with topdef_desc = Topdef_typ_def tdt }, env)

    (* | _ -> raise (Error (Unsupported_term, td.topdef_loc)) *)
  )

let typecheck_program ?exact_error_messages ?(continue_on_error = false) ~style (tds: topdefs) : topdefs =
  let (env, tds) =
    List.fold_left (fun (env, tds) td ->
      let (td, env) =
        try typecheck_topdef ?exact_error_messages ~style env td with
        | Typecheck_error msg when continue_on_error ->
          prerr_endline (Printf.sprintf "🟥 Type error: %s" msg) ;
          (td, env) in
      (env, td :: tds)) (env_builtin_tuples (), []) tds in
  Debug.log "++++++++++++++++++++++++++++++++++" ;
  List.rev tds

