open Ast_aux
open Ast_fix
open Blocks
open Errors
open Var

let debug_env = false

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
      style_print_symbols = !Flags.print_raw_symbols ;
      style_binds = !Flags.style_binds ;
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

let bindsof (t : trm) : env =
  match t.trm_binds with
  | Some bs -> bs
  | None -> failwith (Printf.sprintf "Call to bindsof with a term with an empty trm_binds : %s" (trm_to_string t))

let mk_env_binds (bs : env_var) : env =
  {env_empty with env_var = bs}

(** * Custom functions, to be put elsewhere later *)

(** [merge_distinct e1 e2]: merges the environments e1 and e2, raising an error if there is shadowing *)
let merge_distinct ~loc (e1 : env_var) (e2 : env_var) : env_var =
  Env.fold e2 (fun e k v ->
    (if (Env.mem e1 k) then raise (Error (Expected_bindings, loc));
    (Env.add e k v))) e1

(** [env_extend e1 e2]: adds the bindings of binds inside e1.*)
let env_extend ~loc (e1 : env) (binds : env) : env = (* TODO: env_bbe = env_var, and binds/"eb" has type env_bbe *)
  assert (Env.size binds.env_tconstr = 0); (* it is assumed no expression can bind type constructors *) (* why is that? I mean how can there be extension of tconstr in binds? *)
  {e1 with env_var = (merge_distinct ~loc e1.env_var binds.env_var)}

let env_merge_binds ~loc (bl : env list) : env =
  let ev = env_empty in
  List.fold_left (env_extend ~loc) ev bl

let unify_or_error_sch ~loc (e : env) (s1 : sch) (s2 : sch) : unit =
  assert (s1.sch_tvars = [] && s2.sch_tvars = []); (* v1 and v2 are necessarly non polymorphic *)
  unify_or_error ~loc e s1.sch_body s2.sch_body (Unable_to_unify (s1.sch_body, s2.sch_body))

(* Takes the env_var of both. Discard any variable that is not in both at the same time, and unify_or_error the schemes (or whatever the operation is) when there is a conflict.
Return a resulting env, with empty env_tconstr. *)
let env_intersect_vars ~loc (curr_env : env) (b1 : env_var) (b2 : env_var) : env_var =
  Env.fold b1 (fun e k s1 ->
  begin match Env.read_option b2 k with
  | None -> e
  | Some s2 ->
    unify_or_error_sch ~loc curr_env s1 s2;
    Env.add e k s1
  end
  ) (Env.empty ())

let env_intersect ~loc (curr_env : env) (b1 : env) (b2 : env) : env =
  mk_env_binds (env_intersect_vars ~loc curr_env b1.env_var b2.env_var)

let env_singleton (v : varid) (ty : typ) : env =
  env_add_var (env_empty) v (sch_of_nonpolymorphic_typ ty)

let typ_constant = function
  | Cst_bool _ -> the_typ_bool
  | Cst_int _ -> the_typ_int
  | Cst_float _ -> the_typ_float
  | Cst_string _ -> the_typ_string
  | Cst_unit _ -> the_typ_unit

let tconstr_inv (e : env_tconstr) (v : var) : bool =
  Env.mem e (string_to_tconstr v)

let tconstr_inv_bind (e : env_tconstr) (v : var) : tconstr_desc option =
  Env.read_option e (string_to_tconstr (print_var v))

(* TODO "simplification of Env" : rewrite this without the list with Env has been simplified *)

(* "AC TODO": rename
   [interpret_pat_var ev v] looks for 'Pattern_${v}' in [ev],
   if it is found, returns 'Pattern_${v}', otherwise returns [v]. *)
let try_read_pattern ~loc (ev : env_var) (v : varid) : varid =
  let flattened = Env.to_list ev in
  let pattern_v = "Pattern__" ^ v in

  match List.find_opt (fun (pv, _) -> pv = pattern_v) flattened with
  | Some (pv, _)->
    if !Flags.verbose then Printf.printf "Found \"%s\"\n" pattern_v;
    pv
  | None ->
    if !Flags.verbose then Printf.printf "variable %s does not have a \"Pattern__\" version\n" v;
    begin match List.find_opt (fun (pv, _) -> pv = v) flattened with
    | Some (pv, _) -> pv
    | None -> raise (Error (Unbound_variable v, loc))
    end

let trm_var_inv_opt (t : trm) : varid option =
  match t.trm_desc with
  | Trm_var v -> Some v
  | _ -> None

let typ_of_some_or_nameless (exp_typ : typ option) : typ =
  match exp_typ with
  | Some ty -> ty
  | _ -> typ_nameless ()

let is_typ_top (t : typ) : bool =
  let t = Repr.get_repr t in
  match t.typ_desc with
  | Typ_constr (tc, _) -> ((print_tconstr tc) = "type_top")
  | _ -> false

let is_sch_typ_top (s : sch) : bool =
  (s.sch_tvars = []) && (is_typ_top s.sch_body)

let is_synschopt_typ_top (s : synsch option) : bool =
  match s with
  | None -> false
  | Some synsch -> is_sch_typ_top synsch.synsch_sch

(** End of custom functions *)


(* Checking that two environments as computed by typecheck_pat below are the same. *)
(* let check_same_domains_in_pattern  (e : env) ~loc (e1 : (var, typ) Env.t) (e2 : (var, typ) Env.t) : unit =
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
      unify_or_error  ~loc e ty1 ty2
        (Conflict_with_context (trm_var ~loc ~typ:ty1 x, ty1, ty2))
  ) ()
 *)
(* TODO   let env_lookup_or_error e v : sch *)

  (* [typecheck_variable e v] looks up [v] in [e] to get its type. *)
let typecheck_variable loc (e : env) (v : var) : typ =
      (* if env_is_in_pattern  chercher d'abord "Pattern__" ^ v TODO *) (* AC *)
  let s =
    match Env.read_option e.env_var v with
    | None -> raise (Error (Unbound_variable v, loc)) (* SymbolName is a temporary solution. Later replace the type of Unbound_variable to var, or even string. *)
    | Some s -> s in
  (* We compute the type of the variable, based on the item associated
    with [sym] in the environment.
    - If the item is a conventional type scheme, then the type of [sym]
      is obtained as the instantiation of this type scheme on fresh variables.
    *)
  let typ = apply_to_fresh_flexibles s in
  typ

(* TODO remove *)
(* For each pattern, we returned the typed pattern and the environment it introduces. *)
(* let rec typecheck_pat  ?(expected_typ:typ option) (e : env) (p : pat) : pat * (var, typ) Env.t =
  let loc = p.pat_loc in
  let aux ?(env : env = e) ?(expected_typ:typ option) (p : pat) : pat * (var, typ) Env.t =
    typecheck_pat  ?expected_typ env p in
  let return (typ : typ) (p_d : pat_desc) ex : pat * (var, typ) Env.t =
    (* Unify typ with expected type if provided *)
    Option.iter (fun typ' ->
      unify_or_error  ~loc e typ typ'
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
    let (x, tyx) = typecheck_variable loc e (Var.constr_to_var c) in
    let tyr = typ_nameless () in
    let ty = typ_arrow_flexible (List.map (fun p -> p.pat_typ) ps) tyr in
    unify_or_error ~loc e ty tyx
      (Conflict_with_context (trm_var_varid ~loc ~typ:tyx x, ty, tyx)) ;
    return tyr (Pat_construct (c, ps)) e'

  | Pat_constraint (p, sty) ->
    let sty = syntyp_internalize e sty in
    let (p, e) = aux ~expected_typ:sty.syntyp_typ p in
    return p.pat_typ (Pat_constraint (p, sty)) e

  | Pat_or (p1, p2) ->
    let (p1, e1) = aux p1 in
    let (p2, e2) = aux p2 in
    check_same_domains_in_pattern  e ~loc e1 e2 ;
    unify_or_error  ~loc e p1.pat_typ p2.pat_typ
      (Conflict_with_context_pattern (p2, p1.pat_typ, p2.pat_typ)) ;
    return p1.pat_typ (Pat_or (p1, p2)) e1
 *)
(* FIXME: trm_desc should be u not t nor e *)

(* ARTHUR: check this *)
(** [typecheck_trm_let_sch] typechecks its provided term [t] and returns its type scheme.
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
let rec typecheck_trm_let_sch  ~loc ?(fully_resolved=false) rf (e : env) ((x, synschopt) : varsynschopt) (t : trm) : trm * sch =
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
        Printf.printf "Got sch = %s\n" (Ast_print.sch_to_string sch);
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
      env_add_var e2 x sch
    ) else e2 in
  (* Now we are ready to typecheck the body of the type scheme in that environment *)
  let t1' = typecheck_trm  e3 ~expected_typ:sch.sch_body t1 in
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
  (t', if !Flags.weak_typer then (sch_of_nonpolymorphic_typ the_typ_top) else sch) (* YL: low chances that this works) *)


(** [typecheck_let] typechecks a let-binding, both in a local [let _ = _ in _] and in a
  top-level declaration.
  It returns the new environment ([e] extended with the binding of the let_def),
   as well as an updated let-binding, and its associated type scheme
   (which seems only used for debugging). *)
and typecheck_let  ~loc (e : env) (b : let_def) : let_def * env * sch =
  let rf = b.let_def_rec in
  let t1 = b.let_def_body in
  let bind = b.let_def_bind in
  let return bind e t1 sch =
    let sch =
      if not !Flags.weak_typer
      then (sch_of_nonpolymorphic_typ the_typ_top)
      else sch
    in
    ({ b with let_def_bind = bind ; let_def_body = t1 }, e, sch) in
  match bind with
  | Bind_anon ->
    let t1 = typecheck_trm ~expected_typ:the_typ_unit e t1 in
    return bind e t1 (sch_of_nonpolymorphic_typ the_typ_unit)
  | Bind_var (x, synschopt) ->
      (* This case handles a binding of the form [let x = t1 in t2].
         It can be monomorphic or polymorphic (e.g. [let x : type a. a list = [] in t2]).
         Note that the [let[@instance]] form is a sequence of such a let-binding, with a
        [let[@register]. *)
      let (t1, sch) =
        (* temp solution *)
        match synschopt with
        | Some synsch ->
          if is_sch_typ_top synsch.synsch_sch then
          (t1, synsch.synsch_sch)
          else
            let synschopt = Some (synsch_internalize ~loc e synsch) in
            typecheck_trm_let_sch ~loc rf e (x, synschopt) t1
        | _ -> typecheck_trm_let_sch ~loc rf e (x, synschopt) t1
      in

      (* If no annotation was provided, we add one in order to be able to display the inferred
        type scheme. *)
      let synsch =
        match synschopt with
        | Some synsch -> synsch
        | None -> {
            synsch_syntax = (sch.sch_tvars, styp_any) ;
            synsch_sch = sch
          } in
      let e2 = env_add_var e x sch in
      return (Bind_var (x, Some synsch)) e2 t1 sch

(*   | Bind_register_instance (x, inst_sig) ->

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
      let t1 = typecheck_trm  ~expected_typ:ty_overall e1 t1 in
      let inst = {
        instance_value = t1 ;
        instance_sig = inst_sig ;
        instance_loc = loc ;
        instance_symbol = x
      } in
      (* Preparing the environment after the registering of the instance. *)
      let e' = env_add_instance ~loc e x inst in
      return (Bind_register_instance (x, inst_sig)) e' t1 (instance_sch inst_sig) *)
    (* | _ -> failwith "Unexpected register/instance in a let binding." *)


(** [typecheck_trm e t] typechecks [t] in [e] and returns [t] with the correct type.
    If an expected type is provided, the type computed for [t] is unified
    with it at the very end of the process. There is no propagation
    downwards into the term of the expected type. *)

    (* TODO (low efforts) : rename to typecheck_trm *)
and typecheck_trm ?(expected_typ:typ option) (e : env) (t : trm) : trm =

  if !Flags.verbose && !Flags.debug then Printf.printf "Entering typecheck_trm with :\n  %s\n" (* (Ast_print.env_to_string ~style:Ast_print.style_debug e ) *) (trm_to_string t);

  let loc = t.trm_loc in
  let aux ?(expected_typ:typ option) ?(env : env = e) (t : trm) : trm =
    typecheck_trm ?expected_typ env t in
  let aux_bbe ?(env : env = e) (b : bbe) : bbe =
    typecheck_bbe env b in
  let return (* ?(annot = t.trm_annot) *) (typ : typ) (u : trm_desc) : trm =
    (* Unify typ with expected type if provided *)
    Option.iter (fun typ' ->
      unify_or_error ~loc e typ typ'
        (Conflict_with_context (t, typ, typ'))) expected_typ;
    (* We also unify with the previously stored type in place, for the rare cases in which the parser
      shares types between terms. *)
    { trm_desc = u;
      trm_loc = loc;
      trm_typ = if !Flags.weak_typer then the_typ_top else typ;
      trm_env = e;
      trm_binds = None (* ;
      trm_annot = annot  *)} in

  let result =
    (* If we are in 'pattern' mode, then variables are interpreted in the
       module 'Pattern__' by default (at depth 1).
       If we see anything else than a variable, we no longer interpret
       variables in the 'Pattern__' module. *)
    if e.env_is_in_pattern then
      match t.trm_desc with
      | Trm_var v ->
        let pat_v = try_read_pattern ~loc e.env_var v in
        let tyx = typecheck_variable loc e pat_v in
        return tyx (Trm_var v)
      | _ -> typecheck_trm {e with env_is_in_pattern = false} t
    else
  begin
  match t.trm_desc with
  | Trm_var x ->
      let tyx = typecheck_variable loc e x in
      return tyx (Trm_var x)

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

      (* TODO: try simpler *)
      let t1 = aux ~env:e2 t1 in
      let typ = typ_arrow (List.map (fun (arg, sty) -> sty.syntyp_typ) args) (typeof t1) in
      return typ (Trm_funs (args, t1))

      (* let ty_res =
        let ty = (* ty is the type of the function *)
          match expected_typ with
          | None -> t.trm_typ (* would be typ_nameless then *)
          | Some ty -> ty in
        match typ_arrow_inv_opt ty with
        | None -> typ_nameless ()
        | Some (ty_args, ty_res) ->
          if List.length ty_args <> List.length args then
            raise (Error (Different_variables_number (List.length ty_args, List.length args), loc)) ;
          List.iter2 (fun ty1 (_x, ty_arg) ->
            let ty2 = ty_arg.syntyp_typ in
            unify_or_error  ~loc e ty1 ty2 (Unable_to_unify (ty1, ty2))) ty_args args ;
          ty_res in
      let t1 = aux ~env:e2 ~expected_typ:ty_res t1 in
      let typ = typ_arrow (List.map (fun (arg, sty) -> sty.syntyp_typ) args) ty_res in
      (* here we avoid the creation of a flexible that is immediately unified *)
      return typ (Trm_funs (args, t1)) *)

  | Trm_if (b, t1, t2) ->
      let b = aux_bbe b in
      (* TODO: the intersection between the two environments *)
      let t1 = aux ~env:(env_extend ~loc e (bindsof b)) t1 in
      let t2 = aux t2 in
      (* if !Flags.verbose then Printf.printf "Types : \n %s : %s \n %s : %s \n %s : %s\n" (trm_to_string b)(Ast_print.typ_to_string b.trm_typ) (trm_to_string t1) (Ast_print.typ_to_string t1.trm_typ) (trm_to_string t2) (Ast_print.typ_to_string t2.trm_typ); *)
      unify_or_error ~loc e (typeof t1) (typeof t2) (Branches_mismatch_if (typeof t1, typeof t2));
      return (typeof t2) (Trm_if (b,t1,t2))

  | Trm_let (b, t2) ->
      let (b, e, _sch) = typecheck_let ~loc e b in
      let t2 = aux ~env:e t2 in
      return (typeof t2) (Trm_let (b, t2))


  (* YL : Still a prototype. TODO "on-the-fly boolof": think about it when everything is done. *)
   (* LATER: On-the-fly boolof
   | Trm_apps (t0, ps) when is_builtin_func_and t0 || is_builtin_func_or t0
   | Trm_bbe_is (t0, p) ->
      let b = aux_bbe e t in
      return the_type_bool b.trm_desc
    *)

  (* TODO Trm_apps (t0, ts) when t0 is tuple constructor ...
    type ts (list map);
    return (typ_prod (List.map typeof ts) (Trm_apps (t0, ts))
    *)

  | Trm_apps (t0, ts) ->
      let t0 = aux t0 in
      let ts = List.map aux ts in
      let ty_ret = typ_nameless () in
      let ty_ts = List.map typeof ts in
      let ty_fun = typ_arrow ty_ts ty_ret in
      (* LATER: remove debug.
       Problem here. We expect a type construction of the form "arrow [typeof ts] ty_ret", but we have something of the form "arrow 'a ('a option)". And there is no trivial translation from arrow to tuple.
      *)

      (* TODO:
         match typ_arrow_inv (typeof t0) with Some ([ty_arg],_) when length ts > 1
         => insérer un tuple et faire le unify_or_error
          else truc par défaut *)
      (* Resolving ambiguity between a currified call to n terms, or a call to an n-ary tuple *)
      if try_unify e (typeof t0) ty_fun then
        return ty_ret (Trm_apps (t0, ts)) (* ok *)
      else if List.length ts > 1 then begin (* why is that? maybe the arguments were passed as an nary, and then translated back? *)
        let typ_fun_one = typ_arrow [(typ_tuple ty_ts)] ty_ret in
        unify_or_error ~loc e (typeof t0) typ_fun_one (Application_mistyped (typeof t0, ty_fun));
        return ty_ret (Trm_apps (t0, [trm_tuple ts]))
      end else
        raise (Error (Application_mistyped (typeof t0, ty_fun), loc))


  | Trm_annot (t1, sty) ->
      let sty = syntyp_internalize e sty in
      let t1 = aux ~expected_typ:sty.syntyp_typ t1 in
      return (typeof t1) (Trm_annot (t1, sty))

  | Trm_forall (_a, _t) ->
    (* This constructor should only appear at let-definitions. *)
    raise (Error (Unsupported_term "Trm_forall", loc))

  | Trm_match (t0, pts) ->
    let t0 = aux t0 in
    let tyr = typ_nameless () in
    let pts =
      List.map (fun (pi, ti) ->
        let pi = typecheck_pat ~expected_typ:(typeof t0) e pi in
        let ei = env_extend ~loc e (bindsof pi) in
        let ti = aux ~env:ei ~expected_typ:tyr ti in
        (pi, ti)) pts in
    return tyr (Trm_match (t0, pts))

  | Trm_tuple ts ->
    (* LATER: could test
        | Trm_apps "__tuple" ts
        before the generic Trm_apps *)
    let ts = List.map aux ts in
    let ty_tuple = typ_tuple (List.map typeof ts) in
    if !Flags.verbose then (* LATER: remove this debugging stuff *)
      begin
      Printf.printf "Tuple :\n";
      List.iter (fun arg -> Printf.printf "%s : %s\n" (trm_to_string arg) (Ast_print.typ_to_string arg.trm_typ)) ts;
    end;
    return ty_tuple (Trm_tuple ts)

  (* Handling boolean operators. Very trivial typing for terms. *)
  (* LATER: this might become std functions *)
  | Trm_not t ->
    let t = aux t in
    return the_typ_bool (Trm_not t)
  | Trm_and (t1, t2) ->
    let t1 = aux t1 in
    let t2 = aux t2 in
    return the_typ_bool (Trm_and (t1, t2))
  | Trm_or (t1, t2) ->
    let t1 = aux t1 in
    let t2 = aux t2 in
    return the_typ_bool (Trm_or (t1, t2))
  | Trm_switch cases ->
    let type_case (b, t) =
      let b = aux_bbe b in
      let e = env_extend ~loc e (bindsof b) in
      let t = aux ~env:e t in
      (b, t)
    in
    let cases = List.map type_case cases in
    let ty_ret = typ_of_some_or_nameless expected_typ in
    List.iter (fun (_, t) ->
      let ty = typeof t in
      unify_or_error ~loc e ty_ret ty (Mismatch_type_switch (ty_ret, ty)))
      (*  *)
    cases;

    return ty_ret (Trm_switch cases)

  | Trm_while (b, t) ->
    let b = aux_bbe b in
    let e = env_extend ~loc e (bindsof b) in
    let t = aux ~expected_typ:the_typ_unit ~env:e t in
    return the_typ_unit (Trm_while (b, t))

  | _ -> failwith "unexpected construct in typecheck_trm"
  (* | Trm_bbe_is _ -> raise (Error (Unsupported_term "Trm_bbe_is", loc))
  | Trm_pat_var _ -> raise (Error (Unsupported_term "Trm_pat_var", loc))
  | Trm_pat_wild  -> raise (Error (Unsupported_term "Trm_pat_wild", loc)) *)
  end
  in

  if !Flags.verbose && !Flags.debug then Printf.printf "Exiting typecheck_trm with :\n  %s\n
  Concluding on type : %s\n " (trm_to_string result) (Ast_print.typ_to_string result.trm_typ);
  result

and typecheck_bbe (e : env) (b : bbe) : bbe = (* TODO Remove the env, and add a "bindsof" function *)
  if !Flags.verbose && !Flags.debug then Printf.printf "Entering typecheck_bbe with :\n  %s\n" (* (Ast_print.env_to_string ~style:Ast_print.style_debug e) *) (trm_to_string b);

  let loc = b.trm_loc in
  let aux_ml ?(expected_typ:typ option) ?(env : env = e) (t : trm) : trm =
    typecheck_trm ?expected_typ env t in
  let aux_bbe ?(env : env = e) (t : trm) : bbe =
    typecheck_bbe env t in
  let aux_pat ?(env : env = e) ?(expected_typ:typ option) (typ : typ) (p : pat) : pat =
    typecheck_pat ?expected_typ env p in
  let return (u : trm_desc) (binds : env) : trm =
    { trm_desc = u;
      trm_loc = loc;
      trm_typ = the_typ_bbe;
      trm_env = e;
      trm_binds = Some binds} in (* LATER: factorizable return functions? *)

  let result =
    match b.trm_desc with
      (* Handling boolean operators *)
      | Trm_not b ->
        let b = aux_bbe b in
        return (Trm_not b) (env_empty)
      | Trm_and (b1, b2) ->
        let b1 = aux_bbe b1 in
        let e = env_extend ~loc e (bindsof b1) in
        let b2 = aux_bbe ~env:e b2 in
        let b1u2 = env_merge_binds ~loc [bindsof b1; bindsof b2] in
        return (Trm_and (b1, b2)) b1u2

      | Trm_or (b1, b2) ->
        let b1 = aux_bbe b1 in
        let b2 = aux_bbe b2 in
        let b1n2 = env_intersect ~loc e (bindsof b1) (bindsof b2) in
        return (Trm_or (b1, b2)) b1n2

      | Trm_bbe_is (t, p) ->
        let t = aux_ml t in
        if !Flags.verbose then Printf.printf "Types : \n %s : %s \n %s : %s\n" (trm_to_string t)(Ast_print.typ_to_string (typeof t)) (trm_to_string p) (Ast_print.typ_to_string p.trm_typ);
        let p = aux_pat ~expected_typ:(typeof t) (t.trm_typ) p in
        unify_or_error ~loc e (typeof t) (typeof p) (Mismatch_type_is (typeof t, typeof p)); (* In theory this is not useful anymore. Remove when all of the modifications are made *)

        if debug_env then Printf.printf "Environment bound by p : %s\n" (Ast_print.env_to_string ~style:Ast_print.style_debug (bindsof p));

        return (Trm_bbe_is (t, p)) (bindsof p)

      (* simply returning aux_ml would not initialize trm_binds, meaning that the result term would be interpreted as a term, and not as a BBE by the "bindsof" function.
        Instead, we wrap with the "return" function to initialize the result bindings to [Some {}] *)
      | _ -> return (aux_ml ~expected_typ:the_typ_bool b).trm_desc (env_empty)
  in
  if !Flags.verbose && !Flags.debug then Printf.printf "Exiting typecheck_bbe with :\n  %s\n" (trm_to_string result);
  result



(* Note on constructor destruction and inversor patterns:
  - Any defined constructor would implicitly define a destructor function, of the form "Pattern__XXX".
  - For this reason, it is admitted that any constructor has its "Pattern__" counterpart, meaning that we can easily discriminate constructors from inversor functions.
  - The "Pattern__" versions have an type that is "opposite" to the original constructor. (more details in [src/ast.mli])
  pattern matching with inversion works this way :
    1. Any application is firstly considered as a constructor application. We look for a "Pattern__" version.
      -> If we find it, consider it as the current application, otherwise use the original one (happens in the case of an inversor function application)
      -> If there any sub-pattern (>= 1), then assert that the return type is an option. Otherwise (= 0), assert it is the type bool.
      -> Propagate the expected types to the corresponding sub-patterns if any.
    2. "List.map2 (fun typ p -> typecheck_pat ~expected_typ:typ p [...]) typs pats", and merge all of the result bindings, checking for conflicts.
    3. The result bindings of the pattern is the disjoint union of all of the sub-pattern bindings.
*)

(* typecheck_pat TODO change name. typecheck_pat -> typecheck_match pour le moment. Type pat -> match_pat ? *)
and typecheck_pat ?(expected_typ:typ option) (e : env) (p : pat) : pat =
  let loc = p.trm_loc in
  let _aux_ml ?(env : env = e) ?(expected_typ:typ option) (t : trm) : trm =
    typecheck_trm ?expected_typ env t in
  let aux_bbe ?(env : env = e) (b : bbe) : bbe =
    typecheck_bbe env b in
  let aux_pat ?(expected_typ:typ option) ?(env : env = e) (p : pat) : pat =
    typecheck_pat ?expected_typ env p in
  let return (typ : typ) (u : trm_desc) (binds : env) : trm =
    Option.iter (fun typ' ->
      unify_or_error ~loc e typ typ'
        (Conflict_with_context (p, typ, typ'))) expected_typ;
    { trm_desc = u;
      trm_loc = loc;
      trm_typ = if !Flags.weak_typer then the_typ_top else typ;
      trm_env = e;
      trm_binds = Some binds}
  in (* LATER: factorizable return functions? *)

  match p.trm_desc with
  | Trm_pat_wild ->
      let typ = typ_nameless () in
      return typ Trm_pat_wild env_empty
  (* TODO : add expected_typ to pat *)
  | Trm_annot (p1, sty) ->
    let sty = syntyp_internalize e sty in
    let typ = sty.syntyp_typ in
    let p1 = aux_pat ~expected_typ:typ ~env:e p1 in
    return (typeof p1) (Trm_annot (p1, sty)) (bindsof p1)

  | Trm_pat_var x ->
    let typ = typ_nameless () in
    let new_env = env_singleton x typ in
    return typ (Trm_pat_var x) new_env

  | Trm_cst cst ->
    let typ = typ_constant cst in
    return typ (Trm_cst cst) env_empty

  | Trm_tuple pl ->
    (* let tys =
      match typ_tuple_inv_opt (Repr.get_repr unfolded_exp_typ) with (* Hack : used get_repr to destruct deep in the unification tree, I don't know if this is good practice. TODO: think about this later.*)
      | Some tys -> tys
      | _ -> raise (Error (Wrong_pattern_constructor "tuple", loc))
   in
   *)
(*     let pl =
      if (List.length tys) <> (List.length pl) then
        raise (Error (Mismatch_pattern_size ((List.length tys), (List.length pl)), loc))
      else List.map2 (fun exp_ty p -> aux_pat ~expected_typ:exp_ty ~env:e p) tys pl
    in
*)
    let pl = List.map (aux_pat ~env:e) pl in
    let tys = List.map (fun p -> p.trm_typ) pl in
    let binds = List.map bindsof pl in
    (* merge all binds  *)
    return (typ_tuple tys) (Trm_tuple pl) (env_merge_binds ~loc binds)


    (* For predicate pattern and inversor pattern, we look in the table for a "Pattern__" version, that would indicate that the pattern is inversible. Meaning that there is a predefined function for destructing the construction. *)
   (* Predicate pattern *)
  | Trm_var x ->
    let pat_x = try_read_pattern ~loc e.env_var x in
    let typ = typ_nameless () in
    let pat_typ = typecheck_variable loc e pat_x in
    unify_or_error ~loc e (typ_arrow [typ] the_typ_bool) pat_typ (Unable_to_unify ((typ_arrow [typ] the_typ_bool), pat_typ));
    return typ (Trm_var x) env_empty
(* In the case where we want to handle expected types, we could destruct it and feed the sup-types
      let exp_typ = match expected_typ with Some ty -> ty | None -> assert false in (* use typ_of_some_or_nameless *)
      let (varid, tyv) = typecheck_variable loc e v in
      (* let typ = varid.varid_typ in *)
      unify_or_error ~loc e (typ_arrow [exp_typ] the_typ_bool) tyv (Unable_to_unify (exp_typ, tyv));
 *)

   (* Inversor pattern *)
  | Trm_apps (t0, ps) ->
    (* For the moment this is expected to work with "Pattern__XXX" written by hand.  *)
    assert (ps <> []);
    (* Kind of the same idea, give a "shape to fit", and do the typing. *)
    (* let typ = match expected_typ with Some ty -> ty | None -> typ_nameless() in *)
    let typ = typ_nameless () in
    let typ_args = List.map (fun _ -> typ_nameless()) ps in

    (* typ_tuple_flex <- typ_tuple_if_several/if_multiple *)
      let t0 = typecheck_trm ~expected_typ:(typ_arrow [typ] (typ_option ((typ_tuple_flex typ_args)(*  (typ_tuple typ_args) *)))) {e with env_is_in_pattern = true} t0 in
      let ps = List.map2 (fun pi typ_arg_i -> typecheck_pat e ~expected_typ:typ_arg_i pi) ps typ_args in
      return typ (Trm_apps (t0,ps)) (env_merge_binds ~loc (List.map bindsof ps))

   (* Conjunction *)
  | Trm_and (p1, p2) ->
    let typ = typ_of_some_or_nameless expected_typ in
    let p1 = aux_pat ~expected_typ:typ p1 in
    let e1 = env_extend ~loc e (bindsof p1) in
    let p2 = aux_pat ~expected_typ:typ ~env:e1 p2 in
    let p1u2 = env_merge_binds ~loc [bindsof p1; bindsof p2] in
    return typ (Trm_and (p1, p2)) p1u2

  | Trm_or (p1, p2) ->
    let typ = typ_of_some_or_nameless expected_typ in
    let p1 = aux_pat ~expected_typ:typ p1 in
    let p2 = aux_pat ~expected_typ:typ p2 in
    let p1n2 = env_intersect ~loc e (bindsof p1) (bindsof p2) in
    return typ (Trm_or (p1, p2)) p1n2

  | Trm_not p ->
    let p = aux_pat p in
    return (typeof p) (Trm_not p) env_empty

   (* TODO: Future improvements, no need for specific constructs *)
   (* Conjunction *)
   (* | Trm_apps (t0, ps) when is_builtin_func_and t0 ->
      begin match ps with
       | [p1;p2] ->
          let typ = typ_of_some_or_nameless expected_typ in
          let p1 = aux_pat e ~expected_typ:typ p1 in
          let e1 = env_extend e (bindsof p1) in
          let p2 = aux_pat e1 ~expected_typ:typ p2 in
          let e1u2 = env_extend e1 (bindsof p2) in
          return typ (Trm_apps (t0, [p1;p2])) e1u2
      | _ -> failwith "Pattern.and expects 2 arguments"
      end
   (* Disjunction *)
   | Trm_apps (t0, ps) when is_builtin_func_or t0 ->
      begin match ps with
       | [p1;p2] ->
          let typ = typ_of_some_or_nameless expected_typ in
          let p1 = aux_pat e ~expected_typ:typ p1 in
          let p2 = aux_pat e ~expected_typ:typ p2 in
          let e1n2 = env_inter (bindsof p1) (bindsof p2) in
          return typ (Trm_apps (t0, [p1;p2])) e1n2
      | _ -> failwith "Pattern.or expects 2 arguments"
      end *)

    | Trm_pat_when (p, b) ->
      let typ = typ_of_some_or_nameless expected_typ in
      let p = aux_pat ~expected_typ:typ p in
      let e1 = env_extend ~loc e (bindsof p) in
      let b = aux_bbe ~env:e1 b in
      let p1u2 = env_merge_binds ~loc [bindsof p; bindsof b] in
      return typ (Trm_pat_when (p, b)) p1u2

  (* | Trm_apps ({trm_desc = Trm_var v}, pl) when tconstr_inv e.env_tconstr v.varid_var -> (* Fonction d'inversion qui vérifie si v est dans l'environnement de types d'entrée. Si oui, alors c'est un constructeur de type et faire cas particulier, sinon autre cas. Ça devrait surement pouvoir être mis dans une clause "when". *)
  match tconstr_inv_bind e.env_tconstr v.varid_var with
    | None -> assert false
    | Some tconstr_desc ->  *)

  | _ -> raise (Error (Unsupported_term "\"Pattern not yet handled\"", loc))

(*Time to write a new typing judgement. For BBEs and for patterns.
Function signature :
- typecheck_bbe :  (e : env) (t : trm) : trm  *)

(*******************************************)
(*******************************************)
(** * Ordered resolution *)

(* let varid_assumptions (varid:varid) : varid list =
  match varid.varid_resolution with
  | VarResolved (_, assumptions) -> assumptions
  | _ -> [] *)

type 'a result =
  | Success of 'a
  | Fail

(* Given an unresolved varid, consider all its possible instances and filter the ones that are
  still compatible with the varid's type. *)
(* let try_resolve varid : unit =
  Counters.(compute_count_and_time counter_resolution_attempt time_resolution_attempt (fun () ->
  let insts =
    match varid.varid_resolution with
    | VarUnresolved candidates -> candidates
    | _ -> assert false in
  let ty = Repr.get_repr varid.varid_typ in
  let is_instantiable (i : instance) : bool =
    incr Counters.counter_candidate_instantiable ;
    let sch = instance_sch i.instance_sig in
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
      unify_with_instance  ~loc:varid.varid_loc varid.varid_env ty
        ~depth:(1 + varid.varid_depth) ~context:varid.varid_symbol i in
    Debug.log "Deduce that %s resolves to the instance declared at %s : %s."
      (Ast_print.symbol_to_string varid.varid_symbol)
      (Ast_print.print_loc i.instance_loc)
      (Ast_print.typ_to_string i.instance_sig.instance_typ) ;
    varid.varid_resolution <- VarResolved (i, assumptions)
  | _ ->
    varid.varid_resolution <- VarUnresolved { insts with candidates_and_modes_candidates = remaining_correct_instances }
  )) *)

(** Performs resolution in AST order, with handling of overloaded
    function before and after resolution inside arguments.
    Returns the number of resolved symbols at this step and
    the number of remaining unresolved symbols. *)
(* let ordered_resolve_in (t:trm) : int * int =
  let nb_resolved_at_this_traversal = ref 0 in
  let nb_unresolved = ref 0 in
  let rec process varid : unit =
    let was_resolved = varid_is_resolved varid in
    (* With this kind of syntax-guided resolution, we ignore triggers. *)
    if not was_resolved then try_resolve varid;
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
  (!nb_resolved_at_this_traversal, !nb_unresolved) *)

(** Iterate ordered resolution. Returns the number of steps until
    convergence. Returns a boolean indicating the success of
    resolution for all varids. *)
(* let ordered_resolution ?(max_traversals = max_int) (t:trm) : bool * int =
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
 *)
(* let try_resolve_with_result  varid : bool =
  match varid.varid_resolution with
  | VarUnknown | VarRegular -> assert false
  | VarResolved _ -> true
  | VarUnresolved _ ->
    Debug.print_current_top_level_resolving varid ;
    try_resolve  varid;
    match varid.varid_resolution with
    | VarUnknown | VarRegular -> assert false
    | VarResolved (_instance, assumptions) ->
      Debug.print_current_top_level_resolved varid assumptions ;
      (* FIXME: should we do add_triggers_into_typ into the type of assumptions? *)
      true
    | VarUnresolved _ -> false
 *)

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

(* let iterative_resolution (all_varids : varid list) : unit =
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
  let finally () =
    StronglyTriggered.clear () ;
    WeaklyTriggered.clear () in
  try (
    while not !success && not !stuck do
      incr Counters.counter_passes ;
      let progress = ref false in
      let attempt_resolution failure_case x =
        Debug.log "Attempting resolution of %s." (print_varid x) ;
        if try_resolve_with_result  x then (
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
 *)

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
          Printf.printf "error raised : %s\n" (Errors_aux.string_of_error_short error);
          raise (Typecheck_error serror_full)
      | Some "" ->
        Printf.printf "Error obtained: %s\n" serror ;
        Printf.printf "🟨 Skipping unspecified error in def of: %s.\n" name ;
        fallback_result
      | Some msg ->
        if exact_error_messages && msg <> serror then begin
          eprintf "%s" context_msg ;
          eprintf "Error expected: %s\n" msg ;
          eprintf "Error obtained: %s\n" serror ;
          eprintf "Full error message: %s\n%!" serror_full ;
          Debug.log "🟧 Unexpected error message."
        end else (
          Printf.printf "Error obtained: %s\n" serror ;
          Printf.printf "🟨 Skipping expected error in def of: %s.\n" name ;
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
    | Topdef_val_def { let_def_bind = Bind_var (c, _) ; let_def_body = t ; _ } when c = var "__print" ->
        begin match t.trm_desc with
        | Trm_cst (Cst_bool b) ->
            print_types := b;
            (td, env)
        | _ -> raise_typecheck_error loc "a [let __print =] must be followed by a boolean"
        end

    (* Switch to the debug mode: [let __debug = 0] or [let __debug = 1],
       or [let __debug = -1] to exit the program *)
    | Topdef_val_def { let_def_bind = Bind_var (c, _) ; let_def_body = t ; _ } when c = var "__debug" ->
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
          let r =
            Counters.(compute_and_time time_ml_constraints (fun () -> typecheck_let  ~loc:td.topdef_loc env d))
            in
          (r, !varids) in(*
        if !Flags.counters_only_for_resolutions_in_last_topdef
          then Counters.reset_stats_except_time_ml_constraints ();
        Counters.(compute_and_time time_symbol_resolution (fun () ->
          iterative_resolution varids)); *)
        Debug.log "🟩 Conclude that this definition has the type scheme %s." (sch_to_string sch) ;
(*         let sym =
          match d.let_def_bind with
          | Bind_anon -> SymbolName (var "<anonymous>")
          | Bind_var (x, _) -> SymbolName x
          | Bind_register_instance (sym, _) -> sym in
        Counters.(compute_and_time time_check_fully_typed (fun () ->
          check_fully_typed ~in_depth:true ~test_acylic:true (snd (trm_foralls_inv d.let_def_body)) sym)); *)
        ({ td with topdef_desc = Topdef_val_def d }, env')

    (* Process the declaration of an external function, e.g.
       [external ( = ) : 'a. 'a -> 'a -> bool = "%equal"] *)
    | Topdef_external tde ->
        let synsch = synsch_internalize ~loc:td.topdef_loc env tde.external_def_syntyp in
        let tde = { tde with external_def_syntyp = synsch } in
        let env = env_add_var env tde.external_def_var synsch.synsch_sch in
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
              let (env, tc) = env_add_type_declaration env td in (* Note: I just need to add the constructors basically *)
              (env, tc :: tcs)
            with Failure msg ->
              raise_typecheck_error loc msg) (env, []) tds in
        let tdt = { tdt with typ_def_typs = List.rev tcs } in
        Debug.log "🟩 Done." ;
        ({ td with topdef_desc = Topdef_typ_def tdt }, env)

    (* | _ -> raise (Error (Unsupported_term "Topdef_typ_def", td.topdef_loc)) *)
  )

let typecheck_program ?exact_error_messages ?(continue_on_error = false) ~style (tds: topdefs) : topdefs =
  let (env, tds) =
    List.fold_left (fun (env, tds) td ->
      let (td, env) =
        try typecheck_topdef ?exact_error_messages ~style env td with
        | Typecheck_error msg when continue_on_error ->
          prerr_endline (Printf.sprintf "🟥 Type error: %s" msg) ;
          (td, env) in
      (env, td :: tds)) (env_builtin, []) tds in
  Debug.log "++++++++++++++++++++++++++++++++++" ;
  List.rev tds

