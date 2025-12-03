open Ast_fix
open Ast_aux
open Ast_print

(* * General functions *)

module SSet = Set.Make (String)
module SMap = Map.Make (String)

module SySet = Set.Make (struct type t = symbol let compare = compare end)

module LMap =
  Map.Make (struct
    type t = loc * instance_sig * symbol
    let compare (loc1, inst1, sym1) (loc2, inst2, sym2) =
      let ( --> ) a b =
        match a with
        | 0 -> b ()
        | r -> r in
      let compare_inst () =
        compare inst1.instance_tvars inst2.instance_tvars --> fun () ->
          List.compare (fun asmpt1 asmpt2 ->
            compare asmpt1.assumption_symbol asmpt2.assumption_symbol --> fun () ->
              typ_compare asmpt1.assumption_typ.syntyp_typ asmpt2.assumption_typ.syntyp_typ
            ) inst1.instance_assumptions inst2.instance_assumptions --> fun () ->
              typ_compare inst1.instance_typ inst2.instance_typ in
      compare sym1 sym2 --> fun () ->
        match Location.is_none loc1, Location.is_none loc2 with
        | false, true -> -1
        | true, false -> 1
        | true, true -> compare_inst ()
        | false, false -> compare loc1 loc2 --> compare_inst
  end)


(* Suggest a variable name from a symbol. *)
let symbol_to_var =
  let open Var in function
  | SymbolName x -> x
  | sym -> var (symbol_to_string sym)

(* Collect all variable names from a program. *)
let collect_all_var_and_symbol_names p =
  let vars = ref SSet.empty in
  let symbols = ref SySet.empty in
  let add n = vars := SSet.add n !vars in
  (* let add_symbol n = symbols := SySet.add n !symbols in
  Env.fold (env_builtin_tuples ()).env_var (fun () x _ -> add (symbol_to_string x)) () ; *)
  program_iter (fun d ->
    match d.topdef_desc with
    | Topdef_val_def { let_def_bind = b ; let_def_body = t ; _ } ->
      begin match b with
      | Bind_var (v, _) ->
        add (Var.print_var v)
      (* | Bind_register_instance (sym, _) ->
        add_symbol sym *)
      | _ -> ()
      end ;
      let rec aux t =
        match t.trm_desc with
        | Trm_let ({ let_def_bind = Bind_var (v, _) ; _ }, _) ->
          add (Var.print_var v) ;
          trm_iter aux t
        (* | Trm_let ({ let_def_bind = Bind_register_instance (sym, _) ; _ }, _) ->
          add_symbol sym ;
          trm_iter aux t *)
        (* | Trm_var { varid_symbol = SymbolName _ ; _ } -> ()
        | Trm_var { varid_symbol = sym ; _ } -> add_symbol sym *)
        | _ -> trm_iter aux t in
      aux t
    | Topdef_typ_def { typ_def_td = tds ; _ } ->
      List.iter (fun td ->
        let open Parsetree in
        add td.ptype_name.txt ;
        match td.ptype_kind with
        | Ptype_variant cs -> List.iter (fun c -> add c.pcd_name.txt) cs
        | Ptype_record rs -> List.iter (fun r -> add r.pld_name.txt) rs
        | _ -> ()) tds
    | Topdef_external { external_def_var = v ; _ } -> add (Var.print_var v)) p ;
  (!vars, !symbols)

let merge_collected_names vars symbols =
  SySet.fold (fun sym vars -> SSet.add (symbol_to_string sym) vars) symbols vars

let collect_all_names p =
  let (vars, symbols) = collect_all_var_and_symbol_names p in
  merge_collected_names vars symbols

(* Create a fresh identifier from a set of names and seed, then add this name into the set. *)
let fresh names seed =
  let ok n = not (SSet.mem n !names) in
  let n = Var.new_name_from_seed seed ok in
  names := SSet.add n !names ;
  n


(* * Instantiation *)

(* Provide a fresh name for an instance. *)
let name_of_instance already_taken n =
  (* Filtering on letters and numbers. *)
  let n =
    String.concat "" (List.map (String.make 1) (List.map (fun c ->
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
      then c else '_')
      (List.init (String.length n) (String.get n)))) in
  let n = "__instance_" ^ n in
  fresh already_taken n

(* Term corresponding to an overloaded symbol declaration. *)
let overload_new ?loc ignore_var (modes : symbol_modes) =
    (* The symbol won't be used, as each occurence will be replaced by the corresponding instance.
      We thus just replace it with a dummy term: this makes sure that any shadowed definition is
      indeed shadowed. *)
    let tr_mode = function
      | Mode_in -> trm_bool true
      | Mode_out -> trm_bool false in
    let arg =
      match modes with
      | None -> trm_unit ()
      | Some (inputs, output) ->
        trm_tuple [
          trm_tuple_flex (List.map tr_mode inputs) ;
          tr_mode output
        ] in
    trm_apps ?loc ~typ:the_typ_unit (trm_var ?loc ignore_var) [arg]

(* An term equivalent to its argument, but eta-expansing all its arguments (typically to
  avoid errors on constructors). *)
let rec term_declaration names t t_ty =
  match t.trm_desc with
  | Trm_forall (a, t') -> trm_forall ~loc:t.trm_loc ~typ:t_ty a (term_declaration names t' t_ty)
  | _ ->
    let loc = t.trm_loc in
    match typ_arrow_inv_opt t_ty with
    | None -> trm_annot ~loc ~typ:t_ty t (mk_syntyp_none ~loc ~typ:t_ty ())
    | Some (tys, _ty) ->
      let tyxs =
        List.map (fun ty ->
          let name = fresh names "x" in
          let sub_xs =
            Option.map (List.mapi (fun i ty ->
              let x = fresh names (Printf.sprintf "%s_%i" name i) in
              (Var.var x, ty))) (typ_tuple_inv_opt ty) in
          (ty, (Var.var name, mk_syntyp_none ()), sub_xs)) tys in
      let t_body =
        let t_a =
          trm_apps ~loc t (List.map (fun (ty, (x, _sty), sub) ->
            match sub with
            | None -> trm_var ~typ:ty x
            | Some subs ->
              let ty_tuple = typ_tuple (List.map snd subs) in
              trm_tuple_flex ~loc ~typ:ty_tuple (List.map (fun (x, ty) ->
                trm_var ~loc ~typ:ty x) subs)) tyxs) in
        List.fold_left (fun t (ty, (x, _sty), subs) ->
          match subs with
          | None -> t
          | Some subs ->
            let p = pat_tuple ~loc (List.map (fun (x, ty) -> pat_var ~loc x) subs) in
            trm_match ~loc (trm_var ~loc ~typ:ty x) [(p, t)]) t_a tyxs in
      trm_funs ~loc ~typ:t_ty (List.map (fun (_ty, av, _sub) -> av) tyxs) t_body

let instantiate p =
  let names = ref (collect_all_names p) in
  (** New identifiers used globally. *)
  let ignore_var = Var.var (fresh names "ignore") in
  let instances_names = ref SMap.empty in
  let name_instance ni =
    let ni = Var.print_instance_id ni in
    match SMap.find_opt ni !instances_names with
    | Some n -> n
    | None ->
      let n = name_of_instance names ni in
      instances_names := SMap.add ni n !instances_names ;
      n in
  (* The map storing all the declared instances with its associated name.
    Usually the location is enough to identify an instance, but some constructs declare more
   than one instances at the same location: we thus parameterise the map by location and the
   instance signature. *)
  let instances = ref LMap.empty in
  (* Generate a new name and term corresponding to an instance declaration, to replace it by a
    standard declaration. *)
  let create_overload_declaration ?(loc = loc_none) ?(annot = mk_syntyp_none ()) sym
      (inst : instance_sig) t : varsynschopt * trm =
    let v = symbol_to_var sym in
    let inst_name = Var.instance_id v loc in
    let n = name_instance inst_name in
    let t = trm_foralls ~loc inst.instance_tvars t in
    (* The type annotation [annot] doesn't take into account annotations and can't be used as-is. *)
    let synsch = {
      synsch_syntax = (inst.instance_tvars, { styp_any with ptyp_loc = varsyntyp_loc (v, annot) }) ;
      synsch_sch = {
        sch_tvars = inst.instance_tvars ;
        sch_body = t.trm_typ
      }
    } in
    let n = Var.var n in
    let id = (loc, { inst with instance_typ = Repr.get_repr inst.instance_typ }, sym) in
    if not (LMap.mem id !instances) then (
      (* Due to the compilation of [let[@instance]] into a usual [let] and a [let[@register]] with
        the same location, this case can happen.  In such cases, the first one is preferable as it
        refers to the original [let] definition. *)
      instances := LMap.add id n !instances
    ) ;
    ((n, Some synsch), t) in
  (* We could compile each instance into [inst.instance_value], and it would mostly work
    (assuming no side effect on the definition of instances…).
    But as we actually defined above a term for each instance, we can reuse them instead. *)
  let get_instance_term ~loc typ inst =
    let id =
      (inst.instance_loc,
       { inst.instance_sig with instance_typ = Repr.get_repr inst.instance_sig.instance_typ },
       inst.instance_symbol) in
    match LMap.find_opt id !instances with
    | Some x -> trm_var ~loc ~typ x
    | None ->
      (* This typically happens for built-in instances, whose values directly binds to OCaml's built-ins. *)
      inst.instance_value in
  (* Adding definitions from the built-in environnment. *)
  (* let built_in =
    Env.fold (env_builtin_tuples ()).env_var (fun p x i ->
      (* This [x] is supposed to refer to a definition in the standard library. *)
      let ds =
        match i with
        | Env_item_var sch -> [] (* We assume that this [x] directly refers to a standard OCaml term. *)
        | Env_item_overload candidates ->
          let insts = candidates.candidates_and_modes_candidates in
          (* I would be very surprised if there would be more than one instance, but let's produce one term for each anyway. *)
          let build inst =
            let inst = inst.instance_sig in
            let (vty, t_builtin) =
              create_overload_declaration x inst (trm_var_symbol x) in
            (* Some of these terms are constructors, so we need to introduce their arguments
              and split them if they are tuples. *)
            let t =
              let t_ty = inst.instance_typ in
              term_declaration names t_builtin t_ty in
            topdef_val Asttypes.Nonrecursive (Bind_var vty) t in
          List.map build insts in
      ds @ p) [] *)
    let built_in = [] (* Might be a source of error. I am still not sure of what this variable is for. But since we remove any occurence of overloading, we would want this to be good I guess?? *)
    in
  (* Replacing the resolved instances within the terms by their actual value. *)
  let rec replace_in_term t =
    let loc = t.trm_loc in
    let rec instantiate_varid ~loc ?typ varid =
      let typ =
        match typ with
        | None -> varid.varid_typ
        | Some ty -> ty in
      match varid.varid_resolution with
      | VarUnknown ->
        (* This shouldn't happen with the default options.
          But -keep-failing may lead here. *)
        trm_var_varid ~loc ~typ varid
      (* | VarResolved (inst, []) -> get_instance_term ~loc typ inst
      | VarResolved (inst, args) ->
        let args = List.map (instantiate_varid ~loc) args in
        let ty_inst = typ_arrow (List.map typ_of args) typ in
        trm_apps ~loc ~typ (get_instance_term ~loc ty_inst inst) args *)
      | VarRegular -> trm_var_varid ~loc ~typ varid in
    match t.trm_desc with
    | Trm_var varid -> instantiate_varid ~loc:t.trm_loc ~typ:t.trm_typ varid
    | Trm_let ({ let_def_rec = rf ; let_def_bind = Bind_register_instance (symbol, inst) ;
        let_def_body = ti }, t) ->
      let (vty, ti) = create_overload_declaration ~loc symbol inst (replace_in_term ti) in
      trm_let ~loc:t.trm_loc ~typ:t.trm_typ rf vty (replace_in_term ti) (replace_in_term t)
    | Trm_apps (t0, ts) ->
      let t0 = replace_in_term t0 in
      let ts = List.map replace_in_term ts in
      let annot =(*
        match t0.trm_desc with
        | Trm_var { varid_symbol = SymbolGetField _ ; _ } -> AnnotRecordGet
        | Trm_var { varid_symbol = SymbolSetField _ ; _ } -> AnnotRecordSet
        | Trm_var { varid_symbol = SymbolMakeRecord _ ; _ } -> AnnotRecordMake
        | Trm_var { varid_symbol = SymbolRecordWith _ ; _ } -> AnnotRecordWith
        | _ -> *) t.trm_annot in
      trm_apps ~annot ~loc:t.trm_loc ~typ:t.trm_typ t0 ts
    | _ -> trm_map replace_in_term t in
  (* Generating the instantiated program. *)
  let p =
    program_maps (fun td ->
      let loc = td.topdef_loc in
      let mval = topdef_val ~loc ?error:td.topdef_expected_error in
      match td.topdef_desc with
      | Topdef_val_def { let_def_rec = rf ; let_def_bind = Bind_register_instance (symbol, inst) ;
          let_def_body = t } ->
        let (vty, t) = create_overload_declaration ~loc symbol inst (replace_in_term t) in
        [mval rf (Bind_var vty) t]
      | Topdef_val_def { let_def_rec = rf ; let_def_bind = bind ; let_def_body = t } ->
        [mval rf bind (replace_in_term t)]
      | Topdef_typ_def { typ_def_typs = tcds ; typ_def_td = tds ; _ } ->
        td :: List.concat (List.map2 (fun tc_desc td ->
          let loc = td.Parsetree.ptype_loc in
          let id = Var.tconstr td.ptype_name.txt in
          match tc_desc.tconstr_def with
          | Tconstr_special_nary | Tconstr_abstract | Tconstr_def_alias _ -> []
          | Tconstr_def_sum cts ->
            (* Add declaration for each constructor. *)
            List.map (fun (c, ty) ->
              let x = Var.constr_to_var c in
              let sch = mk_sch tc_desc.tconstr_tvars ty in
              let inst = instance_sig_from_sch sch in
              let (vty, t) = create_overload_declaration ~loc (SymbolName x) inst (trm_var x) in
              let t = term_declaration names t ty in
              mval Asttypes.Nonrecursive (Bind_var vty) t) cts
          | Tconstr_record ftys ->
            let ty_overall =
              match tc_desc.tconstr_typ with
              | Some ty -> ty
              | None -> typ_constr id (List.map typ_rigid tc_desc.tconstr_tvars) in
            (* Record builder. *)
            let record_make =
              let fs = List.sort compare (List.map fst ftys) in
              let ty = typ_arrow (List.map snd ftys) ty_overall in
              let inst = instance_sig_from_sch (mk_sch tc_desc.tconstr_tvars ty) in
              let (vty, t) =
                let fxs =
                  List.map (fun (f, _ty) ->
                    let n = Printf.sprintf "%s_%s" (Var.print_tconstr id) (Var.print_field f) in
                    (f, Var.var (fresh names n))) ftys in
                create_overload_declaration ~loc (SymbolMakeRecord fs) inst
                  (trm_funs (List.map (fun (f, x) -> (x, mk_syntyp_none ())) fxs)
                    (trm_record_make ~loc (List.map (fun (f, x) -> (f, trm_var x)) fxs))) in
              mval Asttypes.Nonrecursive (Bind_var vty) t in
            (* Instances for each projection. *)
            let projections =
              List.map (fun (f, ty_f) ->
                let ty = typ_arrow [ty_overall] ty_f in
                let inst = instance_sig_from_sch (mk_sch tc_desc.tconstr_tvars ty) in
                let (vty, t) =
                  let x = Var.var (fresh names (Var.print_field f)) in
                  create_overload_declaration ~loc (SymbolGetField f) inst
                    (trm_funs [(x, mk_syntyp_none ())]
                      (trm_record_get (trm_var ~typ:ty_overall x) f)) in
                mval Asttypes.Nonrecursive (Bind_var vty) t) ftys in
            (* Instances for each mutation function. *)
            let mutations =
              List.map (fun (f, ty_f) ->
                let ty = typ_arrow [ty_overall; ty_f] the_typ_unit in
                let inst = instance_sig_from_sch (mk_sch tc_desc.tconstr_tvars ty) in
                let (vty, t) =
                  let x = Var.var (fresh names (Var.print_field f)) in
                  let y = Var.var (fresh names (Var.print_field f)) in
                  create_overload_declaration ~loc (SymbolSetField f) inst
                    (trm_funs [(x, mk_syntyp_none ()); (y, mk_syntyp_none ())]
                      (trm_record_set (trm_var ~typ:ty_overall x) f (trm_var ~typ:ty_f y))) in
                mval Asttypes.Nonrecursive (Bind_var vty) t) ftys in
            (* Instances for each with-projection. *)
            let with_proj =
              List.map (fun (f, ty_f) ->
                let ty = typ_arrow [ty_overall; ty_f] ty_overall in
                let inst = instance_sig_from_sch (mk_sch tc_desc.tconstr_tvars ty) in
                let (vty, t) =
                  let x = Var.var (fresh names (Var.print_field f)) in
                  let y = Var.var (fresh names (Var.print_field f)) in
                  create_overload_declaration ~loc (SymbolRecordWith f) inst
                    (trm_funs [(x, mk_syntyp_none ()); (y, mk_syntyp_none ())]
                      (trm_record_with ~typ:ty_overall
                        (trm_var ~typ:ty_overall x)
                        f (trm_var ~typ:ty_f y))) in
                mval Asttypes.Nonrecursive (Bind_var vty) t) ftys in
            record_make :: projections @ mutations @ with_proj) tcds tds)
      | _ -> [td]) p in
  (* Adding functions used in the generation. *)
  let ignore =
    let a = Var.tconstr "a" in
    let x = Var.var (fresh names "_x") in
    let ty = typ_arrow [typ_constr a []] the_typ_unit in
    let sty = mk_syntyp_tconstr a in
    let ignore_t =
      trm_forall a (trm_funs ~typ:ty [(x, sty)]
        (trm_unit ~typ:the_typ_unit ())) in
    topdef_val Asttypes.Nonrecursive (Bind_var (ignore_var, None)) ignore_t in
  (* Adding all the base tuples definitions. *)
  let tuples =
    List.map (fun i ->
      let var = Var.var (symbol_to_string (SymbolTuple i)) in
      let t =
        let vs = List.init i (Printf.sprintf "arg_%i") in
        trm_funs (List.map (fun v -> (Var.var v, mk_syntyp_none ())) vs)
          ((* We can't represent syntactic tuples in our grammar, so we use a dirty hack by
             printing-out the expected expression. *)
           trm_var (Var.var (Printf.sprintf "(%s)" (String.concat "," vs)))) in
      topdef_val Asttypes.Nonrecursive (Bind_var (var, None)) t) (all_seen_tuple_arity ()) in
  ignore :: tuples @ built_in @ p


(* * Removing of failing terms *)

let remove_failing (p : program) =
  List.filter (fun td -> td.topdef_expected_error = None) p


(* * Removing of type annotations *)

let remove_unnecessary_type_annotations_term =
  let rec aux t =
    let skip = trm_map aux in
    let typ = t.trm_typ in
    let loc = t.trm_loc in
    match t.trm_desc with
    | Trm_funs (xs, t) -> trm_funs ~loc ~typ xs (aux t)
    | Trm_let ({ let_def_rec = rf ; let_def_bind = Bind_anon ; let_def_body = t1 }, t2) ->
      trm_seq ~loc (aux t1) (aux t2)
    | Trm_let ({ let_def_rec = rf ; let_def_bind = Bind_var x ; let_def_body = t1 }, t2) ->
      trm_let ~loc rf x (skip t1) (aux t2)
    | Trm_match (t, pts) -> trm_match ~loc ~typ (skip t) (List.map (fun (p, t) -> (p, aux t)) pts)
    | Trm_annot (t, { syntyp_syntax = Parsetree.{ ptyp_desc = Ptyp_any } }) -> aux t
    | Trm_annot ({ trm_desc = Trm_annot (_, sty1) } as t, sty2)
        when sty1.syntyp_syntax.ptyp_desc = sty2.syntyp_syntax.ptyp_desc ->
      aux t
    | _ ->
      (* t.trm_typ <- None ; *)
      trm_map aux t in
  aux

let remove_unnecessary_type_annotations (p : program) =
  List.map (fun td ->
    match td.topdef_desc with
    | Topdef_val_def ld ->
      { td with topdef_desc =
        Topdef_val_def { ld with let_def_body =
          remove_unnecessary_type_annotations_term (trm_clone ld.let_def_body) } }
    | _ -> td) p

(* * Inlining *)

(* To inline, we keep the following information about each *)
type inline_info =
  | Inline_no                         (* No information on this term. *)
  | Inline_trm of symbol list * trm   (* A function to inline: here are its variables and resulting term. *)
  | Inline_cst of (symbol list * trm) * (cst -> cst)  (* The operation of this function to constants is furthermore known. *)

(* Try to recognise external definitions and their actions over constants. *)
let get_action_from_external def =
  let on_int f =
    Some (function
      | Cst_int i -> f i
      | c -> c) in
  let on_float f =
    Some (function
      | Cst_float z -> f z
      | c -> c) in
  let on_string f =
    Some (function
      | Cst_string s -> f s
      | c -> c) in
  match def with

  | ["%negint"] -> on_int (fun i -> Cst_int (Int.neg i))
  | ["%succint"] -> on_int (fun i -> Cst_int (Int.succ i))
  | ["%predint"] -> on_int (fun i -> Cst_int (Int.pred i))

  | ["%floatofint"] -> on_int (fun i -> Cst_float (Float.of_int i))
  | ["%intoffloat"] -> on_float (fun z -> Cst_int (Float.to_int z))

  | ["caml_float_of_string"] -> on_string (fun s -> Cst_float (Float.of_string s))
  | ["%negfloat"] -> on_float (fun z -> Cst_float (Float.neg z))
  | ["%absfloat"] -> on_float (fun z -> Cst_float (Float.abs z))
  | ["caml_trunc_float"; "caml_trunc"] -> on_float (fun z -> Cst_float (Float.trunc z))
  | ["caml_round_float"; "caml_round"] -> on_float (fun z -> Cst_float (Float.round z))
  | ["caml_ceil_float"; "ceil"] -> on_float (fun z -> Cst_float (Float.ceil z))
  | ["caml_floor_float"; "floor"] -> on_float (fun z -> Cst_float (Float.floor z))

  | ["%string_length"] -> on_string (fun s -> Cst_int (String.length s))

  | _ -> None

(* For each variable, we store some inline information as well as a set of dependencies:
  if this variable is shadowed, here are all the variables whose inline information must
  be reset. *)
type inline_env = (symbol, inline_info * SySet.t) Env.t

(* Adding a variable into the environment.
  This function clears the shadowed dependencies. *)
let env_add (env : inline_env) x info dependencies =
  match Env.read_option env x with
  | None ->
    let env = Env.add env x (info, SySet.empty) in
    SySet.fold (fun y env ->
      match Env.read_option env y with
      | None -> assert false
      | Some (info, deps) -> Env.add env y (info, SySet.add x deps)) dependencies env
  | Some (_, invalidated_deps) ->
    let env = Env.add env x (info, dependencies) in
    SySet.fold (fun y env ->
      match Env.read_option env y with
      | None -> assert false
      | Some (_, deps) -> Env.add env y (Inline_no, deps)) invalidated_deps env

(* Variables appearing within a pattern. *)
let variables_of_pat p =
  let r = ref SySet.empty in
  let add x = r := SySet.add x !r in
  let rec aux p =
    match p.pat_desc with
    | Pat_var x -> add (SymbolName x)
    | Pat_alias (p, x) -> add (SymbolName x) ; aux p
    | _ -> pat_iter aux p in
  aux p ;
  !r

(* Recursively calls a function on all subterms (not on just the current ones like [trm_map]),
  with an environnment passed along.
  The [remove] function is called on this environnment whenever a local identifier is found. *)
let rec trm_map_env local env f t =
  let aux env = trm_map_env local env f in
  let typ = t.trm_typ in
  let loc = t.trm_loc in
  let annot = t.trm_annot in
  match t.trm_desc with
  | Trm_funs (vs, t') ->
    let env' = List.fold_left (fun env (v, _ty) -> local env v) env vs in
    f env (trm_funs ~typ ~loc ~annot vs (f env' t'))
  | Trm_let ({ let_def_rec = rf ; let_def_bind = Bind_var (x, ty) ; let_def_body = t1 }, t2) ->
    let env' = local env x in
    let t1 = f (if rf = Asttypes.Nonrecursive then env else env') t1 in
    let t2 = f env' t2 in
    f env (trm_let ~typ ~loc ~annot rf (x, ty) t1 t2)
  | Trm_match _ -> failwith "trm_map_env (small_compile.ml) with trm \"Trm_match\" is not handled"
    (* | Trm_match (t, pts) ->
    f env (trm_match ~typ ~loc ~annot (aux env t) (List.map (fun (p, t) ->
      let xs = variables_of_pat p in
      let env = SySet.fold (fun v env -> local env v) xs env in
      (p, aux env t)) pts)) *)
  | _ -> f env (trm_map (aux env) t)

(* Called when finding a term [x args] where [x] is associated to the function [fun vs -> t]
  in the current environment. *)
let beta_reduce vs t args =
  let module M = Map.Make (struct type t = Var.var let compare = compare end) in
  assert (List.length vs = List.length args) ;
  let env : trm M.t =
    List.fold_left2 (fun env v arg -> M.add v arg env) M.empty vs args in
  let aux env t =
    match t.trm_desc with
    | Trm_var { varid_var = x ; _ } ->
      begin match M.find_opt x env with
      | None -> t
      | Some t' -> t'
      end
    | _ -> t in
  trm_map_env (fun env v -> M.remove v env) env aux t

(* Unused for the moment *)
(* Given an environment and a term, inline all its subterms that can be inlined according to
  the environment. *)
(* let inline_term env =
  let aux (env : inline_env) (t : trm) =
    match t.trm_desc with
    | Trm_apps ({ trm_desc = Trm_var { varid_var = x ; _ } ; _ }, args) ->
      let inline_trm_case (vs, t') = beta_reduce vs t' args in
      begin match Env.read_option env x with
      | None | Some (Inline_no, _) -> t
      | Some (Inline_trm (vs, t'), _) -> inline_trm_case (vs, t')
      | Some (Inline_cst (vst, f), _) ->
        match args with
        | [{ trm_desc = Trm_cst c ; _ }] -> trm_cst (f c)
        | _ -> inline_trm_case vst
      end
    | _ -> t in
  trm_map_env Env.remove env aux *)

(* Check whether a type constructor appears within a type. *)
let rec tcons_in_typ tc ty =
  let aux = tcons_in_typ tc in
  match ty.typ_desc with
  | Typ_constr (tc', _) when tc' = tc -> true
  | _ -> typ_exists aux ty

(* Remove all the occurences of a type constructor within a term. *)
(* FIXME: Does it still make sense now that we put type everywhere? *)
let remove_tcons_in_term tc t =
  let rec aux t =
    match t.trm_desc with
    | Trm_annot (t', aty) ->
      let ty = aty.syntyp_typ in
      if tcons_in_typ tc ty then t'
      else t
    | _ -> trm_map aux t in
  Some (aux t)

(* Given a let-binding (rf being the recflag and t its definition), decide whether it should
  be inlined or not (according to an ad-hoc heuristic).
  This function also returns its dependencies: it can directly be stored within the environment. *)
(* let decide_inline_info env rf t =
  let deps =
    let r = ref SySet.empty in
    let rec aux shadowed t =
      let add x = if not (SySet.mem x shadowed) then r := SySet.add x !r in
      match t.trm_desc with
      | Trm_funs (vs, t) ->
        let shadowed = List.fold_left (fun s (v, _) -> SySet.add (SymbolName v) s) shadowed vs in
        aux shadowed t
      | Trm_let ({ let_def_rec = rf ; let_def_bind = Bind_var (x, _) ; let_def_body = t1 }, t2) ->
        let shadowed' = SySet.add (SymbolName x) shadowed in
        aux (if rf = Asttypes.Nonrecursive then shadowed else shadowed') t1 ;
        aux shadowed' t2
      | Trm_match (t, pts) ->
        aux shadowed t ;
        List.iter (fun (p, t) ->
          aux (SySet.union (variables_of_pat p) shadowed) t) pts
      | Trm_var x -> add x.varid_symbol
      | _ -> trm_iter (aux shadowed) t in
    aux SySet.empty t ;
    !r in
  let rec simple t =
    match t.trm_desc with
    | Trm_var _
    | Trm_cst _ -> true
    | Trm_forall (_tc, t) -> simple t
    | Trm_funs (_, t) -> simple t
    | Trm_annot (t, _) -> simple t
    | Trm_apps (t', ts) -> t.trm_annot <> AnnotNone && simple t' && List.for_all simple ts
    | _ -> false in
  if rf = Asttypes.Recursive then (Inline_no, deps)
  else
    let rec remove_annots t' =
      match t'.trm_desc with
      | Trm_annot (t', _) -> remove_annots t'
      | _ -> t' in
    let rec aux t =
      match t.trm_desc with
      | Trm_var x ->
        let x = x.varid_symbol in
        begin match Env.read_option env x with
        | None -> assert false
        | Some ret -> ret
        end
      | Trm_funs (vs, t') ->
        let vs = List.map (fun (v, _) -> SymbolName v) vs in
        let t' = remove_annots t' in
        begin match t'.trm_desc with
        | Trm_var x ->
          let x = x.varid_symbol in
          let deps = if List.mem x vs then SySet.empty else deps in
          (Inline_trm (vs, t'), deps)
        | Trm_cst c -> (Inline_trm (vs, t'), SySet.empty)
        | Trm_apps ({ trm_desc = Trm_var x }, ts) when List.for_all simple ts ->
          let inline_trm_case deps' (vs', t') =
            (* In this case, the current is term [fun vs -> x ts], and [x] is [fun vs' -> t']. *)
            (Inline_trm (vs, beta_reduce vs' t' ts), SySet.union deps deps') in
          let x = x.varid_symbol in
          begin match Env.read_option env x with
          | None ->
            assert (List.mem x vs) ;
            (Inline_trm (vs, t'), deps)
          | Some (Inline_no, _) -> (Inline_trm (vs, t'), deps)
          | Some (Inline_trm (vs', t'), deps') -> inline_trm_case deps' (vs', t')
          | Some (Inline_cst (vts, f), deps') ->
            match vs, ts with
            | _, [{ trm_desc = Trm_cst c }] -> (Inline_trm (vs, trm_cst (f c)), SySet.empty)
            | [x1], [{ trm_desc = Trm_var { varid_symbol = x2 } }] when x1 = x2 ->
              (* The current term is just an eta-expansion for x. *)
              (Inline_cst (vts, f), deps')
            | _, _ -> inline_trm_case deps' vts
          end
        | _ ->
          if SySet.is_empty deps || simple t' then
            (Inline_trm (vs, t'), deps)
          else (Inline_no, deps)
        end
      | Trm_forall (tc, t) ->
        let (info, deps) = aux t in
        begin match info with
        | Inline_no -> (Inline_no, deps)
        | Inline_trm (vs, t) ->
          begin match remove_tcons_in_term tc t with
          | Some t -> (Inline_trm (vs, t), deps)
          | None -> (Inline_no, deps)
          end
        | Inline_cst ((vs, t), f) ->
          begin match remove_tcons_in_term tc t with
          | Some t -> (Inline_cst ((vs, t), f), deps)
          | None -> (Inline_no, deps)
          end
        end
      | Trm_annot (t, _) -> aux t
      (* TODO: Possibly other cases, typically using match. *)
      | _ -> (Inline_no, deps) in
    aux t *)

(*Unused for the moment...*)
(* let inline_simple (p : program) =
  let (vars, symbols) = collect_all_var_and_symbol_names p in
  let env = (Env.empty () : inline_env) in
  let env =
    SySet.fold (fun sym env -> env_add env sym Inline_no SySet.empty) symbols env in
  let names = ref (merge_collected_names vars symbols) in
  let env =
    Env.fold (env_builtin_tuples ()).env_var (fun env x _ ->
      env_add env x Inline_no SySet.empty) env in
  let (_env, p) =
    List.fold_left (fun (env, p) td ->
      match td.topdef_desc with
      | Topdef_val_def ({ let_def_body = t ; let_def_bind = Bind_var (x, _) ; _ } as ld) ->
          let t = inline_term env t in
          let td = { td with topdef_desc = Topdef_val_def { ld with let_def_body = t } } in
          let (info, deps) = decide_inline_info env ld.let_def_rec t in
          let env = env_add env (SymbolName x) info deps in
          (env, td :: p)
      | Topdef_val_def ({ let_def_body = t ; _ } as ld) ->
          let t = inline_term env t in
          let td = { td with topdef_desc = Topdef_val_def { ld with let_def_body = t } } in
          (env, td :: p)
      | Topdef_typ_def tyd ->
        let env =
          List.fold_left (fun env td ->
            let names =
              let open Parsetree in
              match td.ptype_kind with
              | Ptype_variant cs -> List.map (fun c -> SymbolName (Var.var c.pcd_name.txt)) cs
              | Ptype_record rs ->
                List.map (fun r -> SymbolGetField (Var.field r.pld_name.txt)) rs
              | _ -> [] in
            List.fold_left (fun env n ->
              env_add env n Inline_no SySet.empty) env names) env tyd.typ_def_td in
        (env, td :: p)
      | Topdef_external te ->
        let info =
          match get_action_from_external te.external_def_def with
          | None -> Inline_no
          | Some f ->
            let x = Var.var (fresh names "x") in
            Inline_cst (([SymbolName x], trm_apps (trm_var te.external_def_var) [trm_var x]), f) in
        let env = env_add env (SymbolName te.external_def_var) info SySet.empty in
        (env, td :: p)) (env, []) p in
  List.rev p *)

(* * Adding explicit type on pattern-matching. *)

let add_type_on_match_in_term ?(always = false) =
  let rec aux t =
    let typ = t.trm_typ in
    match t.trm_desc with
    | Trm_match (tm, pts) ->
      let tm = aux tm in
      let typm = tm.trm_typ in
      let (do_it, tm, stypm) =
        let stypm = typ_to_styp typm in
        let default = (true, tm, stypm) in
        if always then default
        else
          match tm.trm_desc with
          | Trm_annot (tm', stypm') ->
            begin match merge_styp stypm stypm'.syntyp_syntax with
            | None -> default
            | Some sty_merged -> (true, tm', sty_merged)
            end
          | _ -> default in
      if do_it then (
        let annot = mk_syntyp ~typ:typm stypm in
        trm_match ~typ (trm_annot ~typ:typm tm annot) (List.map (fun (p, t) -> (p, aux t)) pts)
      ) else trm_match ~typ tm (List.map (fun (p, t) -> (p, aux t)) pts)
    | _ -> trm_map aux t in
  aux

let add_type_on_match ?always =
  List.map (fun td ->
    match td.topdef_desc with
    | Topdef_val_def ld ->
      { td with topdef_desc =
                  Topdef_val_def { ld with let_def_body =
                    add_type_on_match_in_term ?always ld.let_def_body } }
    | _ -> td)

let unfold_ocaml_external =
  List.concat_map (fun td ->
    let loc = td.topdef_loc in
    match td.topdef_desc with
    | Topdef_external { external_def_def = "OCaml" :: l ; external_def_syntyp = sty ; external_def_var = x } ->
      let xo = Var.var (String.concat "." l) in
      let sch = sty.synsch_sch in
      let typ = sch.sch_body in
      [{ td with topdef_desc =
                  Topdef_val_def {
                    let_def_rec = Asttypes.Nonrecursive ;
                    let_def_bind = Bind_var (x, Some sty) ;
                    let_def_body = trm_foralls ~loc sch.sch_tvars (trm_var ~loc ~typ xo)
                  } }]
    | Topdef_typ_def tyd ->
      let is_from_stdlib t_decl =
        List.exists (fun a -> a.Parsetree.attr_name.txt = "ocaml") t_decl.Parsetree.ptype_attributes in
      let d = List.filter (fun d -> not (is_from_stdlib d)) tyd.typ_def_td in
      if d = [] then []
      else [{ td with topdef_desc = Topdef_typ_def { tyd with typ_def_td = d } }]
    | _ -> [td])

