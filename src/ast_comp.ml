(** First translation pass, decodes derived constructs, and simplifies syntax *)

open Asttypes
open Parsetree
open Var
open Ast_fix
open Ast_aux
open Tools
open Ast_print
open PPrint
open Ppxlib
open Ast_builder.Default

(* Fresh variable counter *)
let fresh_counter = ref 0

let reset_fresh_counter () =
  fresh_counter := 0

let fresh_var () =
  incr fresh_counter;
  "_x" ^ string_of_int !fresh_counter

(* Helper function to create right-associative conjunction *)
let rec trm_ands ~loc (ts : trm list) : trm =
  match ts with
  | [] -> failwith "trm_ands: empty list"
  | [t] -> t
  | t :: ts' -> trm_and ~loc t (trm_ands ~loc ts')

(* Check if a term is a duplicated continuation: k () where k is a variable *)
let is_duplicated_continuation (t : trm) : bool =
  match t.trm_desc with
  | Trm_apps ({ trm_desc = Trm_var _; _ },
             [{ trm_desc = Trm_cst (Cst_unit _); _ }]) -> true
  | _ -> false

(* Helper to check if first character is uppercase *)
let is_capitalized (s : string) : bool =
  if String.length s = 0 then false
  else
    let c = String.get s 0 in
    c >= 'A' && c <= 'Z'

(** [bound_vars_varsyntyps args] returns the list of variables bound by function arguments *)
let bound_vars_varsyntyps (args : varsyntyps) : varid list =
  List.map fst args

(** [bound_vars_pat p] returns the list of variables bound by a pattern *)
let rec bound_vars_pat (p : trm) : varid list =
  match p.trm_desc with
  | Trm_pat_wild ->
      []

  | Trm_pat_var x ->
      [x]

  | Trm_and (p1, p2) | Trm_or (p1, p2) ->
      bound_vars_pat p1 @ bound_vars_pat p2

  | Trm_not p1 ->
      bound_vars_pat p1

  | Trm_pat_when (p1, _b) ->
      bound_vars_pat p1

  | Trm_apps ({ trm_desc = Trm_pat_var _; _ }, ps) ->
      List.concat (List.map bound_vars_pat ps)

  | Trm_tuple ps ->
      List.concat (List.map bound_vars_pat ps)

  | Trm_cst _ | Trm_var _ | Trm_apps _ ->
      []

  | _ ->
      []

(** [free_vars_pat env p] computes free variables in a pattern *)
let rec free_vars_pat (env : varid list) (p : trm) : varid list =
  match p.trm_desc with
  | Trm_pat_wild | Trm_pat_var _ | Trm_cst _ ->
      []

  | Trm_and (p1, p2) | Trm_or (p1, p2) ->
      free_vars_pat env p1 @ free_vars_pat env p2

  | Trm_not p1 ->
      free_vars_pat env p1

  | Trm_pat_when (p1, b) ->
      let bound_p1 = bound_vars_pat p1 in
      let env' = bound_p1 @ env in
      free_vars_pat env p1 @ free_vars env' b

  | Trm_apps (f, ps) ->
      free_vars env f @ List.concat (List.map (free_vars_pat env) ps)

  | Trm_tuple ps ->
      List.concat (List.map (free_vars_pat env) ps)

  | Trm_var _ ->
      []

  | _ ->
      []

(** [free_vars env t] computes the list of free variables in term [t] *)
and free_vars (env : varid list) (t : trm) : varid list =
  match t.trm_desc with
  | Trm_var x ->
      if List.mem x env || (is_capitalized x) || (x = "__assert_false") || (String.starts_with ~prefix:"__pattern_" x) then [] else [x]

  | Trm_cst _
  | Trm_break _
  | Trm_continue _
  | Trm_next _ -> []

  | Trm_funs (_, args, t1) ->
      let bound = bound_vars_varsyntyps args in
      free_vars (bound @ env) t1

  | Trm_if (_, b, t1, t2) ->
      free_vars env b @ free_vars env t1 @ free_vars env t2

  | Trm_let (ld, t2) ->
      let fv_ld = free_vars_let_def env ld in
      let bound = match ld.let_def_bind with
        | Bind_anon -> []
        | Bind_var (x, _) -> [x]
      in
      fv_ld @ free_vars (bound @ env) t2

  | Trm_apps (t0, ts) ->
      free_vars env t0 @ List.concat (List.map (free_vars env) ts)

  | Trm_annot (t1, _sty) ->
      free_vars env t1

  | Trm_forall (_ty, t1) ->
      free_vars env t1

  | Trm_match (_, t0, pts) ->
      let fv_t0 = free_vars env t0 in
      let fv_branches = List.concat (List.map (fun (p, t) ->
        let bound_p = bound_vars_pat p in
        let env' = bound_p @ env in
        free_vars_pat env p @ free_vars env' t
      ) pts) in
      fv_t0 @ fv_branches

  | Trm_tuple ts ->
      List.concat (List.map (free_vars env) ts)

  | Trm_not t1 ->
      free_vars env t1

  | Trm_and (t1, t2) | Trm_or (t1, t2) ->
      free_vars env t1 @ free_vars env t2

  | Trm_switch (_, cases) ->
      List.concat (List.map (fun (b, t) ->
        free_vars env b @ free_vars env t
      ) cases)

  | Trm_while (_, b, t1) ->
      free_vars env b @ free_vars env t1

  | Trm_block (_, t) -> free_vars env t
  | Trm_exit (_, t) -> free_vars env t
  | Trm_return (_, t) -> free_vars env t

  | Trm_bbe_is (t1, p) ->
      free_vars env t1 @ free_vars_pat env p

  | Trm_pat_var x ->
      if List.mem x env then [] else [x]

  | Trm_pat_wild ->
      []

  | Trm_pat_when (p, b) ->
      free_vars_pat env p @ free_vars env b

(** [free_vars_let_def env ld] computes free variables in a let definition *)
and free_vars_let_def (env : varid list) (ld : let_def) : varid list =
  let env' = if ld.let_def_rec = Recursive then
    match ld.let_def_bind with
    | Bind_anon -> env
    | Bind_var (x, _) -> x :: env
  else
    env
  in
  free_vars env' ld.let_def_body

let vars_or_unit_fun (env : varid list) : (varid * syntyp) list =
	match env with
	| [] -> [(fresh_var (), mk_syntyp_unit ())]
	| _ -> List.map (fun v -> (v, mk_syntyp_none ())) env

let vars_or_unit_args (env : varid list) : trm list =
	match env with
	| [] -> [trm_unit ()]
	| _ -> List.map trm_var_varid env

let mk_duplicate ~loc (cont : trm) (body : trm -> trm) : trm =
	(* YL LATER: buggy code. Would find any possible free variables, since given an empty context. Only current solution would be to have during translation a list of all the currently bound variables. *)
  (* let k = fresh_var () in
	let k_var = trm_var_varid ~loc k in
	let free_vars_cont = free_vars [] cont in
	let k_call = trm_apps ~loc k_var (vars_or_unit_args free_vars_cont) in
	let k_fun = trm_funs ~loc (vars_or_unit_fun free_vars_cont) cont in
	trm_let ~loc Nonrecursive (k, None) k_fun (body k_call) *)
  body cont

(* Accumulator to factorize function definition *)
(* Actually useless, since ocaml's ast is currified by default *)
(* let rec factorize_fun (t : trm) : varsyntyps * trm =
  match t.trm_desc with
  | Trm_funs (args, t') ->
    let args', t'' = factorize_fun t' in
    (args@args'), t''
  | _ -> [] , t
 *)
(* Main translation functions *)
let rec comp_trm (t : trm) : trm =
  let aux_trm = comp_trm in
  let aux_bbe = comp_bbe in
  let loc = t.trm_loc in
  let typ = t.trm_typ in
  match t.trm_desc with
  | Trm_var x ->
    trm_var ~loc ~typ x

  | Trm_cst c ->
    trm_cst ~loc ~typ c

  | Trm_funs (_, args, t1) ->
    (* let args, t1 = factorize_fun t in *)
    let t1' = aux_trm t1 in
    trm_funs ~loc ~typ None args t1'

  | Trm_if (_, b0, t1, t2) ->
    let t1' = aux_trm t1 in
    let t2' = aux_trm t2 in
    aux_bbe b0 t1' t2'

  | Trm_let (ld, t2) ->
    let t2' = aux_trm t2 in
    let ld' = comp_let_def ld in
    trm_let_def ~loc ~typ ld' t2'

  | Trm_apps (t0, ts) ->
    let t0' = aux_trm t0 in
    let ts' = List.map aux_trm ts in
    trm_apps ~loc ~typ t0' ts'

  | Trm_annot (t1, sty) ->
    let t1' = aux_trm t1 in
    trm_annot ~loc ~typ t1' sty

  | Trm_forall (n, t1) ->
    let t1' = aux_trm t1 in
    trm_forall ~loc ~typ n t1'

  | Trm_match (_, _t0, _pts) ->
    (* As specified, match should not be used at this point *)
    trm_apps ~loc ~typ (trm_var_varid ~loc "assert") [trm_bool ~loc false]

  | Trm_tuple ts ->
    let ts' = List.map aux_trm ts in
    trm_tuple ~loc ~typ ts'

  | Trm_not t1 ->
    let t1' = aux_trm t1 in
    trm_not ~loc ~typ t1'

  | Trm_and (t1, t2) ->
    let t1' = aux_trm t1 in
    let t2' = aux_trm t2 in
    trm_and ~loc ~typ t1' t2'

  | Trm_or (t1, t2) ->
    let t1' = aux_trm t1 in
    let t2' = aux_trm t2 in
    trm_or ~loc ~typ t1' t2'

  | Trm_switch (_, cases) ->
    comp_switch ~loc ~typ cases

  | Trm_while (_, b1, t2) ->
    let loop_name = "__my_loop" in
    let loop_var = trm_var_varid ~loc loop_name in
    let t2' = aux_trm t2 in
    let loop_call = trm_apps ~loc loop_var [trm_unit ~loc ()] in
    let seq = trm_seq ~loc t2' loop_call in
    let unit_syntyp = mk_syntyp_unit () in
    let body = aux_bbe b1 seq (trm_unit ~loc ()) in
    let unit_type ~loc =
      ptyp_constr ~loc (Located.mk ~loc (Lident "unit")) []
    in
    let unit_to_unit_type : core_type =
      let loc = Location.none in
      ptyp_arrow ~loc Nolabel (unit_type ~loc) (unit_type ~loc)
    in
    (* Instead of using typ to styp, just build the styp by hand *)
    let loop_fun = trm_funs ~loc None [(fresh_var (), unit_syntyp)] body in
    (* let unit_styp = unit_syntyp.syntyp_syntax in *)
    (* let let_typ = typ_to_styp (typ_arrow [the_typ_unit] the_typ_unit) in *)
    trm_let ~loc Recursive (loop_name, Some (synsch_of_nonpolymorphic_typ (mk_syntyp unit_to_unit_type))) loop_fun loop_call

  (* Non-term constructors should be errors *)
  | Trm_bbe_is _ ->
    failwith "comp_trm: Trm_bbe_is is not a term"

  | Trm_pat_var _ ->
    failwith "comp_trm: Trm_pat_var is not a term"

  | Trm_pat_wild ->
    failwith "comp_trm: Trm_pat_wild is not a term"

  | Trm_pat_when _ ->
    failwith "comp_trm: Trm_pat_when is not a term"

  | _ -> failwith "comp_trm: exception handling constructs not yet handled"

and comp_bbe (b : bbe) (u : trm) (u' : trm) : trm =
  let aux_trm = comp_trm in
  let aux_bbe = comp_bbe in
  let aux_pat = comp_pat in
  let loc = b.trm_loc in
  match b.trm_desc with
  | Trm_bbe_is (t1, p2) ->
    (match t1.trm_desc with
      | Trm_var y ->
        aux_pat y p2 u u'
      | _ ->
        let y = fresh_var () in
        let t1' = aux_trm t1 in
        let body = aux_pat y p2 u u' in
        trm_let ~loc Nonrecursive (y, None) t1' body)

  | Trm_not b1 ->
    (* [[not b]] (u) (u') ==> [[b]] (u') (u) *)
    aux_bbe b1 u' u

  | Trm_and (b1, b2) ->
    (* [[b1 && b2]] (u) (u') ==> let k () = u' in [[b1]] ([[b2]] (u) (k ())) (k ()) *)
    if is_duplicated_continuation u' then
      let inner = aux_bbe b2 u u' in
      aux_bbe b1 inner u'
    else
		  let body k =
        let inner = aux_bbe b2 u k in
        aux_bbe b1 inner k
      in
      mk_duplicate ~loc u' body
  | Trm_or (b1, b2) ->
    (* [[b1 || b2]] (u) (u') ==> let k () = u in [[b1]] (k ()) ([[b2]] (k ()) (u')) *)
    if is_duplicated_continuation u then
      let inner = aux_bbe b2 u u' in
      aux_bbe b1 u inner
    else
      let body k =
        let inner = aux_bbe b2 k u' in
        aux_bbe b1 k inner
      in
      mk_duplicate ~loc u body
  | _ ->
    (* Boolean term case: [[t]] (u) (u') ==> if [[t]] then u else u' *)
    let t' = aux_trm b in
    trm_if ~loc None t' u u'

and comp_pat (y : varid) (p : trm) (u : trm) (u' : trm) : trm =
  let aux_trm = comp_trm in
  let aux_bbe = comp_bbe in
  let aux_pat = comp_pat in
  let loc = p.trm_loc in
  match p.trm_desc with
  | Trm_pat_wild ->
    (* (y |> _ (u) (u')) ==> u *)
    u

  | Trm_pat_var x ->
    (* (y |> ??x (u) (u')) ==> let x = y in u *)
    let y_var = trm_var_varid ~loc y in
    trm_let ~loc Nonrecursive (x, None) y_var u

  | Trm_and (p1, p2) ->
    (* (y |> (p1 & p2) (u) (u')) ==> let k () = u' in (y |> [[p1]] (y |> [[p2]] (u) (k ())) (k ())) *)
    if is_duplicated_continuation u' then
      let inner = aux_pat y p2 u u' in
      aux_pat y p1 inner u'
    else
      let body k =
        let inner = aux_pat y p2 u k in
        aux_pat y p1 inner k
      in
      mk_duplicate ~loc u' body

  | Trm_or (p1, p2) ->
    (* (y |> (p1 | p2) (u) (u')) ==> let k () = u in (y |> [[p1]] (k ()) (y |> [[p2]] (k ()) (u'))) *)
    if is_duplicated_continuation u then
      let inner = aux_pat y p2 u u' in
      aux_pat y p1 u inner
    else
      let body k =
        let inner = aux_pat y p2 k u' in
        aux_pat y p1 k inner
      in
      mk_duplicate ~loc u body

  | Trm_not p1 ->
    (* (y |> (not p) (u) (u')) ==> (y |> [[p]] (u') (u)) *)
    aux_pat y p1 u' u

  | Trm_pat_when (p1, b2) ->
    (* (y |> (p when b) (u) (u')) ==> let k () = u' in (y |> [[p]] ([[b]] (u) (k ())) (k ())) *)
    if is_duplicated_continuation u' then
      let inner = aux_bbe b2 u u' in
      aux_pat y p1 inner u'
    else
      let body k =
        let inner = aux_bbe b2 u k in
        aux_pat y p1 inner k
      in
      mk_duplicate ~loc u' body

  | Trm_var constr_name when is_capitalized constr_name ->
    (* Constructor without arguments: (y |> C (u) (u')) ==> match y with C -> u | _ -> u' *)
    let y_var = trm_var_varid ~loc y in
    let success_pat = trm_var ~loc constr_name in
    let success_case = (success_pat, u) in
    let wildcard = trm_pat_wild ~loc () in
    let failure_case = (wildcard, u') in
    trm_match ~loc None y_var [success_case; failure_case]

  | Trm_var f ->
    (* Non-capitalized variable: boolean predicate *)
    (* (y |> g (u) (u')) ==> let x = [[g]] y in if x then u else u' *)
    let x = fresh_var () in
    let y_var = trm_var_varid ~loc y in
    let f_term = trm_var ~loc f in
    let f' = aux_trm f_term in
    let f_applied = trm_apps ~loc f' [y_var] in
    let x_var = trm_var_varid ~loc x in
    let body = trm_if ~loc None x_var u u' in
    trm_let ~loc Nonrecursive (x, None) f_applied body

  | Trm_apps ({ trm_desc = Trm_var constr_name; _ }, ps) when is_capitalized constr_name ->
    (* Constructor with arguments: (y |> C (p1, ..., pn) (u) (u')) ==>
        match y with C (x1, ..., xn) -> [[(x1 is p1) && ... && (xn is pn)]] (u) (u') | _ -> u' *)
    let y_var = trm_var_varid ~loc y in
    let fresh_vars = List.map (fun _ -> fresh_var ()) ps in
    let pat_vars = List.map (fun x -> trm_pat_var ~loc x) fresh_vars in
    let match_pat = trm_apps ~loc (trm_var ~loc constr_name) pat_vars in
    let is_checks = List.map2 (fun x p ->
      trm_bbe_is ~loc (trm_var_varid ~loc x) p
    ) fresh_vars ps in
    let combined = trm_ands ~loc is_checks in
    let success_body = aux_bbe combined u u' in
    let success_case = (match_pat, success_body) in
    let wildcard = trm_pat_wild ~loc () in
    let failure_case = (wildcard, u') in
    trm_match ~loc None y_var [success_case; failure_case]

  | Trm_apps (f, ps) ->
    (* Function pattern: (y |> f (p1, ..., pn) (u) (u')) ==> let x = [[f]] y in (x |> Some (p1, ..., pn) (u) (u')) *)
    let x = fresh_var () in
    let y_var = trm_var_varid ~loc y in
    let f' = aux_trm f in
    let f_applied = trm_apps ~loc f' [y_var] in
    let some_constr = "Some" in
    let ps_pattern =
      match ps with
      | [p_single] -> p_single
      | _ -> { (List.hd ps) with trm_desc = Trm_tuple ps }
    in
    let some_pattern = trm_apps ~loc (trm_var_varid ~loc some_constr) [ps_pattern] in
    let body = aux_pat x some_pattern u u' in
    trm_let ~loc Nonrecursive (x, None) f_applied body

  | Trm_tuple ps ->
    (* Tuple pattern: (y |> (p1, ..., pn) (u) (u')) ==> let (x1, ..., xn) = y in [[(x1 is p1) && ... && (xn is pn)]] (u) (u') *)
    let y_var = trm_var_varid ~loc y in
    let fresh_vars = List.map (fun _ -> fresh_var ()) ps in
    let pat_vars = List.map (fun x -> trm_pat_var ~loc x) fresh_vars in
    let tuple_pat = trm_tuple ~loc pat_vars in
    let is_checks = List.map2 (fun x p ->
      trm_bbe_is ~loc (trm_var_varid ~loc x) p
    ) fresh_vars ps in
    let combined = trm_ands ~loc is_checks in
    let body = aux_bbe combined u u' in
    trm_match ~loc None y_var [(tuple_pat, body)]

  | Trm_cst c ->
    (* Constant pattern: (y |> c (u) (u')) ==> if (y = c) then u else u' *)
    let y_var = trm_var_varid ~loc y in
    let c_term = trm_cst ~loc c in
    let eq = trm_apps ~loc (trm_var_varid ~loc "=") [y_var; c_term] in
    trm_if ~loc None eq u u'

  | Trm_annot (p, styp) ->
    let t' = aux_pat y p u u' in
    trm_annot ~loc t' styp

  | _ ->
    (* If this is not a pattern, then it is a term of the type xxx -> bool *)
    let x = fresh_var () in
    let y_var = trm_var_varid ~loc y in
    let t' = aux_trm p in
    let t_applied = trm_apps ~loc t' [y_var] in
    let x_var = trm_var_varid ~loc x in
    let body = trm_if ~loc None x_var u u' in
    trm_let ~loc Nonrecursive (x, None) t_applied body

and comp_switch ~loc ~typ (cases : (bbe * trm) list) : trm =
  match cases with
  | [] ->
      (* switch [] ==> raise_switch_failure *)
      trm_var_varid ~loc "__assert_false"

  | (b, t) :: rest ->
      (* switch (case b then t) :: case_list ==> [[b]] ([[t]]) ([[switch case_list]]) *)
      let t' = comp_trm t in
      let rest_compiled = comp_switch ~loc ~typ rest in
      comp_bbe b t' rest_compiled

and comp_let_def (ld : let_def) : let_def =
  { ld with let_def_body = comp_trm ld.let_def_body }

(* Top-level translation functions *)
let comp_topdef (td : topdef) : topdef =
  match td.topdef_desc with
  | Topdef_val_def ld ->
      reset_fresh_counter ();
      let ld' = comp_let_def ld in
      { td with topdef_desc = Topdef_val_def ld' }

  | Topdef_typ_def _ ->
      td

  | Topdef_external _ ->
      td

let comp_program (p : program) : program =
  List.map comp_topdef p