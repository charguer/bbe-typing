(** Implementation of a first translation pass.
    Decodes derived constructs and simplifies syntax, into a minimal IR. *)

(* Implements most of the compilation scheme detailed in the paper *)

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

let is_variable (t : trm) : bool =
  match t.trm_desc with
  | Trm_var _ -> true
  | _ -> false

(** [is_duplicated_continuation t] checks if a term is a duplicated continuation if the form [k (x1, ..., xn)] where k, x1, ..., xn are variables *)
(* Idea for inversion:
- Instead of a boolean, option on a list. Then compare the list with the arg vars of the duplicated continuation. If it is, then continue.
  -> Note, could have done the same by giving it the actual list as argument, instead of comparing outside, so no real time save...*)
let is_duplicated_continuation (t : trm) : bool =
  match t.trm_desc with
  (* In the case where the argument is a unit, we only need to verify that the function is a variable *)
  | Trm_apps ({ trm_desc = Trm_var _; _ },
             [{ trm_desc = Trm_cst (Cst_unit _); _ }]) -> true
  (* Otherwise, we check that all arguments are variables : Note YL: not sure this works? What if the continuation was done so early that some variables that were not bound before are bound now? *)
  | Trm_apps ({ trm_desc = Trm_var _; _ }, args) when (List.for_all is_variable args) -> true
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

let rec list_intersect (l1 : varid list) (l2 : varid list) : varid list =
  match l1 with
  | [] -> []
  | x1 :: l1' ->
    let rest = (list_intersect l1' l2) in
    if List.mem x1 l2 then x1 :: rest else rest

(** [bound_vars e] returns the list of variables bound by a BBE or a pattern *)
let rec bound_vars (b : trm) : varid list =
  (* TODO: handle bv(false) as the set of all variables *)
  match b.trm_desc with
  | Trm_bbe_is (t, p) -> bound_vars p

  | Trm_pat_wild ->
      []

  | Trm_pat_var x ->
      [x]

  | Trm_and (b1, b2) ->
    bound_vars b1 @ bound_vars b2

  | Trm_or (b1, b2) ->
    list_intersect (bound_vars b1) (bound_vars b2)

  | Trm_not p1 ->
      []

  | Trm_pat_when (p1, b2) ->
      bound_vars p1 @ bound_vars b2

  | Trm_apps (_, ps) ->
      List.concat (List.map bound_vars ps)

  | Trm_tuple ps ->
      List.concat (List.map bound_vars ps)

  | Trm_cst _ | Trm_var _ ->
      []

  | _ ->
      []

(** [free_vars_pat env p] computes free variables in a pattern *)
let rec free_vars_pat (env : varid list) (p : trm) : varid list =
  (* The code only differs on the case Trm_apps, where the variable bound by the precedent patterns should be removed from the free variables of the remaining ones *)
  match p.trm_desc with
  | Trm_pat_wild | Trm_pat_var _ | Trm_cst _ ->
      []

  | Trm_and (p1, p2) ->
    let bound_p1 = bound_vars p1 in
    free_vars_pat env p1 @ (free_vars_pat (env @ bound_p1) p2)

  | Trm_or (p1, p2) ->
    free_vars_pat env p1 @ free_vars_pat env p2

  | Trm_not p1 ->
    free_vars_pat env p1

  | Trm_pat_when (p1, b2) ->
    let bound_p1 = bound_vars p1 in
    free_vars_pat env p1 @ free_vars (bound_p1 @ env) b2

  | Trm_apps (f, ps) ->
    (* Each pattern binds its variables to the next patterns.
    The recursive auxiliary function computes the bound variables of each pattern and adds it to the variable environment for the next ones *)
    let rec aux var_env ps =
      match ps with
      | pi :: ps' ->
        let bound_pi = bound_vars pi in
        free_vars var_env pi @ aux (var_env @ bound_pi) ps'
      | [] -> []
    in
    aux env ps

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
      (* Example of factorization:
      let env = if (binds_of ld ??x) then (x :: env) else env in
      fv_ld @ free_vars (bound @ env) t2 *)

  | Trm_apps (t0, ts) ->
      free_vars env t0 @ List.concat (List.map (free_vars env) ts)

  | Trm_annot (t1, _sty) ->
      free_vars env t1

  | Trm_forall (_ty, t1) ->
      free_vars env t1

  | Trm_match (_, t0, pts) ->
      let fv_t0 = free_vars env t0 in
      let fv_branches = List.concat (List.map (fun (p, t) ->
        let bound_p = bound_vars p in
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

  | Trm_try_with (t1, p, t2) ->
    let bound_p = bound_vars p in
    (free_vars env t1) @ (free_vars_pat env p) @ (free_vars (env @ bound_p) t2)

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
and free_vars_let_def (args : varid list) (ld : let_def) : varid list =
  let args' = if ld.let_def_rec = Recursive then
    match ld.let_def_bind with
    | Bind_anon -> args (* should be assert false? in which case can we have a recursive anonymous function.  *)
    | Bind_var (x, _) -> x :: args
  else
    args
  in
  free_vars args' ld.let_def_body

let vars_or_unit_fun (args : varid list) : (varid * syntyp) list =
	match args with
	| [] -> [(fresh_var (), mk_syntyp_unit ())]
	| _ -> List.map (fun v -> (v, mk_syntyp_none ())) args

let vars_or_unit_args (args : varid list) : trm list =
	match args with
	| [] -> [trm_unit ()]
	| _ -> List.map trm_var_varid args

let mk_duplicate ~loc (arg_vars : varid list) (cont : trm) (body : trm -> trm) : trm =
  let k = fresh_var () in
	let k_var = trm_var_varid ~loc k in
	let k_call = trm_apps ~loc k_var (vars_or_unit_args arg_vars) in
	let k_fun = trm_funs ~loc None (vars_or_unit_fun arg_vars) cont in
	trm_let ~loc Nonrecursive (k, None) k_fun (body k_call)

(* let rec factorize_fun (t : trm) : varsyntyps * trm =
  match t.trm_desc with
  | Trm_funs (args, t') ->
    let args', t'' = factorize_fun t' in
    (args@args'), t''
  | _ -> [] , t
 *)

(*
Smart constructors:
- try_next t l k
- try_exit t1 l t2 k
- raise_next l
- raise_exit l t

Representation of try-next/exit :
match t with
| Exn_Next l -> k

match t1 with
| Exn_Exit (l, t2) -> k
*)

(* Main translation function for terms *)
let rec comp_trm (t : trm) : trm =
  let aux_trm = comp_trm in
  let aux_bbe = comp_bbe in
  let loc = t.trm_loc in
  let typ = t.trm_typ in
  match t.trm_desc with
  | Trm_var x ->
    (* [[x]] ==> x *)
    trm_var ~loc ~typ x

  | Trm_cst c ->
    (* [[n]] ==> n *)
    trm_cst ~loc ~typ c

  | Trm_funs (_, args, t1) ->
    (* [[fun (x1, ..., xn) -> t1]] ==> fun (x1, ..., xn) -> [[t]]*)
    let t1' = aux_trm t1 in
    trm_funs ~loc ~typ None args t1'

  | Trm_if (l, b0, t1, t2) ->
    (* if^"L" b0 then t1 else t2 ==>
    let k() = [[t2]] in [[b0]] (try [[t1]] with | Exn_Next "L" -> k ()) (k ()) *)
    let t2' = aux_trm t2 in
    let t1' = aux_trm t1 in
    Option.fold l
    ~none:(aux_bbe b0 t1' t2')
    ~some:(fun lbl ->
            let t12' = trm_try_next t1' lbl t2' in
            aux_bbe b0 t12' t2')

    (* let k () = t2 in aux_bbe b0 (try t1' with | Exn_Next L -> k) *)

  | Trm_let (ld, t2) ->
    (* [[let x = t1 in t2]] ==> let x = [[t1]] in [[t2]]*)
    let t2' = aux_trm t2 in
    let ld' = comp_let_def ld in
    trm_let_def ~loc ~typ ld' t2'

  | Trm_apps (t0, ts) ->
    (* [[ t0 [t1; ...; tn] ]] ==> [[t0]] [ [[t1]]; ...; [[tn]] ] *)
    let t0' = aux_trm t0 in
    let ts' = List.map aux_trm ts in
    trm_apps ~loc ~typ t0' ts'

  | Trm_annot (t1, sty) ->
    (* Propagating the annotation -- Outdated *)
    let t1' = aux_trm t1 in
    trm_annot ~loc ~typ t1' sty

  | Trm_forall (n, t1) ->
    (* Propageting the forall -- Outdated *)
    let t1' = aux_trm t1 in
    trm_forall ~loc ~typ n t1'

  | Trm_match (_, _t0, _pts) ->
    (* TODO: add handling of match construct when it is fully added is a keyword of the IR *)
    trm_assert_false ~loc ~typ ()

  | Trm_try_with (_, _, _) ->
    (* We do not expect try_with at this point of the translation, it only appears later *)
    trm_assert_false ~loc ~typ ()

  | Trm_tuple ts ->
    (* C (t1, ..., tn) ==> C ([[t1]], ..., [[tn]]) (for any primitive constructor C) *)
    let ts' = List.map aux_trm ts in
    trm_tuple ~loc ~typ ts'

  (* The [not], [and] and [or] operators are all functions applied to term arguments, and are compiled as such *)
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

  | Trm_switch (l, cases) ->
    (* A switch is compiled as a sequence of boolean conditions, with labels *)
    comp_switch ~loc ~typ l cases

  | Trm_while (_, b1, t2) ->
    (* [[while b1 do t2 done]] ==> let rec f () = [[b]] ([[t2]]; f ()) (()) in f ()*)
    let loop_name = "__my_loop" in
    let loop_var = trm_var_varid ~loc loop_name in
    let t2' = aux_trm t2 in
    (* loop_call <- "__my_loop ()" *)
    let loop_call = trm_apps ~loc loop_var [trm_unit ~loc ()] in
    (* seq <- "[[t2]]; __my_loop ()"*)
    let seq = trm_seq ~loc t2' loop_call in
    let unit_syntyp = mk_syntyp_unit () in
    (* body <- [[b1]] ([[t2]]; __my_loop ()) (()) *)
    let body = aux_bbe b1 seq (trm_unit ~loc ()) in
    let unit_type ~loc =
      ptyp_constr ~loc (Located.mk ~loc (Lident "unit")) []
    in
    (* building by hand the OCaml type of [__my_loop] *)
    let unit_to_unit_type : core_type =
      let loc = Location.none in
      ptyp_arrow ~loc Nolabel (unit_type ~loc) (unit_type ~loc)
    in
    (* loop_fun <- (fun () -> [[b1]] ([[t2]]; __my_loop ()) (())) *)
    let loop_fun = trm_funs ~loc None [(fresh_var (), unit_syntyp)] body in
    trm_let ~loc Recursive (loop_name, Some (synsch_of_nonpolymorphic_typ (mk_syntyp unit_to_unit_type))) loop_fun loop_call

  | Trm_block (lbl, t) ->
    (* [[L:{t}]] ==>try [[t]] with | Exn_Exit "L" x -> x *)
    let t' = aux_trm t in
    trm_try_exit t' lbl

  | Trm_next lbl ->
    (* [[next "L"]] ==> raise (Exn_Next "L") *)
    trm_raise_next lbl

  | Trm_exit (lbl, t) ->
    (* [[exit "L" t]] ==> raise (Exn_Exit ("L", [[t]]) *)
    let t' = aux_trm t in
    trm_raise_exit lbl t'

  (* Non-term constructors should be errors *)
  | Trm_bbe_is _ ->
    failwith "comp_trm: Trm_bbe_is is not a term"

  | Trm_pat_var _ ->
    failwith "comp_trm: Trm_pat_var is not a term"

  | Trm_pat_wild ->
    failwith "comp_trm: Trm_pat_wild is not a term"

  | Trm_pat_when _ ->
    failwith "comp_trm: Trm_pat_when is not a term"

  (* Constructors not yet handled: Trm_return, Trm_continue, Trm_break *)
  | _ -> failwith "comp_trm: exception handling constructs not yet handled"

and comp_switch ~loc ~typ (l : label option) (cases : (bbe * trm) list) : trm =
  match cases with
  | [] ->
      (* switch [] ==> raise_switch_failure *)
      trm_var_varid ~loc "*"

  | (b, t) :: rest ->
      (* switch ((case b then t) :: [c2; ...; cn]) ==> [[b]] ([[t]]) ([[switch [c2; ...; cn]]]) *)
      let into_if = trm_if l b t (trm_switch l rest) in
      comp_trm into_if
      (* let t' = comp_trm into_if in
      let rest_compiled = comp_switch ~loc ~typ rest in
      comp_bbe b t' rest_compiled *)

(** [comp_bbe] is the main translation function for BBEs.
The call [comp_bbe b u u'] compiles the BBE [b] with two continuations [u] and [u'].
The generated code evaluates to either [u] or [u'], depending on the evaluation result of b *)
and comp_bbe (b : bbe) (u : trm) (u' : trm) : trm =
  let aux_trm = comp_trm in
  let aux_bbe = comp_bbe in
  let aux_pat = comp_pat in
  let loc = b.trm_loc in
  match b.trm_desc with
  | Trm_bbe_is (t1, p2) ->
    (* [[t is p]] (u) (u') ==> let y = [[t]] in [[y |> p]] (u) (u') *)
    begin match t1.trm_desc with
    | Trm_var y ->
      aux_pat y p2 u u'
    | _ ->
      let y = fresh_var () in
      let t1' = aux_trm t1 in
      let body = aux_pat y p2 u u' in
      trm_let ~loc Nonrecursive (y, None) t1' body
    end

  | Trm_not b1 ->
    (* [[not b]] (u) (u') ==> [[b]] (u') (u) *)
    aux_bbe b1 u' u

  | Trm_and (b1, b2) ->
    (* [[b1 && b2]] (u) (u') ==> let k () = u' in [[b1]] ([[b2]] (u) (k ())) (k ()) *)
    let body k =
      let inner = aux_bbe b2 u k in
      aux_bbe b1 inner k
    in
    if is_duplicated_continuation u' then
      body u'
    else
      mk_duplicate ~loc [] u' body
  | Trm_or (b1, b2) ->
    (* [[b1 || b2]] (u) (u') ==> let k (x1, ..., xn) = u in [[b1]] (k ()) ([[b2]] (k ()) (u')) *)
    let body k =
      let inner = aux_bbe b2 k u' in
      aux_bbe b1 k inner
    in
    if is_duplicated_continuation u then
      body u
    else
      let x_bar = list_intersect (list_intersect (bound_vars b1) (bound_vars b2)) (free_vars [] u) in
      mk_duplicate ~loc x_bar u body
  | _ ->
    (* Boolean term case: [[t]] (u) (u') ==> if [[t]] then u else u' *)
    let t' = aux_trm b in
    trm_if ~loc None t' u u'

(** [comp_pat] is the main translation function for patterns.
It assumes that the variable [y] is already in scope with a let-binding,
which is supposedly the case if the call was made from [comp_bbe] *)
and comp_pat (y : varid) (p : trm) (u : trm) (u' : trm) : trm =
  let aux_trm = comp_trm in
  let aux_bbe = comp_bbe in
  let aux_pat = comp_pat in
  let loc = p.trm_loc in
  match p.trm_desc with
  | Trm_pat_wild ->
    (* [[y |> _]] (u) (u') ==> u *)
    u

  | Trm_pat_var x ->
    (* [[y |> ??x]] (u) (u') ==> let x = y in u *)
    let y_var = trm_var_varid ~loc y in
    trm_let ~loc Nonrecursive (x, None) y_var u

  | Trm_and (p1, p2) ->
    (* [[y |> (p1 & p2)]] (u) (u') ==> let k () = u' in [[y |> p1]] ([[y |> p2]] (u) (k ())) (k ()) *)
    if is_duplicated_continuation u' then
      let inner = aux_pat y p2 u u' in
      aux_pat y p1 inner u'
    else
      let body k =
        let inner = aux_pat y p2 u k in
        aux_pat y p1 inner k
      in
      mk_duplicate ~loc [] u' body

  | Trm_or (p1, p2) ->
    (* [[y |> (p1 | p2)]] (u) (u') ==> let k (x1, ..., xn) = u in [[y |> p1]] (k (x1, ..., xn)) ([[y |> p2]] (k (x1, ..., xn)) (u')) *)
    if is_duplicated_continuation u then
      let inner = aux_pat y p2 u u' in
      aux_pat y p1 u inner
    else
      let body k =
        let inner = aux_pat y p2 k u' in
        aux_pat y p1 k inner
      in
      let x_bar = list_intersect (list_intersect (bound_vars p1) (bound_vars p2)) (free_vars [] u) in
      mk_duplicate ~loc x_bar u body

  | Trm_not p1 ->
    (* [[y |> (not p)]] (u) (u') ==> [[y |> p]] (u') (u) *)
    aux_pat y p1 u' u

  | Trm_pat_when (p1, b2) ->
    (* [[y |> (p when b)]] (u) (u') ==> let k () = u' in [[y |> p]] ([[b]] (u) (k ())) (k ()) *)
    if is_duplicated_continuation u' then
      let inner = aux_bbe b2 u u' in
      aux_pat y p1 inner u'
    else
      let body k =
        let inner = aux_bbe b2 u k in
        aux_pat y p1 inner k
      in
      mk_duplicate ~loc [] u' body

  | Trm_var constr_name when is_capitalized constr_name ->
    (* Constructor without arguments: [[y |> C]] (u) (u') ==> match y with C -> u | _ -> u' *)
    let y_var = trm_var_varid ~loc y in
    let success_pat = trm_var ~loc constr_name in
    let success_case = (success_pat, u) in
    let wildcard = trm_pat_wild ~loc () in
    let failure_case = (wildcard, u') in
    trm_match ~loc None y_var [success_case; failure_case]

  | Trm_var f ->
    (* Non-capitalized variable: boolean predicate *)
    (* [[y |> g]] (u) (u') ==> let x = [[g]] y in (if x then u else u') *)
    let x = fresh_var () in
    let y_var = trm_var_varid ~loc y in
    let f_term = trm_var ~loc f in
    let f' = aux_trm f_term in
    let f_applied = trm_apps ~loc f' [y_var] in
    let x_var = trm_var_varid ~loc x in
    let body = trm_if ~loc None x_var u u' in
    trm_let ~loc Nonrecursive (x, None) f_applied body

  | Trm_apps ({ trm_desc = Trm_var constr_name; _ }, ps) when is_capitalized constr_name ->
    (* Constructor with arguments: [[y |> C (p1, ..., pn)]] (u) (u') ==>
        let k () = u' in match y with
          | C (x1, ..., xn) -> [[(x1 is p1) && ... && (xn is pn)]] (u) (u')
          | _ -> u' *)
    (* (for globally fresh variables {x1, ..., xn} *)
    (* TODO: handle the duplication *)
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
    (* Function pattern: [[y |> f (p1, ..., pn)]] (u) (u') ==>
      let x = [[f]] y in [[x |> Some (p1, ..., pn)]] (u) (u') *)
    (* (for a globally fresh variable y) *)
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
    (* Tuple pattern: [[y |> (p1, ..., pn)]] (u) (u') ==>
      let (x1, ..., xn) = y in [[(x1 is p1) && ... && (xn is pn)]] (u) (u') *)
    (* (for globally fresh variables {x1, ..., xn} *)
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
    (* Constant pattern: [[y |> c]] (u) (u') ==> if (y = c) then u else u' *)
    let y_var = trm_var_varid ~loc y in
    let c_term = trm_cst ~loc c in
    let eq = trm_apps ~loc (trm_var_varid ~loc "=") [y_var; c_term] in
    trm_if ~loc None eq u u'

  | Trm_annot (p, styp) ->
    (* Propagating the annotated pattern -- Outdated *)
    let t' = aux_pat y p u u' in
    trm_annot ~loc t' styp

  | _ ->
    (* If this is not a pattern, then it is a term of the type ['a -> bool] *)
    let x = fresh_var () in
    let y_var = trm_var_varid ~loc y in
    let t' = aux_trm p in
    let t_applied = trm_apps ~loc t' [y_var] in
    let x_var = trm_var_varid ~loc x in
    let body = trm_if ~loc None x_var u u' in
    trm_let ~loc Nonrecursive (x, None) t_applied body

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