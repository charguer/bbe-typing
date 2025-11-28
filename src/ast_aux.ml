open Printf
open Var
open Ast_fix


(* ** Environments *)

let env_empty : env = {
  env_var = Env.empty () ;
  env_tconstr = Env.empty ()
}

let env_dummy = env_empty


(*#########################################################################*)
(* ** Locations *)

let loc_none : loc = Location.none

let get_line l =
  let _, line, _ = Location.get_pos_info l.Location.loc_start in
  line


(*#########################################################################*)
(* ** Types *)

let no_mark : mark = -1

let incr_mark =
  let current_mark = ref 0 in
  fun () ->
    incr current_mark ;
    !current_mark


(*#########################################################################*)
(* ** Typing environment *)

(* TODO

  | Pstr_type of Asttypes.rec_flag * Parsetree.type_declaration list
->
  | Topdef_typ_def of Asttypes.rec_flag * tconstr_def list

type t =
and u =


  let tr_type_declaration (td : Parsetree.type_declaration) : tconstr_def =


type type_kind =
|	Ptype_variant of constructor_declaration list
|	Ptype_record of label_declaration list

type label_declaration = {
  	pld_name : string Asttypes.loc; -> nom du champ
  	pld_mutable : Asttypes.mutable_flag; -> ignorer pour l'instant
  	pld_type : core_type; -> type du champ
}

type constructor_declaration = {
  	pcd_name : string Asttypes.loc; -> C
  	pcd_args : constructor_arguments; -> supposer que c'est un "Pcstr_tuple of core_type list", et récupérer la liste des types
  	pcd_res : core_type option; -> ignorer, juste pour les gadts
}

type type_declaration = {
  	ptype_name : string Asttypes.loc;    -> t
  	ptype_params : (core_type * Asttypes.variance) list;  -> 'a  ignorer la variance
  	ptype_cstrs : (core_type * core_type * loc) list;  -> ignorer cela (soustypage)
  	ptype_kind : type_kind; -> si c'est algebraic ou record
  	ptype_loc : loc;
}

 pour traduire les cores types, il faut une map des params vers des types rigides créés pour l'occasion.

*)


(* TODO
   TConstr_def_record of (field * typ) list

type t = { x : int; y: float }
type 'a t = { x : 'a; y: 'a * float }

Typ_constr of tconstr * typs
  v : int t
  v.x  :  substitution du 'a par int

  liste de tvars qui sont des rigid type variables
  liste de types, les arguments du Typ_constr
  construit la liste associative, List.combine, pour appeler la fonction d'instantiation

  lookup in env_tconstr, étant donné le nom "t", retrouve la description (field * typ) list,
  avec laquelle on peut faire un List.assoc_opt, pour savoir que ".x" est associé à, par ex, 'a.

*)


(* TODO


  get_tconstr (e : env) (id : tconstr) : tconstr_def

  get_tconstr_algebraic (e : env) (id : tconstr) : (tconstr * typ) list

  get_tconstr_record ...

*)


let mk_overloaded_symbol (x : var) : symbol =
  SymbolName x

let overload_var (is : candidates_and_modes) : env_item =
  Env_item_overload is

let env_add_tconstr (e : env) (x : tconstr) (tcd : tconstr_desc) : env =
  { e with env_tconstr = Env.add e.env_tconstr x tcd }

let env_add_tvar (e : env) (x : tvar_rigid) (ty : typ) : env =
  env_add_tconstr e x {
    tconstr_tvars = [] ;
    tconstr_typ =
      Some {
        typ_desc = Typ_constr (x, []) ;
        typ_mark = no_mark
      } ;
    tconstr_def = Tconstr_abstract
  }

let env_add_symbol (e : env) (x : symbol) (it : env_item) : env =
  { e with env_var = Env.add e.env_var x it }

let env_add_var (e : env) (x : var) (it : env_item) : env =
  env_add_symbol e (SymbolName x) it

let env_add_var_pair (e : env) ((x,it) : var * env_item) : env =
  env_add_var e x it


(*#########################################################################*)
(* ** Terms *)

(* In terms obtained from parsing, we have everywhere:
       type_annot_typ = None
   and
       trm_typ = None
   These fields are set once-and-for-all during typechecking. *)

let varsyntyp_loc ((_, ty) : varsyntyp) =
  ty.syntyp_syntax.ptyp_loc


(* Important:

  let f = (fun (type a) (x:a) ...)
 is viewed in the ast as:
  let f = trm_forall (type a) (fun (x:a) -> ..)
*)




(* TODO

  Pexp_match of expression * case list

	Trm_match of trm * cases
  => must resolve fully trm, like in a let, with up+down
  => propagate annot expected type to cases
  =>

type case = {
  	case_pat : pat;
  	case_when : trm option;
    case_body : trm; }

and cases = case list

exemples

   match x with
   | 0 ->  0
   | 1 ->  1.2

   let y : float =
    match x with
    | 0 ->  0
    | 1 ->  1.2

   let rec f n =
     match n with
     | 0 -> 0
     | _ -> 1.0 +. f (n-1)

*)

(* TODO


|	Pexp_record of (Longident.t Asttypes.loc * expression) list * expression option

  | Trm_record of trm option * fieldtrms

type fieldtrms = (field * trm) list


  si with : type la base, puis pour chacun des champs, le typer avec ~annot le type du champ

  sinon : on cherche une définition de type TConstr_def_record dans l'env global
  tel que
  pour commencer, on fait naivement en balayant toutes les définitions de env_tconstr
  jusqu'à en trouver une qui convient, cad qui a exactement les mêmes champs (égalité entre les listes de clés).
  => rajouter plus tard la fonction de tri alphabétique des labels
*)
(*

     Pexp_field (e1, field)
 ->  Pexp_app (Pexp_var ("." ^ field)) [t1]      t1 = aux e1

    ajouter comme instance  ".x"  avec le shéma associé
    au moment de la définition du record
       type 'a t = { x : int * 'a } ici
       ".x" : 'a t -> (int * 'a)


    |	Pexp_setfield (e1, field, e2)     e1.field <- e2

    ajouter comme instance  ".<-x"  avec le shéma :
       ".x" : 'a t -> (int * 'a) -> unit

*)


(*#########################################################################*)
(* ** Operations on locations *)

let refine_loc l_out l_in =
  if Location.is_none l_in then l_out else l_in



(*#########################################################################*)
(* ** Smart constructors for types *)

let tvar_rigid : string -> tvar_rigid = tconstr

let mktyp (td : typ_desc) : typ =
   { typ_desc = td;
     typ_mark = no_mark; }

let typ_tvar (var : tvar) : typ =
  mktyp (Flexible (var, VaridSet.empty))

let typ_nameless () : typ =
  typ_tvar (no_name_tvar ())

let typ_namelesses (nb : int) : typ list =
  List.init nb (fun _i -> typ_nameless ())

let typ_constr (id : tconstr) (tys : typ list) : typ =
  mktyp (Typ_constr (id, tys))

let typ_rigid (var : tvar_rigid) : typ =
  typ_constr var []

let typ_empty () : typ =
  typ_constr (tconstr "_") []

let typ_unit () : typ =
  typ_constr (tconstr "unit") []

let typ_list (t1 : typ) : typ =
  typ_constr (tconstr "list") [t1]

let typ_int () : typ =
  typ_constr (tconstr "int") []

let typ_string () : typ =
  typ_constr (tconstr "string") []

let typ_float () : typ =
  typ_constr (tconstr "float") []

let typ_bool () : typ =
  typ_constr (tconstr "bool") []

let the_typ_bool = typ_bool ()
let the_typ_int = typ_int ()
let the_typ_float = typ_float ()
let the_typ_string = typ_string ()
let the_typ_unit = typ_unit ()

let typ_arrow (ty_args: typ list) (ty_ret: typ) : typ =
  assert (ty_args <> []) ;
  typ_constr (tconstr "->") (ty_args @ [ty_ret])

let typ_arrow_flexible (ty_args: typ list) (ty_ret: typ) : typ =
  if ty_args = [] then ty_ret
  else typ_arrow ty_args ty_ret

let typ_tuple (args: typ list) : typ =
  if args = [] then the_typ_unit
  else typ_constr (tconstr "*") args

(* FIXME: Do we still need these?
let typ_overload (ty : typ) (is : candidates_and_modes) : typ =
  ITyp_symbol (ITyp_overload (transform, ty, is))

let typ_resolving_parameters ?(transform = Transform_None) (ty : typ) (i : instance) ?modes parameters : internal_type =
  let modes =
    match modes with
    | Some None -> None
    | Some (Some (l, _) as modes) ->
      assert (List.length l = List.length parameters) ;
      modes
    | None -> Some (List.map (fun _ -> Mode_in) parameters, Mode_out) in
  ITyp_symbol (ITyp_resolving_parameters (transform, ty, i, modes, parameters))

let typ_resolved ?(transform = Transform_None) (ty : typ) (i : instance) : internal_type =
  ITyp_symbol (ITyp_resolved (transform, Resolution (ty, i, [])))
*)

let mk_sch (vs : tvar_rigid list) (ty : typ) : sch = {
  sch_tvars = vs ;
  sch_body = ty
}

let sch_of_nonpolymorphic_typ (ty : typ) =
  mk_sch [] ty

let synsch_of_nonpolymorphic_typ (ty : syntyp) = {
  synsch_syntax = ([], ty.syntyp_syntax) ;
  synsch_sch = sch_of_nonpolymorphic_typ ty.syntyp_typ
}

let instance_sch i =
  mk_sch i.instance_tvars i.instance_typ

let instance_sig_from_sch sch = {
    instance_tvars = sch.sch_tvars ;
    instance_assumptions = [] ;
    instance_typ = sch.sch_body
  }


(*#########################################################################*)
(* ** Smart constructors for varids *)

let create_varid ?(loc = loc_none) ?(env = env_dummy) ?(typ:typ option) ?(resolution = VarUnknown) ?(depth = 0) ?context symbol : varid =
  incr Counters.counter_varid ; {
    varid_unique_int = new_varid_unique_int () ;
    varid_symbol = symbol ;
    varid_loc = loc ;
    varid_resolution = resolution;
    varid_depth = depth;
    varid_typ =
      begin match typ with
      | Some ty -> ty
      | None -> typ_nameless ()
      end;
    varid_env = env ;
    varid_context = context ;
    varid_marker_strong = false ;
    varid_marker_weak = false
  }


(*#########################################################################*)
(* ** Tuples *)

(* For tuples, we want to know which arities have been found in the source AST. *)

module ISet =
  Set.Make (struct
    type t = int
    let compare = compare
  end)

let seen_tuples = ref ISet.empty

let add_tuple_arity i =
  seen_tuples := ISet.add i !seen_tuples

let all_seen_tuple_arity () =
  ISet.elements !seen_tuples


(*#########################################################################*)
(* ** Smart constructors for term desc *)

let trm_decorate (annot:annot) (t:trm) : trm =
  { t with trm_annot = annot }

(* [mktrm] is used in these constructors, although it would probably fit best in the next section. *)
let mktrm ?(loc:loc = loc_none) ?(typ:typ = typ_nameless ()) ?(annot = AnnotNone) (d : trm_desc) : trm = {
  trm_desc = d;
  trm_loc = loc;
  trm_typ = typ;
  trm_env = env_dummy;
  trm_annot = annot
}

let trm_desc_cst (c : cst) : trm_desc =
  Trm_cst c

let trm_desc_bool (b : bool) : trm_desc =
  trm_desc_cst (Cst_bool b)

let trm_desc_int (n : int) : trm_desc =
  trm_desc_cst (Cst_int n)

let trm_desc_float (f : float) : trm_desc =
  trm_desc_cst (Cst_float f)

let trm_desc_string (s : string) : trm_desc =
  trm_desc_cst (Cst_string s)

let trm_desc_unit () : trm_desc =
  trm_desc_cst (Cst_unit ())

let trm_desc_var_varid varid : trm_desc =
  Trm_var varid

let trm_desc_var_symbol ?typ ?resolution (x : symbol) : trm_desc =
  trm_desc_var_varid (create_varid ?typ ?resolution x)

let trm_desc_var ?typ ?resolution (x : var) : trm_desc =
  trm_desc_var_symbol ?typ ?resolution (SymbolName x)

let trm_desc_funs (xs : varsyntyps) (t : trm) : trm_desc =
  assert (xs <> []) ;
  Trm_funs (xs, t)

let trm_desc_if (t0 : trm) (t1 : trm) (t2 : trm) : trm_desc =
  Trm_if (t0, t1, t2)

let trm_desc_let_def (l : let_def) (t2 : trm) : trm_desc =
  Trm_let (l, t2)

let trm_desc_let (r : rec_flag) (x : varsynschopt) (t1 : trm) (t2 : trm) : trm_desc =
  trm_desc_let_def {
    let_def_rec = r ;
    let_def_bind = Bind_var x ;
    let_def_body = t1
  } t2

let trm_desc_seq (t1 : trm) (t2 : trm) : trm_desc =
  Trm_let ({
    let_def_rec = Nonrecursive ;
    let_def_bind = Bind_anon ;
    let_def_body = t1
  }, t2)

let trm_desc_apps (t0 : trm) (ts : trms) : trm_desc =
  Trm_apps (t0, ts)

let trm_desc_match (t : trm) (pts : (pat * trm) list) : trm_desc =
  Trm_match (t, pts)

(* ** Smart constructors for bbe trm descriptors*)
let trm_desc_bbeis (t : trm) (p : trm_pat) : trm_desc =
  Trm_bbeis (t, p)

(* ** Smart constructors for pattern trm descriptors*)
let trm_desc_patvar_varid varid : trm_desc =
  Trm_patvar varid

let trm_desc_patvar ?typ ?resolution (x : var) : trm_desc =
  trm_desc_patvar_varid (create_varid ?typ ?resolution (SymbolName x))

let trm_desc_patwild () : trm_desc =
  Trm_patwild

(*#########################################################################*)
(* ** Smart constructors for terms *)

let trm_unit ?loc ?typ ?annot () : trm =
  mktrm ?loc ?typ ?annot (trm_desc_unit ())

let trm_cst ?loc ?typ ?annot (c : cst) : trm =
  mktrm ?loc ?typ ?annot (trm_desc_cst c)

let trm_bool ?loc ?typ ?annot (b : bool) : trm =
  mktrm ?loc ?typ ?annot (trm_desc_bool b)

let trm_int ?loc ?typ ?annot (n : int) : trm =
  mktrm ?loc ?typ ?annot (trm_desc_int n)

let trm_float ?loc ?typ ?annot (f : float) : trm =
  mktrm ?loc ?typ ?annot (trm_desc_float f)

let trm_string ?loc ?typ ?annot (s : string) : trm =
  mktrm ?loc ?typ ?annot (trm_desc_string s)

let trm_unit ?loc ?typ ?annot () : trm =
  mktrm ?loc ?typ ?annot (trm_desc_unit ())

let trm_var ?loc ?typ ?annot ?resolution (x : var) : trm =
  mktrm ?loc ?typ ?annot (trm_desc_var ?typ ?resolution x)

let trm_var_symbol ?loc ?typ ?annot ?resolution (x : symbol) : trm =
  mktrm ?loc ?typ ?annot (trm_desc_var_symbol ?typ ?resolution x)

let trm_var_varid ?loc ?typ ?annot varid : trm =
  mktrm ?loc ?typ ?annot (trm_desc_var_varid varid)

let trm_funs ?loc ?typ ?annot (xs : varsyntyps) (t : trm) : trm =
  mktrm ?loc ?typ ?annot (trm_desc_funs xs t)

let trm_funs_if_non_empty ?loc ?typ ?annot (xs : varsyntyps) (t : trm) : trm =
  if xs = [] then t
  else trm_funs ?loc ?typ ?annot xs t

let trm_if ?loc ?typ ?annot (t0 : trm) (t1 : trm) (t2 : trm) : trm =
  mktrm ?loc ?typ ?annot (trm_desc_if t0 t1 t2)

let trm_let ?loc ?typ ?annot (r : rec_flag) (x : varsynschopt) (t1 : trm) (t2 : trm) : trm =
  mktrm ?loc ?typ ?annot (trm_desc_let r x t1 t2)

let trm_let_def ?loc ?typ ?annot (l : let_def) (t2 : trm) : trm =
  mktrm ?loc ?typ ?annot (trm_desc_let_def l t2)

let trm_seq ?loc ?typ ?annot (t1 : trm) (t2 : trm) : trm =
  mktrm ?loc ?typ ?annot (trm_desc_seq t1 t2)

let trm_apps ?loc ?typ ?annot (t0 : trm) (ts : trms) : trm =
  mktrm ?loc ?typ ?annot (trm_desc_apps t0 ts)

(*let trm_overload_new ?loc ?typ ?annot (inputs : symbol_modes) : trm =
  mktrm ?loc ?typ ?annot (Trm_overload_new inputs)*)

let trm_annot ?loc ?typ ?annot (t : trm) (aty : syntyp) : trm =
  mktrm ?loc ?typ ?annot (Trm_annot (t, aty))

let trm_forall ?loc ?typ ?annot (ty : tvar_rigid) (t : trm) : trm =
  mktrm ?loc ?typ ?annot (Trm_forall (ty, t))

let trm_match ?loc ?typ ?annot (t : trm) (pts : (pat * trm) list) : trm =
  mktrm ?loc ?typ ?annot (trm_desc_match t pts)

let trm_foralls ?loc ?typ (tys : tvar_rigid list) (t : trm) : trm =
  if tys = [] then t
  else List.fold_left (fun t ty -> trm_forall ?loc ?typ ty t) t (List.rev tys)

let trm_record_get ?loc ?typ t f =
  mktrm ?loc ?typ ~annot:AnnotRecordGet
    (trm_desc_apps (trm_var_symbol ?loc (SymbolGetField f)) [t])

let trm_record_set ?loc ?typ t1 f t2 =
  mktrm ?loc ?typ ~annot:AnnotRecordSet
    (trm_desc_apps (trm_var_symbol ?loc (SymbolSetField f)) [t1; t2])

let trm_record_make ?loc ?typ fts =
  let fs = List.sort compare (List.map fst fts) in
  mktrm ?loc ?typ ~annot:AnnotRecordMake
    (trm_desc_apps (trm_var_symbol ?loc (SymbolMakeRecord fs)) (List.map snd fts))

let trm_record_with ?loc ?typ t1 f t2 =
  mktrm ?loc ?typ ~annot:AnnotRecordWith
    (trm_desc_apps (trm_var_symbol ?loc (SymbolRecordWith f)) [t1; t2])

let trm_tuple ?loc ?typ ?annot (ts : trms) : trm =
  let i = List.length ts in
  add_tuple_arity i ;
  assert (i >= 2) ;
  mktrm ?loc ?typ ~annot:(AnnotTuple i)
    (trm_desc_apps (trm_var_symbol ?loc (SymbolTuple i)) ts)

let trm_tuple_flex ?loc ?typ ?annot (ts : trms) : trm =
  match ts with
  | [] -> trm_unit ?loc ?typ ?annot ()
  | [t] -> t
  | _ -> trm_tuple ?loc ?typ ?annot ts

let trm_desc_constr ?loc ?typ (c : constr) (ts : trms) : trm_desc =
  let c = constr_to_var c in
  match ts with
  | [] -> trm_desc_var ?typ c
  | [t] ->
    let typ_fun = Option.map (fun typ -> typ_arrow [t.trm_typ] typ) typ in
    trm_desc_apps (mktrm ?loc (trm_desc_var ?typ:typ_fun c)) [t]
  | _ ->
    let typ_fun =
      Option.map (fun typ -> typ_arrow [typ_tuple (List.map (fun t -> t.trm_typ) ts)] typ) typ in
    trm_desc_apps (mktrm ?loc (trm_desc_var ?typ:typ_fun c)) [trm_tuple_flex ?loc ts]

let trm_constr ?loc ?typ ?annot (c : constr) (ts : trms) : trm =
  mktrm ?loc ?typ ?annot (trm_desc_constr ?loc ?typ c ts)


(* ** Smart constructors for bbes *)
let trm_bbeis ?loc ?typ ?annot (t : trm) (p : trm_pat) : trm =
  mktrm ?loc ?typ ?annot (trm_desc_bbeis t p)

(* ** Smart constructors for trm_patterns *)

let trm_patvar ?loc ?typ ?annot ?resolution (x : var) : trm =
  mktrm ?loc ?typ ?annot (trm_desc_patvar ?typ ?resolution x)

let trm_patvar_varid ?loc ?typ ?annot varid : trm =
  mktrm ?loc ?typ ?annot (trm_desc_patvar_varid varid)

let trm_patwild ?loc ?typ ?annot () : trm =
  mktrm ?loc ?typ ?annot (trm_desc_patwild ())


(*#########################################################################*)
(* ** Smart constructors for patterns *)

let mkpat ?(loc = loc_none) ?typ (desc : pat_desc) : pat = {
  pat_desc = desc ;
  pat_loc = loc ;
  pat_typ =
    match typ with
    | Some ty -> ty
    | None -> typ_nameless ()
}

let pat_any ?loc ?typ () = mkpat ?loc ?typ Pat_any

let pat_var ?loc ?typ (x : var) = mkpat ?loc ?typ (Pat_var x)

let pat_alias ?loc ?typ (p : pat) (x : var) = mkpat ?loc ?typ (Pat_alias (p, x))

let pat_constant ?loc ?typ (c : cst) = mkpat ?loc ?typ (Pat_constant c)

let pat_tuple ?loc ?typ (ps : pats) =
  add_tuple_arity (List.length ps) ;
  mkpat ?loc ?typ (Pat_tuple ps)

let pat_construct ?loc ?typ (c : constr) (ps : pats) = mkpat ?loc ?typ (Pat_construct (c, ps))

let pat_constraint ?loc ?typ (p : pat) (ty : syntyp) = mkpat ?loc ?typ (Pat_constraint (p, ty))

let pat_or ?loc ?typ (p1 : pat) (p2 : pat) = mkpat ?loc ?typ (Pat_or (p1, p2))

let pat_ors ?loc ?typ (ps : pats) =
  match ps with
  | [] -> failwith "pat_ors: empty list."
  | p :: ps -> List.fold_left (pat_or ?loc ?typ) p ps


(*#########################################################################*)
(* ** Environment initialisation *)

let mk_base_constr name ?(symbol = SymbolName (Var.var name)) inputs tvars typ =
  let x = var name in
  Env_item_overload {
      candidates_and_modes_candidates = [{
        instance_value = trm_var ~typ ~resolution:VarRegular x ;
        instance_sig = {
          instance_tvars = tvars ;
          instance_assumptions = [] ;
          instance_typ = typ
        } ;
        instance_loc = loc_none ;
        instance_symbol = symbol
      }] ;
      candidates_and_modes_modes = Some (inputs, Mode_in)
  }

let env_builtin =
  let mk_special = {
    tconstr_tvars = [] ;
    tconstr_typ = None ;
    tconstr_def = Tconstr_special_nary } in
  let mk_base typ = {
    tconstr_tvars = [] ;
    tconstr_typ = Some typ ;
    tconstr_def = Tconstr_abstract } in
  let e = env_empty in
  let e = env_add_tconstr e (tconstr "*") mk_special in
  let e = env_add_tconstr e (tconstr "->") mk_special in
  let e = env_add_tconstr e (tconstr "int") (mk_base the_typ_int) in
  let e = env_add_tconstr e (tconstr "bool") (mk_base the_typ_bool) in
  let e = env_add_tconstr e (tconstr "float") (mk_base the_typ_float) in
  let e = env_add_tconstr e (tconstr "string") (mk_base the_typ_string) in
  let e = env_add_tconstr e (tconstr "unit") (mk_base the_typ_unit) in
  let e =
    let tv = tvar_rigid "'a" in
    let t = typ_rigid tv in
    env_add_tconstr e (tconstr "list") {
      tconstr_tvars = [tv] ;
      tconstr_def = Tconstr_def_sum [
        (constr "[]", typ_list t) ;
        (constr "::", typ_arrow [t] (typ_list t)) ;
      ] ;
      tconstr_typ = None
    } in
  let e =
    let tv = tvar_rigid "'a" in
    let t = typ_rigid tv in
    env_add_var e (var "[]") (mk_base_constr "[]" [] [tv] (typ_list t)) in
  let e =
    let tv = tvar_rigid "'a" in
    let t = typ_rigid tv in
    env_add_var e (var "::")
      (mk_base_constr "::" [Mode_in] [tv]
        (typ_arrow [typ_tuple [t; typ_list t]] (typ_list t))) in
  let e =
    env_add_var e (var "^")
      (mk_base_constr "^" [Mode_in; Mode_in] []
        (typ_arrow [the_typ_string; the_typ_string] the_typ_string)) in
  e

let env_builtin_with_tuples =
  List.fold_left (fun e i ->
    let tvs = List.init i (fun i -> tvar_rigid (Printf.sprintf "'a%i" i)) in
    let tys = List.map typ_rigid tvs in
    let ty = typ_arrow tys (typ_tuple tys) in
    let c = Printf.sprintf "__tuple_%i" i in
    let e =
      env_add_tconstr e (tconstr "*") {
        tconstr_tvars = tvs ;
        tconstr_def = Tconstr_def_sum [
          (constr c, ty)
        ] ;
        tconstr_typ = None
      } in
    let e =
      env_add_symbol e (SymbolTuple i)
        (mk_base_constr ~symbol:(SymbolTuple i) c (List.init i (fun _ -> Mode_in)) tvs ty) in
    e) env_builtin

let env_builtin_tuples () = env_builtin_with_tuples (all_seen_tuple_arity ())

(* ** Smart constructors *)

let env_item_var_nonpolymorphic (ty : typ) : env_item =
  Env_item_var (sch_of_nonpolymorphic_typ ty)


(*#########################################################################*)
(* ** Smart constructors for toplevels *)

let topdef_desc_val_let_def ?sch (l : let_def) : topdef_desc =
  Topdef_val_def l

let topdef_desc_val ?sch (rf : rec_flag) (b : bind) (t1 : trm) : topdef_desc =
  topdef_desc_val_let_def {
    let_def_rec = rf ;
    let_def_bind = b ;
    let_def_body = t1
  }

let topdef_desc_typ_def ?typs (rf : rec_flag) (td : Parsetree.type_declaration list) : topdef_desc =
  Topdef_typ_def {
    typ_def_rec = rf ;
    typ_def_td = td ;
    typ_def_typs =
      match typs with
      | None -> [] (* Dummy *)
      | Some typs -> typs
  }

let topdef_desc_external (x : var) (ty : synsch) (def : string list) : topdef_desc =
  Topdef_external {
    external_def_var = x ;
    external_def_syntyp = ty ;
    external_def_def = def ;
  }

let mktopdef ?(loc:loc=loc_none) ?error (td : topdef_desc) : topdef =
  { topdef_desc = td;
    topdef_loc = loc;
    topdef_expected_error = error }

let topdef_val ?loc ?error (rf : rec_flag) (b : bind) (t1 : trm) : topdef =
  mktopdef ?loc ?error (topdef_desc_val rf b t1)

let topdef_val_let_def ?loc ?error (l : let_def) : topdef =
  mktopdef ?loc ?error (topdef_desc_val_let_def l)

let topdef_typ_def ?loc ?error ?typs (rf : rec_flag) (td : Parsetree.type_declaration list) : topdef =
  mktopdef ?loc ?error (topdef_desc_typ_def ?typs rf td)

let topdef_external ?loc ?error (x : var) (ty : synsch) (def : string list) : topdef =
  mktopdef ?loc ?error (topdef_desc_external x ty def)



(*#########################################################################*)
(* ** Syntactic types *)

let mk_syntyp ?(typ = typ_nameless ()) (cty : styp) : syntyp = {
  syntyp_syntax = cty;
  syntyp_typ = typ
}

let styp_any =
  Parsetree.{
    ptyp_desc = Ptyp_any ;
    ptyp_loc = loc_none ;
    ptyp_loc_stack = [] ;
    ptyp_attributes = []
  }

let mk_syntyp_none ?typ ?(loc : loc = loc_none) () : syntyp =
  mk_syntyp ?typ { styp_any with ptyp_loc = loc }

let mk_syntyp_unit ?(loc : loc = loc_none) () : syntyp =
  mk_syntyp {
    ptyp_desc = Ptyp_constr ({txt = Lident "unit"; loc = loc_none}, []);
    ptyp_loc = loc;
    ptyp_loc_stack = [];
    ptyp_attributes = []; }

let rec typ_to_styp t =
  let make desc =
    Parsetree.{
      ptyp_desc = desc ;
      ptyp_loc = loc_none ;
      ptyp_loc_stack = [] ;
      ptyp_attributes = []
    } in
  match t.typ_desc with
  | Unified t -> typ_to_styp t
  | Typ_constr (tc, ts) when Var.print_tconstr tc = "*" ->
    make (Ptyp_tuple (List.map typ_to_styp ts))
  | Typ_constr (tc, ts) ->
    make (Ptyp_constr ({txt = Lident (Var.print_tconstr tc); loc = loc_none}, List.map typ_to_styp ts))
  | _ -> make Ptyp_any

let rec merge_styp sty1 sty2 =
  let open Parsetree in
  match sty1.ptyp_desc, sty2.ptyp_desc with
  | Ptyp_any, _ -> Some sty2
  | _, Ptyp_any -> Some sty1
  | Ptyp_constr (id1, sty1s), Ptyp_constr (id2, sty2s) when id1.txt = id2.txt ->
      let styos = List.map2 merge_styp sty1s sty2s in
      let styso =
        try Some (List.map (function Some sty -> sty | None -> raise Not_found) styos)
        with Not_found -> None in
      Option.map (fun stys ->
        { sty1 with ptyp_desc = Ptyp_constr (id1, stys) }) styso
  | _, _ -> None


(*#########################################################################*)
(* ** Accessor/Setter functions *)

let save_trm_env (t : trm) (e : env) : trm =
  { t with trm_env = e }

let get_env (t : trm) : env = t.trm_env

let tvar_rigid_of_styp (ct : styp) : tvar_rigid =
  match ct.ptyp_desc with
  | Ptyp_var s -> tvar_rigid ("'" ^ s)
  | _ -> failwith "type params must be a variable"


(*#########################################################################*)
(* ** Iterators on typ *)

let typ_iter (f : typ -> unit) (ty : typ) : unit =
  match ty.typ_desc with
  | Flexible _ -> ()
  | Unified ty1 -> f ty1
  | Typ_constr (_, tys) -> List.iter f tys

let typ_map (f : typ -> typ) (ty : typ) : typ =
  match ty.typ_desc with
  | Flexible _ -> ty
  | Unified t -> f t
  | Typ_constr (id, tys) ->
      let tys2 = List.map f tys in
      if List.for_all2 (==) tys tys2 (* Optimisation to avoid allocation. *)
        then ty
        else typ_constr id tys2

let rec typ_exists (f : typ -> bool) (ty : typ) : bool =
  match ty.typ_desc with
  | Flexible _ -> false
  | Unified ty -> typ_exists f ty
  | Typ_constr (id, tys) -> List.exists f tys

let rec replace_rigid_with v tv (ty : typ) : typ =
  let aux = replace_rigid_with v tv in
  match ty.typ_desc with
  | Typ_constr (x, tys) when x = v ->
    assert (tys = []) ;
    tv
  | _ -> typ_map aux ty

let rec typ_compare ty1 ty2 =
  match ty1.typ_desc, ty2.typ_desc with
  | Flexible (x, _), Flexible (y, _) -> compare x y
  | Unified ty1, Unified ty2 -> typ_compare ty1 ty2
  | Typ_constr (c1, tys1), Typ_constr (c2, tys2) ->
    begin match compare c1 c2 with
    | 0 -> List.compare typ_compare tys1 tys2
    | r -> r
    end
  | Flexible _, _ -> -1
  | _, Flexible _ -> 1
  | Typ_constr _, _ -> 1
  | _, Typ_constr _ -> -1


(*#########################################################################*)
(* ** Inversion functions for types *)

let typ_matrix_inv_opt (ty: typ) : typ option =
  match ty.typ_desc with
  | Typ_constr (c, [ty]) when c = tconstr "matrix" -> Some ty
  | _ -> None

let typ_arrow_inv_opt (ty: typ) : (typ list * typ) option =
  match ty.typ_desc with
  | Typ_constr (c, tys) when c = tconstr "->" -> Some (Tools.unlast tys)
  | _ -> None

(* [typ_arrow_inv ty] returns [(ty_args, ty_ret)] when
   [ty] has been built as [typ_arrow ty_args ty_ret]. *)
let typ_arrow_inv (ty: typ) : typ list * typ =
  match typ_arrow_inv_opt ty with
  | Some res -> res
  | None -> failwith "typ_arrow_inv: the argument is not an arrow type"

  (* [typ_arrow] is n-ary, in contrary to OCaml. *)
  (* Partial application is not a [typ_arrow]!
    One needs to explicitely state that something is a partial application.
    This leads to better error messages. *)

let rec trm_foralls_inv (t : trm) : tvar_rigid list * trm =
  match t.trm_desc with
  | Trm_forall (n, t1) ->
      let (vs, t2) = trm_foralls_inv t1 in
      (n :: vs, t2)
  | _ -> ([], t)

let typ_tuple_inv_opt ty : typ list option =
  match ty.typ_desc with
  | Typ_constr (c, tys) when c = tconstr "*" -> Some tys
  | _ -> None


let typ_of (t : trm) : typ = (* TODO: should we put get_repr here ? or in a wrapper for this function in typecheck.ml? *)
  t.trm_typ


(*#########################################################################*)

(** * Iterators on trm *)

let trm_iter (f : trm -> unit) (t : trm) : unit =
  match t.trm_desc with
  | Trm_patvar _
  | Trm_var _
  | Trm_patwild
  | Trm_cst _ -> ()
  | Trm_funs (_, t) -> f t
  | Trm_if (t1, t2, t3) -> List.iter f [t1; t2; t3]
  | Trm_let ({ let_def_body = t1 ; _ }, t2) -> List.iter f [t1; t2]
  | Trm_apps (t, ts) -> List.iter f (t :: ts)
  | Trm_annot (t, _) -> f t
  | Trm_forall (_, t) -> f t
  | Trm_match (t, pts) -> List.iter f (t :: List.map snd pts)
  | Trm_bbeis (t, p) -> List.iter f [t; p]

let trm_map (f : trm -> trm) (t : trm) : trm =
  let loc = t.trm_loc in
  let typ = t.trm_typ in
  let annot = t.trm_annot in
  match t.trm_desc with
  | Trm_patvar _
  | Trm_var _
  | Trm_patwild
  | Trm_cst _ -> t
  | Trm_funs (vs, t1) ->
    let t2 = f t1 in
    if t2 == t1 then t else trm_funs ~loc ~typ ~annot vs t2
  | Trm_if (t1, t2, t3) ->
    let t1' = f t1 in
    let t2' = f t2 in
    let t3' = f t3 in
    if t1' == t1 && t2' == t2 && t3' == t3 then t else trm_if ~loc ~typ ~annot t1' t2' t3'
  | Trm_let (ld, t2) ->
    let t1 = ld.let_def_body in
    let t1' = f t1 in
    let t2' = f t2 in
    if t1' == t1 && t2' == t2 then t
    else
      begin match ld.let_def_bind with
      | Bind_anon -> trm_seq ~loc ~typ ~annot t1' t2'
      | Bind_var _ | Bind_register_instance (_, _) ->
        mktrm ~loc ~typ ~annot (Trm_let ({ ld with let_def_body = t1' }, t2'))
      end
  | Trm_apps (tf, ts) ->
    let tf' = f tf in
    let ts' = List.map f ts in
    if tf' == tf && List.for_all2 (==) ts' ts then t else trm_apps ~loc ~typ ~annot tf' ts'
  | Trm_annot (t0, ty) ->
    let t1 = f t0 in
    if t1 == t0 then t else trm_annot ~loc ~typ ~annot t1 ty
  | Trm_forall (ty, t0) ->
    let t1 = f t0 in
    if t1 == t0 then t else trm_forall ~loc ~typ ~annot ty t1
  | Trm_match (t1, pts) ->
    let t2 = f t1 in
    let pts' = List.map (fun (p, t) -> (p, f t)) pts in
    if t2 == t1 && List.for_all2 (fun (_p', t') (_p, t) -> t' == t) pts' pts then t
    else trm_match ~loc ~typ ~annot t2 pts'
  | Trm_bbeis (t1, p1) ->
    let t1' = f t1 in
    let p1' = f p1 in
    if t1 == t1' && p1 == p1' then t
    else trm_bbeis ~loc ~typ ~annot t1' p1'

(** * Iterators on patterns *)

let pat_iter (f : pat -> unit) (p : pat) : unit =
  match p.pat_desc with
  | Pat_any
  | Pat_var _
  | Pat_constant _ -> ()
  | Pat_alias (p, _) -> f p
  | Pat_tuple ps -> List.iter f ps
  | Pat_construct (_, ps) -> List.iter f ps
  | Pat_constraint (p, _) -> f p
  | Pat_or (p1, p2) -> f p1 ; f p2

let pat_map (f : pat -> pat) (p : pat) : pat =
  let loc = p.pat_loc in
  let typ = p.pat_typ in
  match p.pat_desc with
  | Pat_any
  | Pat_var _
  | Pat_constant _ -> p
  | Pat_alias (p1, x) ->
    let p2 = f p1 in
    if p2 == p1 then p else pat_alias ~loc ~typ p2 x
  | Pat_tuple ps ->
    let ps' = List.map f ps in
    if List.for_all2 (==) ps' ps then p
    else pat_tuple ~loc ~typ ps'
  | Pat_construct (c, ps) ->
    let ps' = List.map f ps in
    if List.for_all2 (==) ps' ps then p
    else pat_construct ~loc ~typ c ps'
  | Pat_constraint (p1, aty) ->
    let p2 = f p1 in
    if p2 == p1 then p else pat_constraint ~loc ~typ p2 aty
  | Pat_or (p1, p2) ->
    let p1' = f p1 in
    let p2' = f p2 in
    if p1' == p1 && p2' == p2 then p
    else pat_or ~loc ~typ p1' p2'

let pat_vars p =
  let rec aux p =
    match p.pat_desc with
    | Pat_any -> []
    | Pat_var x -> [x]
    | Pat_alias (p, x) -> x :: aux p
    | Pat_constant _ -> []
    | Pat_tuple ps -> List.concat_map aux ps
    | Pat_construct (_c, ps) -> List.concat_map aux ps
    | Pat_constraint (p, _ty) -> aux p
    | Pat_or (p1, _p2) -> aux p1 (* Both sides should declare the same variables. *) in
  List.sort_uniq compare (aux p)

let rec pat_clone p =
  let loc = p.pat_loc in
  let typ = p.pat_typ in
  match p.pat_desc with
  | Pat_any -> pat_any ~loc ~typ ()
  | Pat_var x -> pat_var ~loc ~typ x
  | Pat_constant c -> pat_constant ~loc ~typ c
  | _ -> pat_map pat_clone p

(** * Iterators on program *)

let rec trm_clone t =
  let loc = t.trm_loc in
  let typ = t.trm_typ in
  match t.trm_desc with
  | Trm_var x ->
    mktrm ~loc ~typ (Trm_var { x with varid_env = x.varid_env ; varid_resolution = x.varid_resolution })
  | Trm_cst c -> trm_cst ~loc ~typ c
  | Trm_match (t, pts) ->
    trm_match ~loc ~typ (trm_clone t) (List.map (fun (p, t) -> (pat_clone p, trm_clone t)) pts)
  | _ -> trm_map trm_clone t

let rec pat_map_typ f p =
  let aux = pat_map_typ f in
  let f_syntyp sty = { sty with syntyp_typ = f sty.syntyp_typ } in
  let p = { p with pat_typ = f p.pat_typ } in
  let loc = p.pat_loc in
  let typ = p.pat_typ in
  match p.pat_desc with
  | Pat_constraint (p, sty) -> pat_constraint ~loc ~typ (aux p) (f_syntyp sty)
  | _ -> pat_map aux p

let rec trm_map_typ f t =
  let aux = trm_map_typ f in
  let f_syntyp sty = { sty with syntyp_typ = f sty.syntyp_typ } in
  let f_sch sch = { sch with sch_body = f sch.sch_body } in
  let t = { t with trm_typ = f t.trm_typ } in
  let loc = t.trm_loc in
  let typ = t.trm_typ in
  match t.trm_desc with
  | Trm_var varid -> trm_var_varid ~loc ~typ { varid with varid_typ = f varid.varid_typ }
  | Trm_funs (vs, t) -> trm_funs ~loc ~typ (List.map (fun (x, sty) -> (x, f_syntyp sty)) vs) (aux t)
  | Trm_annot (t, sty) -> trm_annot ~loc ~typ (aux t) (f_syntyp sty)
  | Trm_match (t, pts) ->
    trm_match ~loc ~typ (aux t) (List.map (fun (p, t) ->
      (pat_map_typ f p, aux t)) pts)
  | Trm_let ({ let_def_rec = rf ; let_def_bind = Bind_var (x, synschopt) ; let_def_body = t1 }, t2) ->
    let t1 = aux t1 in
    let t2 = aux t2 in
    let synschopt =
      Option.map (fun synsch ->
        (* FIXME: One should probably check that no nasty shadowing is taking place here. *)
        { synsch with synsch_sch = f_sch synsch.synsch_sch }) synschopt in
    trm_let ~loc ~typ rf (x, synschopt) t1 t2
  | Trm_let ({ let_def_rec = rf ; let_def_bind = Bind_register_instance (sym, inst) ; let_def_body = t1 }, t2) ->
    let t1 = aux t1 in
    let t2 = aux t2 in
    let inst =
      { inst with
          instance_assumptions =
            List.map (fun asmpt ->
              { asmpt with assumption_typ = f_syntyp asmpt.assumption_typ }) inst.instance_assumptions ;
          instance_typ = f inst.instance_typ } in
    trm_let_def ~loc ~typ {
      let_def_rec = rf ;
      let_def_bind = Bind_register_instance (sym, inst) ;
      let_def_body = t1
    } t2
  | _ -> trm_map aux t

let program_iter (f : topdef -> unit) (p : program) : unit =
  List.iter f p

let program_map (f : topdef -> topdef) (p : program) : program =
  List.map f p

let program_maps (f : topdef -> topdef list) (p : program) : program =
  List.concat_map f p

let program_clone (p : program) =
  List.map (fun td ->
    let desc =
      match td.topdef_desc with
      | Topdef_val_def vd -> Topdef_val_def { vd with let_def_body = trm_clone vd.let_def_body }
      | Topdef_typ_def td ->
        Topdef_typ_def { td with typ_def_typs =
          List.map (fun tc -> { tc with tconstr_typ = tc.tconstr_typ }) td.typ_def_typs }
      | Topdef_external te ->
        Topdef_external { te with external_def_syntyp =
          { te.external_def_syntyp with synsch_sch = te.external_def_syntyp.synsch_sch } } in
    { td with topdef_desc = desc }) p


(** * Miscellaneous *)

let mk_syntyp_tconstr ?(loc : loc = loc_none) tconstr = {
    syntyp_syntax = Parsetree.{
        ptyp_desc = Ptyp_constr ({ loc = loc_none ; txt = Lident (print_tconstr tconstr) }, []) ;
        ptyp_loc = loc ;
        ptyp_loc_stack = [] ;
        ptyp_attributes = []
      } ;
    syntyp_typ = typ_constr tconstr [] ;
  }

exception Contains_flexible of tvar

let rec contains_flexible_exn (ty : typ) : unit =
  match ty.typ_desc with
  | Flexible (ty_flex_name, _trigger) -> raise (Contains_flexible ty_flex_name)
  | _ -> typ_iter contains_flexible_exn ty


let contains_flexible (ty : typ) : bool =
  try
    contains_flexible_exn ty;
    false
  with
  | Contains_flexible _ -> true

