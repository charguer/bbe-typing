open Printf
open Var
open Ast_fix


(* ** Environments *)

let env_empty : env = {
  env_var = Env.empty () ;
  env_label = Env.empty () ;
  env_tconstr = Env.empty () ;
  env_is_in_pattern = false ;
}

let env_dummy = env_empty

let fresh_counter = ref 0

let reset_fresh_counter () =
  fresh_counter := 0

let fresh_var () =
  incr fresh_counter;
  "_y" ^ string_of_int !fresh_counter


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


(* let mk_overloaded_symbol (x : var) : symbol =
  SymbolName x
 *)
(* let overload_var (is : candidates_and_modes) : env_item =
  Env_item_overload is
 *)
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

(* let env_add_symbol (e : env) (x : symbol) (it : env_item) : env =
  { e with env_var = Env.add e.env_var x it }
 *)
let env_add_var (e : env) (x : var) (s : sch) : env =
  { e with env_var = Env.add e.env_var x s }

let env_add_var_pair (e : env) ((x,s) : var * sch) : env =
  env_add_var e x s


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


  si with : type la base, puis pour chacun des champs, le typer avec (* ~annot *) le type du champ

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
  mktyp (Flexible var)

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

let typ_exn () : typ =
  typ_constr (tconstr "exn") []

let typ_bbe () : typ =
  typ_constr (tconstr "type_bbe") []

let typ_top () : typ =
  typ_constr (tconstr "type_top") []

let the_typ_bool = typ_bool ()
let the_typ_int = typ_int ()
let the_typ_float = typ_float ()
let the_typ_string = typ_string ()
let the_typ_unit = typ_unit ()
let the_typ_exn = typ_exn ()
let the_typ_bbe = typ_bbe ()
let the_typ_top = typ_top ()

let typ_arrow (ty_args: typ list) (ty_ret: typ) : typ =
  assert (ty_args <> []) ;
  typ_constr (tconstr "->") (ty_args @ [ty_ret])

let typ_arrow_flexible (ty_args: typ list) (ty_ret: typ) : typ =
  if ty_args = [] then ty_ret
  else typ_arrow ty_args ty_ret

let typ_tuple (args: typ list) : typ =
  if args = [] then the_typ_unit
  else typ_constr (tconstr "*") args

let typ_tuple_flex (args: typ list) : typ =
  assert (args <> []);
  match args with
  | [ty] -> ty
  | _ -> typ_constr (tconstr "*") args

(* Design choices:
  Option types are heavily used for pattern matching on inversor functions.
  As it is required to have an n-ary option return for variable binding.

  It is also very important that the option takes several possible arguments, to bind several variables at a time.
  e.g. ('a * 'b * 'c) option. With constructor [Some : 'a * 'b * 'c -> ('a * 'b * 'c) option].


  3 possible choices:
    - We represent an option as a [Tconstr_special_nary] type. building an option takes a list of types, representing the n-arity.
    - An option is wrapper around a single type. This means that n-arity is represented with a tuple type argument.

  Implementation issues:
    - Choice 1: what would be the type of the constructors ? And how would we unify them ? How do we unify None, and Some (1, 2), since there is no "a priori" knowledge on the constructor type.
    - Choice 2: This is not an issue we have, as any flexible type would be correctly bound to the according tuple type. This however asks for conversion of tuple instances when using the constructors.

    unify (Some (2, 3), Some (true, false)) would raise an error since trying to unify "tuple (int, int)" and "tuple (bool, bool)"

  *)


let typ_option (ty : typ) : typ =
  (* if args = [] then the_typ_bool (* still to be determined, whether I want an "option on unit" or a bool *)
  else  *)
  typ_constr (tconstr "option") [ty]

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

(* let instance_sch i =
  mk_sch i.instance_tvars i.instance_typ
 *)
(* let instance_sig_from_sch sch = {
    instance_tvars = sch.sch_tvars ;
    instance_assumptions = [] ;
    instance_typ = sch.sch_body
  } *)


(*#########################################################################*)
(* ** Smart constructors for varids *)

(* let create_varid ?(loc = loc_none) ?(env = env_dummy) ?(typ:typ option) ?(resolution = VarUnknown) ?(depth = 0) ?context v : varid =
  incr Counters.counter_varid ; {
    varid_unique_int = new_varid_unique_int () ;
    varid_var = v ;
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
  } *)

(* let create_varid ?(loc = loc_none) (v : var) : varid =
  incr Counters.counter_varid ; {
    varid_var = v ;
    varid_loc = loc ;
  } *)

let create_varid (v : var) : varid = v (* TODO: remove *)

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

(* let trm_decorate (annot:annot) (t:trm) : trm =
  { t with trm_annot = annot } *)

(* [mktrm] is used in these constructors, although it would probably fit best in the next section. *)
let mktrm ?(loc:loc = loc_none) ?(typ:typ = typ_nameless ()) (* ?(annot = AnnotNone) *) (d : trm_desc) : trm = {
  trm_desc = d;
  trm_loc = loc;
  trm_typ = typ;
  trm_env = env_dummy;
  trm_binds = None;
  (* trm_annot = annot *)
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

(* let trm_desc_var_symbol ?typ ?resolution (x : symbol) : trm_desc =
  trm_desc_var_varid (create_varid ?typ ?resolution x)
 *)
let trm_desc_var (x : var) : trm_desc =
  trm_desc_var_varid (create_varid x)

let trm_desc_funs (l : label option) (xs : varsyntyps) (t : trm) : trm_desc =
  assert (xs <> []) ;
  Trm_funs (l, xs, t)

let trm_desc_if (l : label option) (t0 : trm) (t1 : trm) (t2 : trm) : trm_desc =
  Trm_if (l, t0, t1, t2)

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

let trm_desc_match (l : label option) (t : trm) (pts : (pat * trm) list) : trm_desc =
  Trm_match (l, t, pts)

let trm_desc_tuple (ts : trm list) : trm_desc =
  Trm_tuple ts

let trm_desc_not (t : trm) : trm_desc =
  Trm_not t

let trm_desc_and (t1 : trm) (t2 : trm) : trm_desc =
  Trm_and (t1, t2)

let trm_desc_or (t1 : trm) (t2 : trm) : trm_desc =
  Trm_or (t1, t2)

let trm_desc_switch (l : label option) (cases : (bbe * trm) list) : trm_desc =
  Trm_switch (l, cases)

let trm_desc_while (l : label option) (b : bbe) (t : trm) : trm_desc =
  Trm_while (l, b, t)

let trm_desc_block (lbl : label) (t : trm) : trm_desc =
  Trm_block (lbl, t)

let trm_desc_exit (lbl : label) (t : trm) : trm_desc =
  Trm_exit (lbl, t)

let trm_desc_return (lbl : label) (t : trm) : trm_desc =
  Trm_return (lbl, t)

let trm_desc_break (lbl : label) : trm_desc =
  Trm_break lbl

let trm_desc_continue (lbl : label) : trm_desc =
  Trm_continue lbl

let trm_desc_next (lbl : label) : trm_desc =
  Trm_next lbl

(* let trm_desc_raise (ex : except) : trm_desc =
  Trm_raise ex
let trm_desc_try (t1 : trm) (ex : except) (t2 : trm) : trm_desc =
  Trm_try (t1, ex, t2) *)

(* ** Smart constructors for bbe trm descriptors*)
let trm_desc_bbe_is (t : trm) (p : pat) : trm_desc =
  Trm_bbe_is (t, p)

(* ** Smart constructors for pattern trm descriptors*)
let trm_desc_pat_var_varid varid : trm_desc =
  Trm_pat_var varid

let trm_desc_pat_var (x : var) : trm_desc =
  trm_desc_pat_var_varid (create_varid x)

let trm_desc_pat_wild () : trm_desc =
  Trm_pat_wild

let trm_desc_pat_when (p : pat) (b : bbe) : trm_desc =
  Trm_pat_when (p, b)

let trm_desc_assert_false () : trm_desc =
  trm_desc_var (var "__assert_false")

(*#########################################################################*)
(* ** Smart constructors for terms *)

let trm_unit ?loc ?typ (* ?annot *) () : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_unit ())

let trm_cst ?loc ?typ (* ?annot *) (c : cst) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_cst c)

let trm_bool ?loc ?typ (* ?annot *) (b : bool) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_bool b)

let trm_int ?loc ?typ (* ?annot *) (n : int) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_int n)

let trm_float ?loc ?typ (* ?annot *) (f : float) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_float f)

let trm_string ?loc ?typ (* ?annot *) (s : string) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_string s)

let trm_unit ?loc ?typ (* ?annot *) () : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_unit ())

let trm_var ?loc ?typ (* ?annot *) (x : var) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_var x)

(* let trm_var_symbol ?loc ?typ (* ?annot *) ?resolution (x : symbol) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_var_symbol ?typ ?resolution x)
 *)
let trm_var_varid ?loc ?typ (* ?annot *) varid : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_var_varid varid)

let trm_funs ?loc ?typ (l : label option) (* ?annot *) (xs : varsyntyps) (t : trm) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_funs l xs t)

let trm_funs_if_non_empty ?loc ?typ (l : label option) (* ?annot *) (xs : varsyntyps) (t : trm) : trm =
  if xs = [] then t
  else trm_funs ?loc ?typ l (* ?annot *) xs t

let trm_if ?loc ?typ (* ?annot *) (l : label option) (t0 : trm) (t1 : trm) (t2 : trm) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_if l t0 t1 t2)

let trm_let ?loc ?typ (* ?annot *) (r : rec_flag) (x : varsynschopt) (t1 : trm) (t2 : trm) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_let r x t1 t2)

let trm_let_def ?loc ?typ (* ?annot *) (l : let_def) (t2 : trm) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_let_def l t2)

let trm_seq ?loc ?typ (* ?annot *) (t1 : trm) (t2 : trm) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_seq t1 t2)

let trm_apps ?loc ?typ (* ?annot *) (t0 : trm) (ts : trms) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_apps t0 ts)

(*let trm_overload_new ?loc ?typ (* ?annot *) (inputs : symbol_modes) : trm =
  mktrm ?loc ?typ (* ?annot *) (Trm_overload_new inputs)*)

let trm_annot ?loc ?typ (* ?annot *) (t : trm) (aty : syntyp) : trm =
  mktrm ?loc ?typ (* ?annot *) (Trm_annot (t, aty))

let trm_forall ?loc ?typ (* ?annot *) (ty : tvar_rigid) (t : trm) : trm =
  mktrm ?loc ?typ (* ?annot *) (Trm_forall (ty, t))

let trm_match ?loc ?typ (* ?annot *) (l : label option) (t : trm) (pts : (pat * trm) list) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_match l t pts)

let trm_foralls ?loc ?typ (tys : tvar_rigid list) (t : trm) : trm =
  if tys = [] then t
  else List.fold_left (fun t ty -> trm_forall ?loc ?typ ty t) t (List.rev tys)

(* let trm_record_get ?loc ?typ t f =
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
    (trm_desc_apps (trm_var_symbol ?loc (SymbolRecordWith f)) [t1; t2]) *)

(* TODO: check later if trying to handle tuples. Deprecated *)
let trm_tuple ?loc ?typ (* ?annot *) (ts : trms) : trm =
  (* let i = List.length ts in
  add_tuple_arity i ;
  assert (i >= 2) ;
  mktrm ?loc ?typ ~annot:(AnnotTuple i)
    (trm_desc_apps (trm_var_symbol ?loc (SymbolTuple i)) ts) *)
  mktrm ?loc ?typ (trm_desc_tuple ts)

let trm_not ?loc ?typ (* ?annot *) (t : trm) : trm =
  mktrm ?loc ?typ (trm_desc_not t)

let trm_and ?loc ?typ (* ?annot *) (t1 : trm) (t2 : trm) : trm =
  mktrm ?loc ?typ (trm_desc_and t1 t2)

let trm_or ?loc ?typ (* ?annot *) (t1 : trm) (t2 : trm) : trm =
  mktrm ?loc ?typ (trm_desc_or t1 t2)

(* TODO: probably remove *)
let trm_tuple_flex ?loc ?typ (* ?annot *) (ts : trms) : trm =
  match ts with
  | [] -> trm_unit ?loc ?typ (* ?annot *) () (* not sure of this one *)
  | [t] -> t (* I think this was the actual code *)
  | _ -> trm_tuple ?loc ?typ (* ?annot *) ts


(** [trm_desc_constr] *)
let trm_desc_constr ?loc ?typ (c : constr) (ts : trms) : trm_desc =
  let c = constr_to_var c in
  match ts with
  | [] -> trm_desc_var c
  | [t] ->
    let typ_fun = Option.map (fun typ -> typ_arrow [t.trm_typ] typ) typ in
    trm_desc_apps (mktrm ?loc ?typ:typ_fun (trm_desc_var c)) [t]
  | _ ->
    let typ_fun =
      Option.map (fun typ -> typ_arrow [typ_tuple (List.map (fun t -> t.trm_typ) ts)] typ) typ in
    trm_desc_apps (mktrm ?loc ?typ:typ_fun (trm_desc_var c)) ts

let trm_constr ?loc ?typ (* ?annot *) (c : constr) (ts : trms) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_constr ?loc ?typ c ts)

let trm_switch ?loc ?typ (* ?annot *) (l : label option) (cases : (bbe * trm) list) : trm =
  mktrm ?loc ?typ (trm_desc_switch l cases)

let trm_while ?loc ?typ (* ?annot *) (l : label option) (b : bbe) (t : trm) : trm =
  mktrm ?loc ?typ (trm_desc_while l b t)

let trm_block ?loc ?typ (* ?annot *) (lbl : label) (t : trm) : trm =
  mktrm ?loc ?typ (trm_desc_block lbl t)

let trm_exit ?loc ?typ (* ?annot *) (lbl : label) (t : trm) : trm =
  mktrm ?loc ?typ (trm_desc_exit lbl t)

let trm_return ?loc ?typ (* ?annot *) (lbl : label) (t : trm) : trm =
  mktrm ?loc ?typ (trm_desc_return lbl t)

let trm_break ?loc ?typ (* ?annot *) (lbl : label) : trm =
  mktrm ?loc ?typ (trm_desc_break lbl)

let trm_continue ?loc ?typ (* ?annot *) (lbl : label) : trm =
  mktrm ?loc ?typ (trm_desc_continue lbl)

let trm_next ?loc ?typ (* ?annot *) (lbl : label) : trm =
  mktrm ?loc ?typ (trm_desc_next lbl)

(* let trm_raise ?loc ?typ (* ?annot *) (ex : except) : trm =
  mktrm ?loc ?typ (trm_desc_raise ex)
let trm_try ?loc ?typ (* ?annot *) (t1 : trm) (ex : except) (t2 : trm) : trm =
  mktrm ?loc ?typ (trm_desc_try t1 ex t2) *)

(* ** Smart constructors for bbes *)
let trm_bbe_is ?loc ?typ (* ?annot *) (t : trm) (p : pat) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_bbe_is t p)

(* ** Smart constructors for patterns *)

let trm_pat_var ?loc ?typ (* ?annot *) (x : var) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_pat_var x)

let trm_pat_var_varid ?loc ?typ (* ?annot *) varid : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_pat_var_varid varid)

let trm_pat_wild ?loc ?typ (* ?annot *) () : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_pat_wild ())

let trm_pat_when ?loc ?typ (* ?annot *) (p : pat) (b : bbe) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_pat_when p b)

let trm_assert_false ?loc ?typ () : trm =
  mktrm ?loc ?typ (trm_desc_assert_false ())


(* ** Smart constructors for exception handling constructs *)

let trm_exn_next (t1 : trm) : trm =
  trm_apps (trm_var "Exn_Next") [t1]

let trm_exn_exit (t1 : trm) (t2 : trm) : trm =
  trm_apps (trm_var "Exn_Exit") [t1; t2]

let trm_magic ?loc ?typ (t : trm) : trm =
  mktrm ?loc ?typ (trm_desc_apps (trm_var "Obj.magic") [t])

 let trm_try_next ?loc ?typ (t : trm) (lbl : label) (k : trm) : trm =
  mktrm ?loc ?typ (trm_desc_match None t [(trm_exn_next (trm_string lbl), k); (trm_pat_wild (), trm_assert_false ())])

let trm_try_exit ?loc ?typ (t1 : trm) (lbl : label) : trm =
  let x = fresh_var () in
  mktrm ?loc ?typ (trm_desc_match None t1 [(trm_exn_exit (trm_string lbl) (trm_pat_var x), trm_magic (trm_var x)); (trm_pat_wild (), trm_assert_false ())])

let trm_raise_next ?loc ?typ (lbl : label) : trm =
  mktrm ?loc ?typ (trm_desc_apps (trm_var "raise") [trm_exn_next (trm_string lbl)])

let trm_raise_exit ?loc ?typ (lbl : label) (t : trm) : trm =
  mktrm ?loc ?typ (trm_desc_apps (trm_var "raise") [trm_exn_exit (trm_string lbl) (trm_magic t)])

(*
Smart constructors:
- try_next t (freshvar) k
- try_exit t1 (freshvar) t2 k
- raise_next (not fresh, given.)
- raise_exit (not fresh, given.) t
- trm_case : )
Representation of try-next/exit :
match t with
| Exn_Next l -> k

match t1 with
| Exn_Exit (l, t2) -> k
*)


(*#########################################################################*)
(* ** Smart constructors for patterns *)

(* let mkpat ?(loc = loc_none) ?typ (desc : pat_desc) : pat = {
  pat_desc = desc ;
  pat_loc = loc ;
  pat_typ =
    match typ with
    | Some ty -> ty
    | None -> typ_nameless ()
} *)

(* let pat_any ?loc ?typ () = mkpat ?loc ?typ Pat_any

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
 *)

(*#########################################################################*)
(* ** Environment initialisation *)

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
  let e = env_add_tconstr e (tconstr "option") mk_special in
  let e = env_add_tconstr e (tconstr "int") (mk_base the_typ_int) in
  let e = env_add_tconstr e (tconstr "bool") (mk_base the_typ_bool) in
  let e = env_add_tconstr e (tconstr "float") (mk_base the_typ_float) in
  let e = env_add_tconstr e (tconstr "string") (mk_base the_typ_string) in
  let e = env_add_tconstr e (tconstr "unit") (mk_base the_typ_unit) in
  let e = env_add_tconstr e (tconstr "exn") (mk_base the_typ_exn) in
  (* Adding special types for corner cases *)
  let e = env_add_tconstr e (tconstr "type_bbe") (mk_base the_typ_bbe) in
  let e = env_add_tconstr e (tconstr "type_top") (mk_base the_typ_top) in
  let e =
    let tv = tvar_rigid "'a" in
    let t = typ_rigid tv in
    env_add_tconstr e (tconstr "list") {
      tconstr_tvars = [tv] ;
      tconstr_def = Tconstr_def_sum [
        (constr "[]", typ_list t) ;
        (constr "::", typ_arrow [t; typ_list t] (typ_list t)) ; (* Note : isn't this a "typ_arrow [t] (typ_arrow [typ_list t] (typ_list t))" *)
      ] ;
      tconstr_typ = None
    } in
  let e =
    let tv = tvar_rigid "'a" in
    let t = typ_rigid tv in
    env_add_var e (var "[]") (mk_sch [tv] (typ_list t)) in
  let e =
    let tv = tvar_rigid "'a" in
    let t = typ_rigid tv in
    env_add_var e (var "::")
      (mk_sch [tv]
        (typ_arrow [t; typ_list t] (typ_list t))) in
  let e =
    env_add_var e (var "^")
      (mk_sch []
        (typ_arrow [the_typ_string; the_typ_string] the_typ_string)) in
  let e =
    let tv = tvar_rigid "'a" in
    let t = typ_rigid tv in
    env_add_var e (var "Some")
      (mk_sch [tv]
        (typ_arrow [t] (typ_option t))) in
  (* Ne sait pas unifier "typ_arrow [typ_tuple tl] (typ_option (typ_tuple tl))" avec "typ_arrow [t] (typ_option t)" *)
  let e =
    let tv = tvar_rigid "'a" in
    let t = typ_rigid tv in
    env_add_var e (var "None") (mk_sch [tv] (typ_option t)) in

  (* Hard coding "__pattern_" versions *)
  (* expected to return one binding *)
  let e =
    let tv = tvar_rigid "'a" in
    let t = typ_rigid tv in
    env_add_var e (var "__pattern_Some")
      (mk_sch [tv]
        (typ_arrow [typ_option t] (typ_option t))) in
  (* expected to return no binding --> So typed to return a boolean *)
  let e =
    let tv = tvar_rigid "'a" in
    let t = typ_rigid tv in
    env_add_var e (var "__pattern_None") (mk_sch [tv] (typ_arrow [typ_option t] (the_typ_bool))) in
  let e = (* Define [assert false] that is made to have type "'a. 'a". "for any a, type a" useful for typing useless branches *)
    let tv = tvar_rigid "'a" in
    let t = typ_rigid tv in
    env_add_var e (var "__assert_false") (mk_sch [tv] t) in
  let e =
    let tv = tvar_rigid "'a" in
    let t = typ_rigid tv in
    env_add_var e (var "=") (mk_sch [tv] (typ_arrow [t; t] (the_typ_bool))) in
  (* Exception related constructs *)
  let e =
    let tv = tvar_rigid "'a" in
    let t = typ_rigid tv in
    env_add_var e (var "raise") (mk_sch [tv] (typ_arrow [the_typ_exn] t)) in
  let e =
    let tv = tvar_rigid "'a" in
    let t = typ_rigid tv in
    env_add_var e (var "Exn_exit") (mk_sch [tv] (typ_arrow [the_typ_string; t] the_typ_exn)) in
  let e =
    env_add_var e (var "Exn_next") (mk_sch [] (typ_arrow [the_typ_string] the_typ_exn)) in
  e

(* let env_builtin_with_tuples =
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
 *)
(* ** Smart constructors *)

let env_item_var_nonpolymorphic (ty : typ) : sch = sch_of_nonpolymorphic_typ ty


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
  | Flexible x, Flexible y -> compare x y
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

let typ_tuple_inv_opt ty : (typ list) option =
  match ty.typ_desc with
  | Typ_constr (c, tys) when c = tconstr "*" -> Some tys
  | _ -> None

let typ_option_inv_opt ty : typ option =
  match ty.typ_desc with
  | Typ_constr (c, [typ]) when c = tconstr "option" -> Some typ
  | _ -> None

let typ_option_inv (ty: typ) : typ =
  match typ_option_inv_opt ty with
  | Some res -> res
  | None -> failwith "typ_option_inv: the argument is not an option type"


let typ_of (t : trm) : typ = (* TODO: should we put get_repr here ? or in a wrapper for this function in typecheck.ml? *)
  t.trm_typ


(*#########################################################################*)

(** * Iterators on trm *)

let ex_iter (f : trm -> unit) (ex : except) : unit =
  match ex with
  | (_, Some t) -> f t
  | _ -> ()

let trm_iter (f : trm -> unit) (t : trm) : unit =
  match t.trm_desc with
  | Trm_pat_var _
  | Trm_var _
  | Trm_pat_wild
  | Trm_break _
  | Trm_continue _
  | Trm_next _
  | Trm_cst _ -> ()
  | Trm_funs (_, _, t) -> f t
  | Trm_if (_, t1, t2, t3) -> List.iter f [t1; t2; t3]
  | Trm_let ({ let_def_body = t1 ; _ }, t2) -> List.iter f [t1; t2]
  | Trm_apps (t, ts) -> List.iter f (t :: ts)
  | Trm_annot (t, _) -> f t
  | Trm_forall (_, t) -> f t
  | Trm_match (_, t, pts) -> List.iter f (t :: List.map snd pts)
  | Trm_tuple ts -> List.iter f ts
  | Trm_not t -> f t
  | Trm_and (t1, t2) -> List.iter f [t1; t2]
  | Trm_or (t1, t2) -> List.iter f [t1; t2]
  | Trm_switch (_, cases) -> List.iter (fun (b, k) -> f b; f k) cases
  | Trm_while (_, b, t) -> List.iter f [b; t]
  | Trm_exit (_, t) -> f t
  | Trm_return (_, t) -> f t
  | Trm_block (_, t) -> f t
  (* | Trm_raise (_, Some t) -> f t
  | Trm_raise ex -> ex_iter f ex
  | Trm_try (t1, ex, t2) -> f t1; ex_iter f ex; f t2 *)
  | Trm_bbe_is (t, p) -> List.iter f [t; p]
  | Trm_pat_when (p, b) -> List.iter f [p; b]

let ex_map (f : trm -> trm) (ex : except) : except =
  match ex with
  | (lbl, Some t) ->
    let t' = f t in
    if t' == t then ex
    else (lbl, Some t')
  | _ -> ex

let trm_map (f : trm -> trm) (t : trm) : trm =
  let loc = t.trm_loc in
  let typ = t.trm_typ in
  (* let annot = t.trm_annot in *)
  match t.trm_desc with
  | Trm_pat_var _
  | Trm_var _
  | Trm_pat_wild
  | Trm_break _
  | Trm_continue _
  | Trm_next _
  | Trm_cst _ -> t
  | Trm_funs (l, vs, t1) ->
    let t2 = f t1 in
    if t2 == t1 then t else trm_funs ~loc ~typ l (* ~annot *) vs t2
  | Trm_if (l, t1, t2, t3) ->
    let t1' = f t1 in
    let t2' = f t2 in
    let t3' = f t3 in
    if t1' == t1 && t2' == t2 && t3' == t3 then t else trm_if ~loc ~typ l (* ~annot *) t1' t2' t3'
  | Trm_let (ld, t2) ->
    let t1 = ld.let_def_body in
    let t1' = f t1 in
    let t2' = f t2 in
    if t1' == t1 && t2' == t2 then t
    else
      begin match ld.let_def_bind with
      | Bind_anon -> trm_seq ~loc ~typ (* ~annot *) t1' t2'
      | Bind_var _ (* | Bind_register_instance (_, _) *) ->
        mktrm ~loc ~typ (* ~annot *) (Trm_let ({ ld with let_def_body = t1' }, t2'))
      end
  | Trm_apps (tf, ts) ->
    let tf' = f tf in
    let ts' = List.map f ts in
    if tf' == tf && List.for_all2 (==) ts' ts then t else trm_apps ~loc ~typ (* ~annot *) tf' ts'
  | Trm_annot (t0, ty) ->
    let t1 = f t0 in
    if t1 == t0 then t else trm_annot ~loc ~typ (* ~annot *) t1 ty
  | Trm_forall (ty, t0) ->
    let t1 = f t0 in
    if t1 == t0 then t else trm_forall ~loc ~typ (* ~annot *) ty t1
  | Trm_match (l, t1, pts) ->
    let t2 = f t1 in
    let pts' = List.map (fun (p, t) -> (f p, f t)) pts in
    if t2 == t1 && List.for_all2 (fun (p', t') (p, t) -> p' == p && t' == t) pts' pts then t
    else trm_match ~loc ~typ l (* ~annot *) t2 pts'
  | Trm_tuple ts ->
    let ts' = List.map f ts in
    if List.for_all2 (==) ts' ts then t
    else trm_tuple ~loc ~typ (* ~annot *) ts'
  | Trm_not t ->
    let t' = f t in
    if t' == t then t
    else trm_not ~loc ~typ (* ~annot *) t'
  | Trm_and (t1, t2) ->
    let t1' = f t1 in
    let t2' = f t2 in
    if t1' == t1 && t2' == t2 then t
    else trm_and ~loc ~typ (* ~annot *) t1' t2'
  | Trm_or (t1, t2) ->
    let t1' = f t1 in
    let t2' = f t2 in
    if t1' == t1 && t2' == t2 then t
    else trm_or ~loc ~typ (* ~annot *) t1' t2'
  | Trm_switch (l, cases) ->
    let cases' = List.map (fun (b, t) -> let b = f b in let t = f t in (b, t)) cases in
    if List.for_all2 (==) cases' cases then t
    else trm_switch ~loc ~typ l cases'
  | Trm_while (l, b1, t2) ->
    let b1' = f b1 in
    let t2' = f t2 in
    if b1 == b1' && t2 == t2' then t
    else trm_while ~loc ~typ l (* ~annot *) b1' t2'
  | Trm_exit (lbl, t1) ->
    let t1' = f t1 in
    if t1' == t1 then t
    else trm_exit ~loc ~typ lbl t1'
  | Trm_return (lbl, t1) ->
    let t1' = f t1 in
    if t1' == t1 then t
    else trm_return ~loc ~typ lbl t1'
  | Trm_block (lbl, t1) ->
    let t1' = f t1 in
    if t1' == t1 then t
    else trm_block ~loc ~typ lbl t1'
  (* | Trm_raise ex ->
    let ex' = ex_map f ex in
    if ex' == ex then t
    else trm_raise ex'
  | Trm_try (t1, ex, t2) ->
    let t1' = f t1 in
    let ex' = ex_map f ex in
    let t2' = f t2 in
    if t1' == t1 && ex' == ex && t2' == t2 then t
    else trm_try t1' ex' t2' *)
  | Trm_bbe_is (t1, p1) ->
    let t1' = f t1 in
    let p1' = f p1 in
    if t1 == t1' && p1 == p1' then t
    else trm_bbe_is ~loc ~typ (* ~annot *) t1' p1'
  | Trm_pat_when (p1, b2) ->
    let p1' = f p1 in
    let b2' = f b2 in
    if p1 == p1' && b2 == b2' then t
    else trm_pat_when ~loc ~typ (* ~annot *) p1' b2'

(** * Iterators on patterns *)

(* let pat_iter (f : pat -> unit) (p : pat) : unit =
  match p.pat_desc with
  | Pat_any
  | Pat_var _
  | Pat_constant _ -> ()
  | Pat_alias (p, _) -> f p
  | Pat_tuple ps -> List.iter f ps
  | Pat_construct (_, ps) -> List.iter f ps
  | Pat_constraint (p, _) -> f p
  | Pat_or (p1, p2) -> f p1 ; f p2 *)

(* let pat_map (f : pat -> pat) (p : pat) : pat =
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
    else pat_or ~loc ~typ p1' p2' *)

(* let pat_vars p =
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
  List.sort_uniq compare (aux p) *)

(* let rec pat_clone p =
  let loc = p.pat_loc in
  let typ = p.pat_typ in
  match p.pat_desc with
  | Pat_any -> pat_any ~loc ~typ ()
  | Pat_var x -> pat_var ~loc ~typ x
  | Pat_constant c -> pat_constant ~loc ~typ c
  | _ -> pat_map pat_clone p *)

(** * Iterators on program *)

let rec trm_clone t =
  let loc = t.trm_loc in
  let typ = t.trm_typ in
  match t.trm_desc with
  | Trm_var x ->
    mktrm ~loc ~typ (Trm_var x (* { x with varid_env = x.varid_env ; varid_resolution = x.varid_resolution } *))
  | Trm_cst c -> trm_cst ~loc ~typ c
  | _ -> trm_map trm_clone t

(* let rec pat_map_typ f p =
  let aux = pat_map_typ f in
  let f_syntyp sty = { sty with syntyp_typ = f sty.syntyp_typ } in
  let p = { p with pat_typ = f p.pat_typ } in
  let loc = p.pat_loc in
  let typ = p.pat_typ in
  match p.pat_desc with
  | Pat_constraint (p, sty) -> pat_constraint ~loc ~typ (aux p) (f_syntyp sty)
  | _ -> pat_map aux p *)

let rec trm_map_typ f t =
  let aux = trm_map_typ f in
  let f_syntyp sty = { sty with syntyp_typ = f sty.syntyp_typ } in
  let f_sch sch = { sch with sch_body = f sch.sch_body } in
  let t = { t with trm_typ = f t.trm_typ } in
  let loc = t.trm_loc in
  let typ = t.trm_typ in
  match t.trm_desc with
  | Trm_var varid -> trm_var_varid ~loc ~typ varid (*  { varid with varid_typ = f varid.varid_typ } *)
  | Trm_funs (l, vs, t) -> trm_funs ~loc ~typ l (List.map (fun (x, sty) -> (x, f_syntyp sty)) vs) (aux t)
  | Trm_annot (t, sty) -> trm_annot ~loc ~typ (aux t) (f_syntyp sty)
  | Trm_match (l, t, pts) ->
    trm_match ~loc ~typ l (aux t) (List.map (fun (p, t) -> (aux p, aux t)) pts)
(*     trm_match ~loc ~typ (aux t) (List.map (fun (p, t) ->
      (pat_map_typ f p, aux t)) pts) *)
  | Trm_let ({ let_def_rec = rf ; let_def_bind = Bind_var (x, synschopt) ; let_def_body = t1 }, t2) ->
    let t1 = aux t1 in
    let t2 = aux t2 in
    let synschopt =
      Option.map (fun synsch ->
        (* FIXME: One should probably check that no nasty shadowing is taking place here. *)
        { synsch with synsch_sch = f_sch synsch.synsch_sch }) synschopt in
    trm_let ~loc ~typ rf (x, synschopt) t1 t2
  (* | Trm_let ({ let_def_rec = rf ; let_def_bind = Bind_register_instance (sym, inst) ; let_def_body = t1 }, t2) ->
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
    } t2 *)
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
  | Flexible ty_flex_name -> raise (Contains_flexible ty_flex_name)
  | _ -> typ_iter contains_flexible_exn ty


let contains_flexible (ty : typ) : bool =
  try
    contains_flexible_exn ty;
    false
  with
  | Contains_flexible _ -> true

