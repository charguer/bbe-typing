I want to write a ppx rewriter for OCaml. And I am in the final phase. 
Here are the details of my framework:

Extended OCaml code (with custom syntax) -> OCaml parsing -> translation into internal DSL AST -> typing and verifications that the syntax is correct -> source-to-source translation to a smaller syntax in DSL AST -> ???

I want to fill the last step with a translation from the DSL AST into OCaml AST. 
All of the code is, and should be written in OCaml. 

Here are the details:
Internal representation of the AST:

```ocaml
type trm_desc =
  | Trm_var of varid
  | Trm_cst of cst
  | Trm_funs of varsyntyps * trm          (* fun (x1 : tyn) ... (xn : tyn) -> trm_body *)
  | Trm_if of trm * trm * trm             (* if t1 then t2 else t3 *)
  (* Check lower for comments on the "trm_let" representation.  *)
  | Trm_let of let_def * trm              (* [t1 ; t2], [let rec x = t1 in t2], [let[@register (+) _ = t1 in t2]] *)
  | Trm_apps of trm * trms                (* Application. Partial application is not allowed. *)
  | Trm_annot of trm * syntyp             (* (t : ty) *)
  | Trm_forall of tvar_rigid * trm        (* fun (type a) -> t *)
  | Trm_match of trm * (pat * trm) list   (* match t with p1 -> t1 | ... | pn -> tn *)
  | Trm_tuple of trm list
  | Trm_not of trm (* TODO: not, and, or could be fun *)
  | Trm_and of trm * trm
  | Trm_or of trm * trm
  | Trm_switch of (bbe * trm) list
  | Trm_while of bbe * trm
  (*BBE constructions*)
  | Trm_bbe_is of trm * pat
  (*Pattern constructions*)
  | Trm_pat_var of varid
  | Trm_pat_wild
  | Trm_pat_when of pat * trm
  (*
  LATER: Trm_for of dir * var * trm * trm * trm
  *)
and trm = {
  trm_desc : trm_desc;
  trm_loc : loc;
  trm_typ : typ; (* a fresh flexible after parsing *)
  (* Should we add a trm_binds attribute? Like : if it was a bbe, then it would bind ...
    And then for patterns, the type would be the input, and "binds" would be the output... *)
  trm_binds : env0 option; (* An option used both to easily get result bindings, and notify if the term actually has result bindings *)
  trm_env : env0; (* a dummy environment after parsing *)
  (* trm_annot : annot (* to help printing back of encoded terms *) *)
}
```

```ocaml
type cst =
| Cst_bool of bool
| Cst_int of int
| Cst_float of float
| Cst_string of string
| Cst_unit of unit

```

```ocaml
and trms = trm list

and bbe = trm
and pat = trm
(** An [env_var] is a typing environment for resolving program variables
  (typically defined by a let-binding): it associates an [env_item] to every
  variable name. Technically, the keys are symbols, due to our encodings
  (see definition of type [symbol]). *)
type env_var = (var, sch) Env.t (* LATER: rename to env_symbol? *)

(** An [env_tconstr] is a typing environment for type constructors (e.g. [list]):
   it associates a type constructor descriptor ([tconstr_desc])
   to every type constructor name (e.g., [list], [array]) *)
type env_tconstr = (tconstr, tconstr_desc) Env.t

(** An [env] is the combined structure that provides a typing environment
    for program variables, type variables (which are represented as
    constructors of arity zero), and type constructors. *)
type env = {
  env_var : env_var;
  env_tconstr : env_tconstr;
  env_is_in_pattern : bool; (* Useful to recognize when to look for a "Pattern__" version. *)
  (* For all constr name (capitalized functions), give its arity. *)
  (* Will include Some, None, and other builtin constructors as well *)
  (* env_constr : (var, int) Env.t *)
}
```

```ocaml
(* Another kind of type variables are the 'rigid' type variable, to capture polymorphic ['a],
  whose structure cannot be refined.  We implement them as a local abstract type constructor,
  whose name is ['a].  Note that the quote is part of its name. *)
type tvar_rigid = tconstr

(** A [typ_desc] describes the possible structure of a type. A type may be:
    - a 'flexible' variable, that is a variable that can unify with anything else.
    - a 'unified' variable, which was a flexible variable but has since then been
      unified with another type variable (Unified is like a link in the Union-Find data structure:
      the [typ] pointed by Unified contains mutable fields).
    - a 'structured' type, that is, the application of a type constructor, e.g. [int list].
    - internal types can furthermore be a 'not-yet-resolved-instance-of-an-overloaded-symbol'
      (see [internal_type] below).
    - a 'rigid' variable (i.e. polymorphic type) is also represented as a constr.

    A constant type is represented as a [Typ_constr] with no arguments,
    e.g. the type [int] is [Typ_constr (tconstr "int") []].

    The function type describes n-ary functions (i.e. not curried function types).
    A function that expects 3 arguments is written [let f (x:ty_x) (y:ty_y) (z:ty_z) : ty_ret = t]
    using the curried syntax, but internally is represented as a [trm_funs], whose type
    is written [(tx_x, ty_y, ty_z) -> ty_ret], and represented using the following encoding:
    [typ_constr (tconstr "->") ([ty_x; ty_y; ty_pz; ty_ret])], with the return type at the
    end of the list. This encoding should be manipulated exclusively via functions
    [typ_arrow] and [typ_arrow_inv], never directly.
    *)
type typ_desc =
  | Flexible of tvar
  | Unified of typ
  | Typ_constr of tconstr * typs

(** A [typ] describes a type object in the unification process performed by the typechecker.
    A [typ] has a _mutable_ description, of type [typ_desc].
    This description should only be modified by means of [History.make_modif_desc],
    for the rollback mechanism to work properly.

    To check whether two types are equal, first we follow the [Unified] links,
    to reach the roots, like in a union-find. Then, we may compare the two roots:
    - if the two roots are physically equal, the types are the same
    - else the types might have unifiable structures (e.g. ['a list] and [int list]) *)
and typ = {
  mutable typ_desc : typ_desc (* to be modified only via [History.make_modif_desc] *) ;
  mutable typ_mark : mark (* for internal use by the [get_repr] function *)
}
(** A [loc] denotes a location in the source code *)
type loc = Location.t
and varid = string (* TODO rename varid to var *)
type let_def = {
  let_def_rec : rec_flag;
  let_def_bind : bind;
  let_def_body : trm0;
}
```

```ocaml
type bind =
  | Bind_anon                                       (* From sequences and [let _ =]. *)
  | Bind_var of varsynschopt                        (* [let x : (type a. a -> a) = ...] *)
  (*   | Bind_register_instance of symbol * instance_sig (* [let[@register (+)] _ (type a) (op[@implicit (+)]) = ...] *) *)
(** A [varsynschopt] describes e.g. [x: (type a. a list)] to
   represent a type-annotated let-bound variable, whose
   type is possibly polymorphic. *)
type varsynschopt = var * synsch option
and synsch = {
  synsch_syntax : tvar_rigid list * styp;
  synsch_sch : sch; (* possibly dummy after parsing *)
}
type styp = Parsetree.core_type (* FIXME: rename to parsetyp *)
type syntyp = {
  syntyp_syntax : styp;
  syntyp_typ : typ; (* a fresh flexible after parsing,
                       possibly a fresh flexible after typing,
                       e.g. in case of an argument without explicit type *)
}
and sch = {
  sch_tvars : tvar_rigid list;
  sch_body : typ;
}

and varsyntyp = var * syntyp
and varsyntyps = varsyntyp list
```


More information :
In the internal representation, constructor usage are seen as function application. This means that `C (t1, t2)` is an application of the term variable C, to a list of 2 arguments. And a constructor with arguments is just a standalone variable. 
The xxx0 types are aliases of xxx type. For example, env0 is an alias for env, trm0 is an alias for trm, etc.

Implementation details :

I want you to write a this with ppxlib, in particular Ast_builder.Default. 

Here is the list of the syntax I want you to handle: 
```
| trm_var
| trm_cst
| trm_funs
| trm_let
| trm_funs
| trm_apps
| trm_annot
| trm_forall
| trm_tuple
| trm_not
| trm_and
| trm_or
```

You should write functions called expand_trm, expand_program, expand_topdef.   

For recursive functions, define in advance an auxiliary alias for simplicity.

```ocaml
  let aux = expand_trm
```
And use this in recursive calls instead. 

The code must be as inlined as possible, and as simple as possible. Namely, any code portion of less than 5 lines should be written inlined.

For example, this kind of code should be inlined:
```ocaml
  | Trm_var constr_name ->
      let y_var = trm_var ~loc scrutinee_var in
      let success_case = (p, on_success) in
      let wildcard = trm_pat_wild ~loc () in
      let failure_case = (wildcard, on_failure) in
      trm_match ~loc y_var [success_case; failure_case]
```

You should respect the writing conventions of the framework. Here is an example of code going through the trm_desc type, take it as a skeleton/example of the writing style:
```ocaml
match t.trm_desc =
  | Trm_var x -> 
  | Trm_cst c -> 
  | Trm_funs (args, t1) -> 
  | Trm_if (b0, t1, t2) -> 
  | Trm_let (b, t2) -> 
  | Trm_apps (t0, ts) -> 
  | Trm_annot (t1, sty) -> 
  | Trm_forall (n, t1) ->
  | Trm_match (t0, pts) ->
  | Trm_tuple ts -> 
  | Trm_not t1 -> 
  | Trm_and (t1, t2) -> 
  | Trm_or (t1, t2) ->
  | Trm_switch cases -> 
  | Trm_while (b1, t2) -> 
  | Trm_bbe_is (t1, p2) ->
  | Trm_pat_var x -> 
  | Trm_pat_wild
  | Trm_pat_when (p1, b2) ->
```

Be careful of the execution order, which is very important for consistent effects. For example, do not write this:
```ocaml  
| Trm_and (t1, t2) ->
  trm_and ~loc (transform_trm fresh_state t1) (transform_trm fresh_state t2)
```
But this instead: 
```ocaml  
| Trm_and (t1, t2) ->
  let t1' = (transform_trm fresh_state t1) in
  let t2' = (transform_trm fresh_state t2) in
  trm_and ~loc t1' t2'
```
<!-- trm_ands function, that takes a list of arguments,. -->

All the helper functions you might need to understand code are:

```ocaml
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

let trm_funs ?loc ?typ (* ?annot *) (xs : varsyntyps) (t : trm) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_funs xs t)

let trm_funs_if_non_empty ?loc ?typ (* ?annot *) (xs : varsyntyps) (t : trm) : trm =
  if xs = [] then t
  else trm_funs ?loc ?typ (* ?annot *) xs t

let trm_if ?loc ?typ (* ?annot *) (t0 : trm) (t1 : trm) (t2 : trm) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_if t0 t1 t2)

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

let trm_match ?loc ?typ (* ?annot *) (t : trm) (pts : (pat * trm) list) : trm =
  mktrm ?loc ?typ (* ?annot *) (trm_desc_match t pts)

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

let trm_switch ?loc ?typ (* ?annot *) (cases : (bbe * trm) list) : trm =
  mktrm ?loc ?typ (trm_desc_switch cases)

let trm_while ?loc ?typ (* ?annot *) (b : bbe) (t : trm) : trm =
  mktrm ?loc ?typ (trm_desc_while b t)

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
```

I also have these types 

```ocaml
(* Top-level external declaration *)
type external_def = {
  external_def_var : var;
  external_def_syntyp : synsch; (* TODO: Rename into external_def_synsch. *)
  external_def_def : string list;
}

type topdef_desc =
  | Topdef_val_def of let_def
  | Topdef_typ_def of typ_def
  | Topdef_external of external_def

(* Some definitions marked with [@type_error "message"] are expected to fail with this
  exact error message. *)
type expected_error = string option

and topdef = {
  topdef_desc : topdef_desc;
  topdef_loc : loc;
  topdef_expected_error : expected_error
}

and topdefs = topdef list
and program = topdefs

type typ_def = {
  typ_def_rec : rec_flag;
  typ_def_td : Parsetree.type_declaration list;  (* FIXME: syntypdef *)
  typ_def_typs : tconstr_desc list (* The list of type constructors defined within this type declarationm.  Dummy after parsing *) (* FIXME: typedef *)
}
```

A constructor with arguments is necessarily a Trm_apps with a Trm_var as first argument. Also, the Trm_var is necessarily a capitalized word (first character is uppercase).


Ask me if you need any question, do not generate false code, if there are details you are missing, or if you see that some informations are necessary, tell me and add a comment in the code.





Here is the code for the second to last step (simplification of the syntax):

```ocaml
open Asttypes
open Parsetree
open Var
open Ast_fix
open Ast_aux
open Tools
open Ast_print
open PPrint

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
      if List.mem x env then [] else [x]

  | Trm_cst _ ->
      []

  | Trm_funs (args, t1) ->
      let bound = bound_vars_varsyntyps args in
      free_vars (bound @ env) t1

  | Trm_if (b, t1, t2) ->
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

  | Trm_match (t0, pts) ->
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

  | Trm_switch cases ->
      List.concat (List.map (fun (b, t) ->
        free_vars env b @ free_vars env t
      ) cases)

  | Trm_while (b, t1) ->
      free_vars env b @ free_vars env t1

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
	let k = fresh_var () in
	let k_var = trm_var_varid ~loc k in
	let free_vars_cont = free_vars [] cont in
	let k_call = trm_apps ~loc k_var (vars_or_unit_args free_vars_cont) in
	let k_fun = trm_funs ~loc (vars_or_unit_fun free_vars_cont) cont in
	trm_let ~loc Nonrecursive (k, None) k_fun (body k_call)

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

  | Trm_funs (args, t1) ->
      let t1' = aux_trm t1 in
      trm_funs ~loc ~typ args t1'

  | Trm_if (b0, t1, t2) ->
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

  | Trm_match (_t0, _pts) ->
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

  | Trm_switch cases ->
      comp_switch ~loc ~typ cases

  | Trm_while (b1, t2) ->
      let loop_name = "__my_loop" in
      let loop_var = trm_var_varid ~loc loop_name in
      let t2' = aux_trm t2 in
      let loop_call = trm_apps ~loc loop_var [trm_unit ~loc ()] in
      let seq = trm_seq ~loc t2' loop_call in
      let unit_syntyp = mk_syntyp_unit () in
      let body = aux_bbe b1 seq (trm_unit ~loc ()) in
      let loop_fun = trm_funs ~loc [(fresh_var (), unit_syntyp)] body in
			(* let unit_styp = unit_syntyp.syntyp_syntax in *)
			let let_typ = typ_to_styp (typ_arrow [the_typ_unit] the_typ_unit) in
      trm_let ~loc Recursive (loop_name, Some (synsch_of_nonpolymorphic_typ (mk_syntyp let_typ))) loop_fun loop_call

  (* Non-term constructors should be errors *)
  | Trm_bbe_is _ ->
      failwith "comp_trm: Trm_bbe_is is not a term"

  | Trm_pat_var _ ->
      failwith "comp_trm: Trm_pat_var is not a term"

  | Trm_pat_wild ->
      failwith "comp_trm: Trm_pat_wild is not a term"

  | Trm_pat_when _ ->
      failwith "comp_trm: Trm_pat_when is not a term"

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
      trm_if ~loc t' u u'

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
      trm_match ~loc y_var [success_case; failure_case]

  | Trm_var f ->
      (* Non-capitalized variable: boolean predicate *)
      (* (y |> g (u) (u')) ==> let x = [[g]] y in if x then u else u' *)
      let x = fresh_var () in
      let y_var = trm_var_varid ~loc y in
      let f_term = trm_var ~loc f in
      let f' = aux_trm f_term in
      let f_applied = trm_apps ~loc f' [y_var] in
      let x_var = trm_var_varid ~loc x in
      let body = trm_if ~loc x_var u u' in
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
      trm_match ~loc y_var [success_case; failure_case]

  | Trm_apps ({ trm_desc = Trm_var _f; _ } as f_term, ps) ->
      (* Function pattern: (y |> f (p1, ..., pn) (u) (u')) ==> let x = [[f]] y in (x |> Some (p1, ..., pn) (u) (u')) *)
      let x = fresh_var () in
      let y_var = trm_var_varid ~loc y in
      let f' = aux_trm f_term in
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
      trm_match ~loc y_var [(tuple_pat, body)]

  | Trm_cst c ->
      (* Constant pattern: (y |> c (u) (u')) ==> if (y = c) then u else u' *)
      let y_var = trm_var_varid ~loc y in
      let c_term = trm_cst ~loc c in
      let eq = trm_apps ~loc (trm_var_varid ~loc "=") [y_var; c_term] in
      trm_if ~loc eq u u'

  | Trm_annot (p, styp) ->
      let t' = aux_pat y p u u' in
      trm_annot ~loc t' styp
  | _ ->
      (* Non-pattern constructors should be errors *)
      failwith (Printf.sprintf "comp_pat: unexpected pattern form %s\n" (trm_to_string ~style:style_debug p))

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
```


<!-- Answer1 -->

Here is the list of the syntax I want you to handle: 
```
| trm_var
| trm_cst
| trm_funs
| trm_let
| trm_funs
| trm_apps
| trm_annot
| trm_forall
| trm_tuple
| trm_not
| trm_and
| trm_or
| trm_match 
```

Note that for trm_match the arguments are restricted to constructors, and constructor applications.
Meaning that in the cases, only patterns of the form : 
    trm_apps (trm_var C) [trm_var x1, ..., trm_var xn]
or  trm_var C  
are acceptable. 

1. yes, you can use t.trm_loc
2. I want you to general untyped OCaml code, only a Parsetree
3. Both var and varid are strings, you can use var as a string. 
4. yes, you can
5. yes, see the Note above for more context
6. Yes, this is not handled for the moment
7. Yes, you should 
8. Yes, you can
