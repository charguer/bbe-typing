(** File implementing a a second translation pass.
    Translates the minimal source language into OCaml AST. *)

(** Final pass, translates DSL into OCaml AST *)

open Ppxlib
open Ast_builder.Default
open Asttypes
open Parsetree
open Var
open Ast_fix
open Ast_aux
open Tools
open Ast_print
open PPrint

(* Helper function to recognize constructors in application position *)
let pop_last (lst : 'a list) : 'a =
  match List.rev lst with
  | x :: xs -> x
  | [] -> assert false (* only called when the lst has at least 1 argument *)

let string_to_ident (s : string) : Longident.t =
  let rec aux sl =
  (* We know by definition that the list is never empty *)
  match sl with
    | [x] -> Lident x (* This is the first element of the split *)
    | x :: sl' -> Ldot (aux sl', x)
    | [] -> assert false
  in
  aux (List.rev (String.split_on_char '.' s))

let is_capitalized (s : string) : bool =
  if String.length s = 0 then false
  else
    let c = String.get s 0 in
    c >= 'A' && c <= 'Z'

let expand_cst ~loc (c : cst) : expression =
  match c with
  | Cst_bool b -> pexp_construct ~loc (Located.mk ~loc (Lident (string_of_bool b))) None
  | Cst_int n -> pexp_constant ~loc (Pconst_integer (string_of_int n, None))
  | Cst_float f -> pexp_constant ~loc (Pconst_float (string_of_float f, None))
  | Cst_string s -> pexp_constant ~loc (Pconst_string (s, loc, None))
  | Cst_unit () -> pexp_construct ~loc (Located.mk ~loc (Lident "()")) None

let expand_cst_pat ~loc (c : cst) : Parsetree.pattern =
  match c with
  | Cst_bool b -> ppat_construct ~loc (Located.mk ~loc (Lident (string_of_bool b))) None
  | Cst_int n -> ppat_constant ~loc (Pconst_integer (string_of_int n, None))
  | Cst_float f -> ppat_constant ~loc (Pconst_float (string_of_float f, None))
  | Cst_string s -> ppat_constant ~loc (Pconst_string (s, loc, None))
  | Cst_unit () -> ppat_construct ~loc (Located.mk ~loc (Lident "()")) None

let expand_funs ~loc (args : varsyntyps) (body : expression) : expression =
  List.fold_right (fun (x, sty) acc ->
    let pat = ppat_var ~loc (Located.mk ~loc x) in
    let pat' = match sty.syntyp_syntax with
      | { ptyp_desc = Ptyp_any; _ } -> pat
      | ty -> ppat_constraint ~loc pat ty
    in
    pexp_fun ~loc Nolabel None pat' acc
  ) args body

let is_obj_magic (t : trm) : bool =
  Debug.log "Checking for Obj.magic with %s" (trm_to_string ~style:style_debug t);
  match t.trm_desc with
  | Trm_apps ({trm_desc = Trm_var fname}, _) when fname = "Obj.magic" -> Debug.log "And returned : true"; true
  | _ -> Debug.log "And returned : false"; false

(* Main translation function from IR trm to OCaml expression *)
let rec expand_trm (t : trm) : expression =
  let aux = expand_trm in
  let loc = t.trm_loc in
  match t.trm_desc with

  | Trm_var x ->
    Printf.printf "Translating variable %s\n" x;
    if x = "__assert_false" then
      pexp_assert ~loc (ebool ~loc false)
    else if is_capitalized (pop_last (String.split_on_char '.' x)) then
      pexp_construct ~loc (Located.mk ~loc (string_to_ident x)) None
    else
      pexp_ident ~loc (Located.mk ~loc (string_to_ident x))

  | Trm_cst c ->
      expand_cst ~loc c

  | Trm_funs (_, args, t1) ->
      let e1 = aux t1 in
      expand_funs ~loc args e1

  | Trm_if (_, t0, t1, t2) ->
      let e0 = aux t0 in (* note that at this point this is a term, no more bbe *)
      let e1 = aux t1 in
      let e2 = aux t2 in
      pexp_ifthenelse ~loc e0 e1 (Some e2)

  | Trm_let (ld, t2) ->
      let t2' = aux t2 in
      expand_let_def ~loc ld t2'

  | Trm_apps (t1, [t2]) when is_obj_magic t ->
    Debug.log "Captured an Obj.magic";
    let t1' = pexp_ident ~loc (Located.mk ~loc (Ldot (Lident "Obj", "magic"))) in
    let t2' = aux t2 in
    pexp_apply ~loc t1' [Nolabel, t2']

  | Trm_apps ({ trm_desc = Trm_var constr_name; _ }, ts) when is_capitalized (pop_last (String.split_on_char '.' constr_name)) ->
    let ts' = List.map aux ts in
      let args = match ts' with
        | [t0] -> Some t0
        | _ -> Some (pexp_tuple ~loc ts')
      in
      pexp_construct ~loc (Located.mk ~loc (string_to_ident constr_name)) args

  | Trm_apps ({ trm_desc = Trm_var fname; _ } as t0, [{trm_desc = Trm_apps ( {trm_desc = Trm_var constr_name} as t2, [t3; t4])} as t1]) when (fname = "raise" && constr_name = "Exn_Exit" && (not (is_obj_magic t4))) ->
    let t4' = trm_magic ~loc:t4.trm_loc t4 in (* temporary solution, the location is bound to the value inside, and not the whole obj.magic expression *)
    aux (trm_apps ~loc t0 [{t1 with trm_desc = (trm_desc_apps t2 [t3; t4'])}])

  | Trm_apps (t0, ts) -> (* Depends on whether apps is a constructor or not *)
      let t0' = aux t0 in
      let ts' = List.map aux ts in
      List.fold_left (fun acc arg -> pexp_apply ~loc acc [Nolabel, arg]) t0' ts'

  | Trm_annot (t1, sty) ->
      let t1' = aux t1 in
      let ty = sty.syntyp_syntax in
      pexp_constraint ~loc t1' ty

  | Trm_forall (n, t1) ->
      let t1' = aux t1 in
      let type_name = (print_tvar_rigid n) in
      pexp_newtype ~loc (Located.mk ~loc type_name) t1'

  | Trm_match (_, t0, pts) ->
      let t0' = aux t0 in
      let cases = List.map (fun (p, t) ->
        let p' = expand_pattern p in
        let t' = aux t in
        case ~lhs:p' ~guard:None ~rhs:t'
      ) pts in
      pexp_match ~loc t0' cases

  | Trm_try_with (t1, p, t2) ->
    let t1' = aux t1 in

    let p' = expand_pattern p in
    let t2' = aux t2 in
    let c = case ~lhs:p' ~guard:None ~rhs:t2' in

    pexp_try ~loc t1' [c]


  | Trm_tuple ts ->
      let ts' = List.map aux ts in
      pexp_tuple ~loc ts'

  | Trm_not t1 ->
      let t1' = aux t1 in
      pexp_apply ~loc (pexp_ident ~loc (Located.mk ~loc (Lident "not"))) [Nolabel, t1']

  | Trm_and (t1, t2) ->
      let t1' = aux t1 in
      let t2' = aux t2 in
      pexp_apply ~loc (pexp_ident ~loc (Located.mk ~loc (Lident "&&"))) [Nolabel, t1'; Nolabel, t2']

  | Trm_or (t1, t2) ->
      let t1' = aux t1 in
      let t2' = aux t2 in
      pexp_apply ~loc (pexp_ident ~loc (Located.mk ~loc (Lident "||"))) [Nolabel, t1'; Nolabel, t2']

  | Trm_switch _ -> failwith "expand_trm: Trm_switch should have been eliminated by comp_program"
  | Trm_while _ -> failwith "expand_trm: Trm_while should have been eliminated by comp_program"
  | Trm_bbe_is _ -> failwith "expand_trm: Trm_bbe_is is not a term"
  | Trm_pat_var _ -> failwith "expand_trm: Trm_pat_var is not a term"
  | Trm_pat_wild -> failwith "expand_trm: Trm_pat_wild is not a term"
  | Trm_pat_when _ -> failwith "expand_trm: Trm_pat_when is not a term"

  | _ -> Debug.log "Trying to expand term : %s\n" (trm_to_string ~style:style_debug t);
    failwith "TODO: implement expansion of all exception handling constructs"

and expand_let_def ~loc (ld : let_def) (t2 : expression) : expression =
  let rec_flag = ld.let_def_rec in
  let body = expand_trm ld.let_def_body in
  let vb = match ld.let_def_bind with
    | Bind_anon -> value_binding ~loc ~pat:(ppat_any ~loc) ~expr:body
    | Bind_var (x, sch_opt) ->
        let pat = ppat_var ~loc (Located.mk ~loc x) in
        let pat' = match sch_opt with
          | None -> pat
          | Some synsch ->
              let (_tvars, ty) = synsch.synsch_syntax in
              ppat_constraint ~loc pat ty
        in
        value_binding ~loc ~pat:pat' ~expr:body
  in
  pexp_let ~loc rec_flag [vb] t2

and expand_pattern (p : pat) : pattern =
  let loc = p.trm_loc in
  match p.trm_desc with

  | Trm_cst c ->
    (expand_cst_pat ~loc c)

  | Trm_pat_wild ->
    ppat_any ~loc

  (* | Trm_pat_var x when (x = "Exn_Next" || x = "Exn_Exit") ->
    ppat_exception ~loc (ppat_var ~loc (Located.mk ~loc x)) *)

  | Trm_pat_var x ->
    ppat_var ~loc (Located.mk ~loc x)

  | Trm_var x ->
    ppat_construct ~loc (Located.mk ~loc (Lident x)) None

  (* | Trm_apps ({ trm_desc = Trm_var x; _ }, ps) when (x = "Exn_Next" || x = "Exn_Exit") ->
    let ps' = List.map expand_pattern ps in
    let arg_pat = match ps' with
      | [p_single] -> Some p_single
      | _ -> Some (ppat_tuple ~loc ps')
    in
    ppat_exception ~loc (ppat_construct ~loc (Located.mk ~loc (Lident x)) arg_pat) *)

  | Trm_apps ({ trm_desc = Trm_var x; _ }, ps) ->
    let ps' = List.map expand_pattern ps in
    let arg_pat = match ps' with
      | [p_single] -> Some p_single
      | _ -> Some (ppat_tuple ~loc ps')
    in
    ppat_construct ~loc (Located.mk ~loc (Lident x)) arg_pat


  | Trm_tuple ps ->
    let ps' = List.map expand_pattern ps in
    ppat_tuple ~loc ps'

  | _ ->
    Debug.log "Trying to expand pattern : %s\n" (trm_to_string ~style:style_debug p);
    failwith "expand_pattern: unsupported pattern form"

(* Top-level definitions *)
let expand_topdef (td : topdef) : structure_item =
  let loc = td.topdef_loc in
  match td.topdef_desc with

  | Topdef_val_def ld ->
      let body = expand_trm ld.let_def_body in
      let rec_flag = ld.let_def_rec in
      let vb = match ld.let_def_bind with
        | Bind_anon -> value_binding ~loc ~pat:(ppat_any ~loc) ~expr:body
        | Bind_var (x, sch_opt) ->
            let pat = ppat_var ~loc (Located.mk ~loc x) in
            let pat' = match sch_opt with
              | None -> pat
              | Some synsch ->
                  let (_tvars, ty) = synsch.synsch_syntax in
                  ppat_constraint ~loc pat ty
            in
            value_binding ~loc ~pat:pat' ~expr:body
      in
      pstr_value ~loc rec_flag [vb]

  | Topdef_typ_def typ_def ->
      pstr_type ~loc typ_def.typ_def_rec typ_def.typ_def_td

  | Topdef_external ext_def ->
      let x = ext_def.external_def_var in
      let (_tvars, ty) = ext_def.external_def_syntyp.synsch_syntax in
      let prims = ext_def.external_def_def in
      pstr_primitive ~loc (value_description ~loc ~name:(Located.mk ~loc x) ~type_:ty ~prim:prims)

let is_not_external s : bool =
  match s.topdef_desc with
  | Topdef_external _ -> false
  | _ -> true

let expand_program (p : program) : structure =

  let func : structure_item =
    let loc = Location.none in
  pstr_type ~loc:loc Recursive [
    type_declaration
      ~loc
      ~name:(Located.mk ~loc "func")
      ~params:[
        (ptyp_var ~loc "a", (NoVariance, NoInjectivity))
      ]
      ~cstrs:[]
      ~kind:Ptype_abstract
      ~private_:Public
      ~manifest:(Some (ptyp_var ~loc "a"))
  ]
  in

 (*  let open_stdlib : structure_item =
  let loc = Location.none in
  Ast_builder.Default.pstr_open ~loc
    (Ast_builder.Default.open_infos
       ~loc
       ~expr:
         (Ast_builder.Default.pmod_ident ~loc
            (Located.lident ~loc "Stdlib"))
       ~override:Fresh)
  in *)
  let exn_decl_next =
    let loc = Location.none in
    let string_ty =
      ptyp_constr
        ~loc
        (Located.lident ~loc "string")
        []
    in
    pstr_exception
      ~loc
      (type_exception
        ~loc
        (extension_constructor
            ~loc
            ~name:(Located.mk ~loc "Exn_Next")
            ~kind:(Pext_decl ([], (Pcstr_tuple [string_ty]), None))))
  in

  let exn_decl_exit =
    let loc = Location.none in
    let string_ty =
      ptyp_constr
        ~loc
        (Located.lident ~loc "string")
        []
    (* in
    let int_ty =
      ptyp_constr
        ~loc
        (Located.lident ~loc "int")
        []
    in
    let int_ref_ty =
      ptyp_constr
        ~loc
        (Located.lident ~loc "ref")
        [int_ty] *)
      in
    let dummy_ty =
      ptyp_constr
        ~loc
        (Located.lident ~loc "float")
        []
    in
    pstr_exception
      ~loc
      (type_exception
        ~loc
        (extension_constructor
            ~loc
            ~name:(Located.mk ~loc "Exn_Exit")
            ~kind:(Pext_decl ([], (Pcstr_tuple [string_ty; dummy_ty]), None))))
  in
  let filtered_p = List.filter (is_not_external) p in

  let result = if !Flags.presentation then (List.map expand_topdef filtered_p)
               else let expand_p = (List.map expand_topdef p) in
               let out = open_out "generated_ast.txt" in
               let f_out = Format.formatter_of_out_channel out in
               Ocaml_common.Printast.implementation f_out expand_p;
               close_out out;
  exn_decl_next::exn_decl_exit::func::expand_p
  in
  let out = open_out "generated_ml.ml" in
  output_string out (Pprintast.string_of_structure result);
  close_out out;

  result

  (* typ_arrow [the_typ_string; t] the_typ_exn *)

  (* Say we already have the env of all the labels.
  What we do is that during typing, we not only add to the env, but also to a global env *)
  (* This global env would be only for exit, since next does not need polymorphism *)
  (* Env from label to ty. No sty for the moment, since not everything has a type. But if we code in caml all of this will be typ-top. And this is not text but ast... *)

  (*
  let e = env_add_tconstr e (tconstr "*") mk_special in
  let e = env_add_tconstr e (tconstr "->") mk_special in
  let e = env_add_tconstr e (tconstr "option") mk_special in
  let e = env_add_tconstr e (tconstr "int") (mk_base the_typ_int) in
  let e = env_add_tconstr e (tconstr "bool") (mk_base the_typ_bool) in
  let e = env_add_tconstr e (tconstr "float") (mk_base the_typ_float) in
  let e = env_add_tconstr e (tconstr "string") (mk_base the_typ_string) in
  let e = env_add_tconstr e (tconstr "unit") (mk_base the_typ_unit) in
  *)

  (*
  Ast_builder.Default.ptyp_int    ~loc
Ast_builder.Default.ptyp_string ~loc
Ast_builder.Default.ptyp_unit   ~loc
Ast_builder.Default.ptyp_bool   ~loc
Ast_builder.Default.ptyp_float  ~loc

  let ty_int    = Ast_builder.Default.ptyp_int    ~loc
let ty_string = Ast_builder.Default.ptyp_string ~loc
let ty_unit   = Ast_builder.Default.ptyp_unit   ~loc
*)

  (* write a function : [typ_into_styp] that does translation from our custom types into ocaml syntax types. For the moment only do the constant types, document it, and if it works we'll see later for the rest. *)
  (* The translation would be done in the later stage, in ast expand, to avoid useless tasks. And so we can unify during typechecking *)
  (* Ok. *)

  (* Voici les types qui nous intéressent. On veut traduire ça en Parsetree.core_type en gros. Ça suffira pour le moment *)
  (* Par exemple, int, bool, float et string seront traduits en "cst int", "cst bool" etc *)

  (* Ok, how do we translate our types into OCaml types?  *)
  (* if we have the correct map, then changing smart constrs is enough. This would be weird mixing code with a string, but well it works I guess. *)
  (* And adding the list of exceptions is easy with a list.map *)
  (* Now the issue at hand is translating types correctly. I know that we can do most of it. But not optimal?
  For the moment, use the printer we made, should be enough *)
  (* what we need. We can't easily add exceptions since it is not polymorphic.
  List all of the labels present *)

  (* Exn_Exit Exn_Next *)