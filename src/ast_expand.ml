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

let expand_funs ~loc (args : varsyntyps) (body : expression) : expression =
  List.fold_right (fun (x, sty) acc ->
    let pat = ppat_var ~loc (Located.mk ~loc x) in
    let pat' = match sty.syntyp_syntax with
      | { ptyp_desc = Ptyp_any; _ } -> pat
      | ty -> ppat_constraint ~loc pat ty
    in
    pexp_fun ~loc Nolabel None pat' acc
  ) args body

(* Main translation function from DSL trm to OCaml expression *)
let rec expand_trm (t : trm) : expression =
  let aux = expand_trm in
  let loc = t.trm_loc in
  match t.trm_desc with

  | Trm_var x when x = "__assert_false" ->
      pexp_assert ~loc (ebool ~loc false)

  | Trm_var constr_name when is_capitalized constr_name ->
      pexp_construct ~loc (Located.mk ~loc (Lident constr_name)) None

  | Trm_var x ->
      pexp_ident ~loc (Located.mk ~loc (Lident x))

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

  | Trm_apps ({ trm_desc = Trm_var constr_name; _ }, ts) when is_capitalized constr_name ->
    let ts' = List.map aux ts in
      let args = match ts' with
        | [t0] -> Some t0
        | _ -> Some (pexp_tuple ~loc ts')
      in
      pexp_construct ~loc (Located.mk ~loc (Lident constr_name)) args

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

  | _ -> failwith "TODO: implement expansion of all exception handling constructs"

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

  | Trm_pat_wild ->
      ppat_any ~loc

  | Trm_pat_var x ->
      ppat_var ~loc (Located.mk ~loc x)

  | Trm_var x ->
      ppat_construct ~loc (Located.mk ~loc (Lident x)) None

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

  let filtered_p = List.filter (is_not_external) p in

  if !Flags.presentation then (* open_stdlib:: *)(List.map expand_topdef filtered_p)
  else func::(List.map expand_topdef p)