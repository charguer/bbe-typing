open Var
open Ast_fix
open Ast_aux
open Ast_print

let debug () = !Flags.debug

let if_debug f =
  if debug () then f ()

let log fmt =
  if debug () then Printf.ksprintf print_endline fmt
  else Printf.ifprintf () fmt


let print_low_level_typ =
  let open Printf in
  let rec aux t =
    match t.typ_desc with
    | Flexible (x, trigger) ->
      let tr =
        if VaridSet.is_empty trigger then ""
        else sprintf " (with %i triggers)" (VaridSet.cardinal trigger) in
      sprintf "Flexible %s%s" (print_tvar x) tr
    | Unified t -> Printf.sprintf "Unified (%s)" (aux t)
    | Typ_constr (id, ts) ->
      sprintf "Typ_constr (%s, %s)"
        (print_tconstr id)
        (String.concat ", " (List.map aux ts)) in
  aux

let print_low_level_syntyp sty =
  Printf.sprintf "{ styp = %s ; typ = %s }"
    (syntyp_to_string sty)
    (print_low_level_typ sty.syntyp_typ)

let print_low_level_trm =
  let open Printf in
  let rec pr_desc i =
    let aux = aux (i + 2) in function
    | Trm_var varid -> sprintf "Var %s" (symbol_to_string varid.varid_symbol)
    | Trm_cst c ->
      let c =
        match c with
        | Cst_bool true -> "true"
        | Cst_bool false -> "false"
        | Cst_int i -> string_of_int i
        | Cst_float f -> string_of_float f
        | Cst_string s -> sprintf {|"%s"|} (String.escaped s)
        | Cst_unit () -> "()" in
      sprintf "Cst %s" c
    | Trm_funs (xs, t) ->
      sprintf "Funs ([%s], %s)"
        (String.concat " ; " (List.map (fun (x, sty) ->
          sprintf "(%s : %s)" (print_var x) (print_low_level_syntyp sty)) xs))
        (aux t)
    | Trm_if (t1, t2, t3) -> sprintf "IfThenElse (%s, %s, %s)" (aux t1) (aux t2) (aux t3)
    | Trm_let ({ let_def_body = t1 ; _ }, t2) -> sprintf "Let (%s, %s)" (aux t1) (aux t2)
    | Trm_apps (t, ts) ->
      sprintf "Apps (%s, [%s])"
        (aux t)
        (String.concat " ; " (List.map aux ts))
    | Trm_annot (t, sty) ->
      sprintf "Annot (%s, %s)" (aux t) (print_low_level_syntyp sty)
    | Trm_forall (a, t) -> sprintf "Forall (%s, %s)" (print_tconstr a) (aux t)
    | Trm_match (t, pts) ->
      sprintf "Match (%s, [%s])"
        (aux t)
        (String.concat " ; " (List.map (fun (_p, t) -> "_ -> " ^ aux t) pts))
    | Trm_bbeis (t, p) -> sprintf "Is (%s, %s)" (aux t) (aux p)
    | Trm_patvar varid -> sprintf "PVar %s" (symbol_to_string varid.varid_symbol)
    | Trm_patwild -> sprintf "Wildcard"
  and aux i t =
    let space = String.make i ' ' in
    sprintf "{ trm =\n%s %s ;\n%s typ =\n%s %s }"
      space (pr_desc i t.trm_desc)
      space
      space (print_low_level_typ t.trm_typ) in
  aux 0

let print_low_level_bind (b : bind) =
  let open Printf in
  match b with
  | Bind_anon -> " _"
  | Bind_var (x, _) -> sprintf "Var %s" (print_var x)
  | Bind_register_instance _ -> "unsupported Binder for the moment"


let print_low_level_reginst insts =
  Printf.sprintf "registered instances <%i>" (List.length insts.candidates_and_modes_candidates)

(*wip : write a small printer of a program [typdef list] for debugging*)
let print_low_level_topdef (td : topdef) : string =
  let open Printf in
    match td.topdef_desc with
    | Topdef_val_def {let_def_bind = b; let_def_body = t} ->
      sprintf "Let_bind (%s ; %s)" (print_low_level_bind b) (print_low_level_trm t)
    | Topdef_typ_def { typ_def_td = tds ; _ } -> sprintf "Topdef_typ_def (%s)" ("unsupported for the moment")
    | Topdef_external { external_def_var = v ; _ } -> sprintf "Topdef_external (%s)" ("unsupported for the moment")

let print_low_level_program (p : program) : string =
  let s : string list = List.map print_low_level_topdef p in
  String.concat "\n \n" s

let env_add_tvar_rigid (x : tvar_rigid) (ty : typ) : unit =
  if_debug (fun () ->
    Printf.printf "%aenv_add_tvar_rigid %s : [%a]\n"
      print_indent ()
      (print_tvar_rigid x)
      print_typ ty)

let env_add_tconstr (x : tconstr) (ty : typ) : unit =
  if_debug (fun () ->
    Printf.printf "%aenv_add_tconstr %s : [%a]\n"
      print_indent ()
      (print_tconstr x)
      print_typ ty)

let env_add_item ~style (x : var) (it : env_item) (is_overloaded : bool) : unit =
  if_debug (fun () ->
    Printf.printf "%aenv_add %s%s : [%a]\n"
      print_indent ()
      (print_var x)
      (if is_overloaded then " overloaded" else "")
      (print_item ~style) it)

let typecheck_up_start ~style (annot : typ option) (t : trm) : unit =
  if_debug (fun () ->
    Printf.printf "%atypecheck up with expected type [%a]\n%a\n"
      print_indent ()
      print_typ_option annot
      (print_trm ~style) t)

let typecheck_end ~style (t : trm) : unit =
  if_debug (fun () ->
    Printf.printf "%aconclude [%a] is the type of:\n%a\n"
      print_indent ()
      print_typ (typ_of t)
      (print_trm ~style) t)

let typecheck_down_start ~style (typ_exp : typ) (t : trm) : unit =
  if_debug (fun () ->
    Printf.printf "%atypecheck down with type [%a]\n%a\n"
      print_indent ()
      print_typ typ_exp
      (print_trm ~style) t)


let print_env_debug env =
  Printf.sprintf "{ %s ; %s }"
    (String.concat " ; " (List.rev
      (Env.fold env.env_tconstr (fun l t td ->
        Printf.sprintf "%s: Type(%d)" (print_tconstr t) (List.length td.tconstr_tvars) :: l) [])))
    (String.concat " ; " (List.rev
      (Env.fold env.env_var (fun l x ->
        let x = symbol_to_string x in function
        | Env_item_var sch -> Printf.sprintf "%s: %s" x (sch_to_string sch) :: l
        | Env_item_overload _ -> (x ^ ": <overloaded>") :: l) [])))

let print_current_top_level_resolving varid =
  if_debug (fun () ->
    Printf.printf "Resolving varid: %s(%s) : %s.\n"
      (symbol_to_string varid.varid_symbol)
      (Var.print_varid_unique_int varid.varid_unique_int)
      (typ_to_string varid.varid_typ))

let print_current_top_level_resolved varid assumptions =
  if_debug (fun () ->
    Printf.printf "Resolved varid: %s(%s) : %s.\n"
      (symbol_to_string varid.varid_symbol)
      (Var.print_varid_unique_int varid.varid_unique_int)
      (typ_to_string varid.varid_typ) ;
    if assumptions <> [] then (
      Printf.printf "New varids created as its assumptions:\n" ;
      List.iter (fun vi ->
        Printf.printf "- %s(%s) : %s.\n"
        (symbol_to_string vi.varid_symbol)
        (Var.print_varid_unique_int vi.varid_unique_int)
        (typ_to_string vi.varid_typ)) assumptions
    ))

