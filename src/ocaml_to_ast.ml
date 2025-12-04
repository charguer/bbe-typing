open Asttypes
open Parsetree
open Var
open Ast_fix
open Ast_aux


(* Documentation of parsetree in:
  https://v2.ocaml.org/releases/4.11/htmlman/compilerlibref/Parsetree.html *)

type ocaml_ast = Parsetree.structure

(*#########################################################################*)
(* ** Errors *)

let unsupported ?(loc=loc_none) (m : string) : 'a =
  prerr_endline (Printf.sprintf "🟪 Unsupported, %s: %s" (Ast_print.print_loc loc) m) ;
  exit 1

(*#########################################################################*)
(* ** Local variables *)

(* During the translation, we sometimes need to introduce local variables.
  In order for them to not shadow any other variable, we also need to know
  what are the variables declared up to now. *)

let (new_local_variable : unit -> Var.var), (variable_found : string -> unit) =
  let current = ref 0 in
  let prefix = "nameless_variable_" in
  let new_local_variable () =
    incr current ;
    var (Printf.sprintf "%s%i" prefix !current) in
  let variable_found v =
    if String.starts_with ~prefix v then
      try (
        let str = String.sub v (String.length v) (String.length v - String.length v) in
        let i = int_of_string str in
        current := max !current (i + 1)
      ) with Failure _int_of_string -> () in
  (new_local_variable, variable_found)

let var (x : string) : Var.var =
  variable_found x ;
  Var.var x

(*#########################################################################*)
(* ** Translate expressions *)

let coretype_to_syntyp ty = {
    syntyp_syntax = ty ;
    syntyp_typ = typ_nameless ()
  }

let coretype_to_synsch ty =
  let (tvars, ty) =
    match ty.ptyp_desc with
    | Ptyp_poly (tvars, ty) ->
      (List.map (fun x -> tconstr ("'" ^ x.txt)) tvars, ty)
    | _ -> ([], ty) in {
    synsch_syntax = (tvars, ty) ;
    synsch_sch = {
      sch_tvars = tvars ;
      sch_body = typ_nameless ()
    }
  }

let check_uniq ?loc fs : unit =
  let rec aux = function
  | [] | [_] -> ()
  | f1 :: f2 :: _ when f1 = f2 ->
    unsupported ?loc (Printf.sprintf "Field %s defined several times." f1)
  | _ :: fs -> aux fs in
  aux (List.sort compare fs)


let tr_longident (lid : Longident.t) : string =
  String.concat "." (Longident.flatten lid)

let tr_constant ?loc (c : constant) : cst =
  match c with
  | Pconst_integer (s, _) -> Cst_int (int_of_string s)
  | Pconst_float (s, _) -> Cst_float (float_of_string s)
  | Pconst_string (s, _, _) -> Cst_string s
  | Pconst_char _ -> unsupported ?loc "char"

let varsyntyp_of_var ?loc (x : var) : varsyntyp =
   (x, mk_syntyp_none ?loc ())

(* Translate a pattern in a place in which we only accept variables, or return [None]. *)
let tr_pat_to_var (p : pattern) : varsyntyp option =
  let loc = p.ppat_loc in
  match p.ppat_desc with
  | Ppat_var var_loc -> Some (varsyntyp_of_var ~loc (var var_loc.txt))
  | Ppat_constraint ({ppat_desc = Ppat_var var_loc; _}, cty) ->
    Some (var var_loc.txt, mk_syntyp cty)
  | Ppat_constraint ({ppat_desc = Ppat_any; _}, cty) ->
    Some (no_name_var (), mk_syntyp cty)
  | Ppat_any -> Some (varsyntyp_of_var ~loc (no_name_var ()))
  | Ppat_construct ({txt = Lident "()"; _}, _) ->
    let cty = mk_syntyp_unit () in
    Some (no_name_var (), { cty with syntyp_typ = the_typ_unit })
  | _ -> None

let rec tr_pat (p : pattern) : pat =
  (* The set of variables appearing in the pattern is also returned. *)
  let aux = tr_pat in
  let loc = p.ppat_loc in
  let pat_desc =
    match p.ppat_desc with
    | Ppat_any -> Pat_any
    | Ppat_var var_loc -> Pat_var (var var_loc.txt)
    | Ppat_alias (p, var_loc) -> Pat_alias (aux p, var var_loc.txt)
    | Ppat_constant c -> Pat_constant (tr_constant ~loc c)
    | Ppat_tuple ps ->
      add_tuple_arity (List.length ps) ;
      Pat_tuple (List.map aux ps)
    | Ppat_construct ({ txt = Lident "true" }, None) ->
        Pat_constant (Cst_bool true)
    | Ppat_construct ({ txt = Lident "false" }, None) ->
        Pat_constant (Cst_bool false)
    | Ppat_construct ({ txt = Lident "()" }, None) ->
        Pat_constant (Cst_unit ())
    | Ppat_construct (c, None) ->
        Pat_construct (constr (tr_longident c.txt), [])
    | Ppat_construct (c, Some (tys, p)) ->
        assert (tys = []) ; (* I'm not sure when this is triggered. I guess only with GADTs. *)
        let p = aux p in
        Pat_construct (constr (tr_longident c.txt), [p])
    | Ppat_or (p1, p2) -> Pat_or (aux p1, aux p2)
    | Ppat_constraint (p, ty) ->
        Pat_constraint (aux p, mk_syntyp ty)
    | _ -> unsupported ~loc "Unsupported pattern." in
  { pat_desc ; pat_loc = p.ppat_loc ; pat_typ = typ_nameless () }

(* unsupported
|	Ppat_interval of constant * constant
|	Ppat_variant of Asttypes.label * pattern option
|	Ppat_record of (Longident.t Asttypes.loc * pattern) list * Asttypes.closed_flag
|	Ppat_array of pattern list
|	Ppat_type of Longident.t Asttypes.loc
|	Ppat_lazy of pattern
|	Ppat_unpack of string option Asttypes.loc
|	Ppat_exception of pattern
|	Ppat_extension of extension
|	Ppat_open of Longident.t Asttypes.loc * pattern
*)

(* Add instance declarations at the beginning of a function body. *)
let add_local_instances local_instances body =
  List.fold_left (fun body ((x, ty), symbol, loc) ->
    trm_let_def ~loc {
        let_def_rec = Nonrecursive ;
        let_def_body = trm_annot ~loc (trm_var ~loc x) ty ;
        let_def_bind =
          Bind_register_instance (symbol, {
            instance_tvars = [] ;
            instance_assumptions = [] ;
            instance_typ = typ_nameless ()
          })
      } body) body (List.rev local_instances)

let payload_to_symbol loc = function
  | PStr [{ pstr_desc =
      Pstr_eval ({ pexp_desc = Pexp_ident { txt = Lident x ; _ } ; _ }, _) ; _ }] ->
    SymbolName (var x)
  | PPat ({ ppat_desc = Ppat_tuple ps }, None)
      when List.for_all (fun p -> p.ppat_desc = Ppat_any) ps ->
    let i = List.length ps in
    if i = 1 then unsupported ~loc "Expected number different than 1 here." ;
    SymbolTuple i
  | PTyp [%type: int] -> SymbolNumericInt
  | PTyp [%type: float] -> SymbolNumericFloat
  | PTyp [%type: string] -> SymbolString
  | PTyp [%type: unit] -> SymbolTuple 0
  | PTyp [%type: bool] -> SymbolBool
  | PPat ([%pat? Get [%p? { ppat_desc = Ppat_record ([(id, _)], Closed) }]], None) ->
    SymbolGetField (field (tr_longident id.txt))
  | PPat ([%pat? Set [%p? { ppat_desc = Ppat_record ([(id, _)], Closed) }]], None) ->
    SymbolSetField (field (tr_longident id.txt))
  | PPat ([%pat? Make [%p? { ppat_desc = Ppat_record (ps, Closed) }]], None) ->
    let fs = List.map (fun (id, _) -> field (tr_longident id.txt)) ps in
    let fs = List.sort compare fs in
    SymbolMakeRecord fs
  | PPat ([%pat? With [%p? { ppat_desc = Ppat_record ([(id, _)], Closed) }]], None) ->
    SymbolRecordWith (field (tr_longident id.txt))
  | _ -> unsupported ~loc "Invalid payload for an instance."

(* Given a list of attributes, extract the [@instance (symbol)] ones and return the
  list of corresponding symbols. *)
let filter_attributes_binding =
  List.filter_map (fun attr ->
    match attr.attr_name.txt with
    | "instance" | "register" ->
      (* In practise, the [@register (symbol)] attribute behaves very similarly
        to a [@instance (symbol)]—its only difference being that it can't introduce
        names, and that it's not meant to be available to the user. *)
      Some (payload_to_symbol attr.attr_loc attr.attr_payload)
    | str ->
      prerr_endline (Printf.sprintf "Warning: ignored attribute %s." str) ;
      None)

type argument_attribute =
  | Arg_Implicit of symbol
  | Arg_Instance of symbol

let rec pat_attributes p =
  p.ppat_attributes
  @ match p.ppat_desc with
    | Ppat_constraint (p, _) -> pat_attributes p
    | _ -> []

(* Process the argument attributes. *)
let filter_attributes_argument =
  List.concat_map (fun attr ->
    let loc = attr.attr_loc in
    match attr.attr_name.txt with
    | "implicit" -> [(Arg_Implicit (payload_to_symbol loc attr.attr_payload), loc)]
    | "instance" -> [(Arg_Instance (payload_to_symbol loc attr.attr_payload), loc)]
    | "implicit_instance" ->
      let symbol = payload_to_symbol loc attr.attr_payload in
      [(Arg_Implicit symbol, loc) ; (Arg_Instance symbol, loc)]
    | str ->
      prerr_endline (Printf.sprintf "Warning: ignored attribute %s." str) ;
      [])

(* Given a list of processed argument attribute, return either [None] if no [Arg_Implicit]
  is provided, or [Some] with the associated symbol if any. *)
let get_implicit_symbol attrs =
  List.fold_left (fun ret (attr, loc) ->
    match attr with
    | Arg_Implicit symbol ->
      begin match ret with
      | None -> Some symbol
      | Some _ -> unsupported ~loc "implicit argument associated to two separate symbols."
      end
    | Arg_Instance _ -> ret) None attrs

(* Given a list of processed argument attribute, return the list of local instances it associates
  with. *)
let get_all_local_instances attrs =
  List.filter_map (fun (attr, loc) ->
    match attr with
    | Arg_Implicit _ -> None
    | Arg_Instance sym -> Some (sym, loc)) attrs

let build_match_on_var ~loc (x : var) (cs : case list) : expression = {
    pexp_desc = Pexp_match ({
        pexp_desc = Pexp_ident { txt = Lident (print_var x) ; loc } ;
        pexp_loc = loc ;
        pexp_loc_stack = [] ;
        pexp_attributes = []
      }, cs) ;
    pexp_loc = loc ;
    pexp_loc_stack = [] ;
    pexp_attributes = []
  }

(* Extract from an expression its list of argument, of local instances, and the remaining expression. *)
let rec pexp_fun_get_annotated_args_body (e : expression)
    : (varsyntyps * (varsyntyp * symbol * loc) list * expression) option =
  let loc = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_fun (Nolabel, None, p, e1) ->
    begin match tr_pat_to_var p with
    | Some x ->
      (* Here x and xs are annotated variables. *)
      let attributes = pat_attributes p in
      let attrs = filter_attributes_argument attributes in
      if get_implicit_symbol attrs <> None then
        unsupported ~loc "implicit argument can only be defined in an explicit instance declaration." ;
      let local_instances =
        List.map (fun (sym, loc) -> (x, sym, loc)) (get_all_local_instances attrs) in
      begin match pexp_fun_get_annotated_args_body e1 with
      | None -> Some ([x], local_instances, e1)
      | Some (xs, insts, ei) -> Some (x :: xs, local_instances @ insts, ei)
      end
    | None ->
      (* We need to compile this function into a match expression. *)
      let x = new_local_variable () in
      let e = build_match_on_var ~loc x [{ pc_lhs = p ; pc_guard = None ; pc_rhs = e1}] in
      let x = varsyntyp_of_var ~loc x in
      Some ([x], [], e)
    end
  | Pexp_fun (_lbl, _eo, _p, _e1) -> unsupported ~loc "labeled arguments"
  | Pexp_function cs ->
    (* We compile it on the fly to a match expression. *)
    let x = new_local_variable () in
    let e = build_match_on_var ~loc x cs in
    let x = varsyntyp_of_var ~loc x in
    Some ([x], [], e)
  | _ -> None

(* FIXME: Do we still need them? *)
let ocaml_to_mode (e : expression) =
  match e.pexp_desc with
  | Pexp_construct ({txt = Lident b; _ }, _) ->
      if b = "true" then Mode_in
      else if b = "false" then Mode_out
      else invalid_arg "modes must be booleans litterals."
  | _ -> invalid_arg "modes must be booleans."

let rec ocaml_list_to_modes (e : expression) : mode list =
  match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "[]"; _ }, _) -> []
  | Pexp_construct ({ txt = Lident "::"; _ }, Some {pexp_desc = Pexp_tuple [e1; e2]; _ }) ->
      ocaml_to_mode e1 :: ocaml_list_to_modes e2
  | _ -> invalid_arg "the input must be a boolean list."

(* Given a term of the form [fun (type a b) -> t], return the list [a; b] and t. *)
let rec extract_leading_type_parameters t =
  match t.pexp_desc with
  | Pexp_newtype ({ txt = ty ; _ }, t) ->
    let ty = tconstr ty in
    let (tys, t) = extract_leading_type_parameters t in
    (ty :: tys, t)
  | _ -> ([], t)

(* Split a term representing a (non-polymorph) function into its arguments and its body. *)
let rec split_fun_args t =
  match t.pexp_desc with
  | Pexp_fun (Nolabel, None, p, t0) ->
    let loc = p.ppat_loc in
    begin match tr_pat_to_var p with
    | Some local_name ->
      let (args, t') = split_fun_args t0 in
      ((local_name, filter_attributes_argument (pat_attributes p)) :: args, t')
    | None ->
      (* We compile it on the fly to a match expression. *)
      let x = new_local_variable () in
      let e = build_match_on_var ~loc x [{ pc_lhs = p ; pc_guard = None ; pc_rhs = t0}] in
      let x = varsyntyp_of_var ~loc x in
      ([(x, [])], e)
    end
  | Pexp_fun (_lbl, _opt, _p, _t) -> unsupported ~loc:t.pexp_loc "labelled arguments." ;
  | Pexp_function cs ->
    (* We compile it on the fly to a match expression. *)
    let loc = t.pexp_loc in
    let x = new_local_variable () in
    let e = build_match_on_var ~loc x cs in
    let x = varsyntyp_of_var ~loc x in
    ([(x, [])], e)
  | _ -> ([], t)


let atsign_inv (e : expression) : bool =
  match e.pexp_desc with
    | Pexp_ident {txt= Longident.Lident "@"} -> true
    | _ -> false

let infix_op_inv ~loc (e1 : expression) (exp_list : (arg_label * expression) list) : (expression * expression * expression) option =
  if not (List.for_all (fun (lbl, _) -> lbl = Nolabel) exp_list)
    then begin
      match List.find_opt (fun (lbl, _) -> not (lbl = Nolabel)) exp_list with
      | Some (Labelled s, _) | Some (Optional s, _) -> unsupported ~loc (Printf.sprintf "labeled arguments : %s" s);
      | _ -> unsupported ~loc "infix_op_inv, unexpected behavior, branch should be impossible"
    end;
  match exp_list with
  | [(_, arg1); (_, arg2)] ->
    begin match arg2.pexp_desc with
      | Pexp_apply (op, [(_, arg21)]) ->
          Some (arg1, op, arg21)
      | _ -> None
    end
  | _ -> None

let recognize_infix_op ~loc (op : expression) (arg1 : trm) (arg2 : trm) : trm_desc =
  match op.pexp_desc with
  | Pexp_ident {txt = Longident.Lident s} ->
    begin match s with
    | "_is" -> trm_desc_bbeis arg1 arg2
    | _ -> unsupported ~loc "infix operation not yet handled"
    end
  | _ -> unsupported ~loc "operator is not an ident"


let pvar_inv (e : expression) : bool =
  match e.pexp_desc with
    | Pexp_ident {txt= Longident.Lident "??"} -> true
    | _ -> false

let rec tr_exp (e : expression) : trm =
  let loc = e.pexp_loc in
  let return ?(annot=AnnotNone) (e':trm_desc) : trm =
    { trm_desc = e';
      trm_loc = e.pexp_loc;
      trm_typ = typ_nameless ();
      trm_binds = None;
      trm_env = env_empty;
      trm_annot = annot } in
  match e.pexp_desc with
  | Pexp_ident lid_loc ->
    (*Goal : look at the identifier, and write different variables -> either the ident is optional, and give a pattern var, or it is a specific ident (here __ -> Trm_patwild) *)
    if lid_loc.txt = Longident.Lident "__" then
       return (trm_desc_patwild ())
    else return (trm_desc_var (var (tr_longident lid_loc.txt)))
  | Pexp_constant c ->
      let cst = tr_constant ~loc c in
      (* let (symbol, annot) =
        match cst with
        | Cst_int _ -> (SymbolNumericInt, AnnotLiteralInt)
        | Cst_float _ -> (SymbolNumericFloat, AnnotLiteralFloat)
        | Cst_string _ -> (SymbolString, AnnotLiteralString)
        | Cst_bool _ | Cst_unit _ ->
          assert false (* In OCaml, boolean and units are dealt as constructors and not constants. See Pexp_construct below. *) in *)
      return (trm_desc_cst cst)
  | Pexp_let (rf, [vb], e2) ->
      let lets = tr_let rf e.pexp_attributes vb in
      let t2 = tr_exp e2 in
      List.fold_left (fun t l ->
        trm_let_def ~loc l t) t2 (List.rev lets)
  | Pexp_let (rf, vbs, e) -> unsupported ~loc "mutual let defs"
  | Pexp_fun _ | Pexp_function _ ->
      let (xs, local_instances, e1) =
         match pexp_fun_get_annotated_args_body e with
         | None -> assert false
         | Some res -> res in
      let t1 = tr_exp e1 in
      let t2 = add_local_instances local_instances t1 in
      return (trm_desc_funs xs t2)
  | Pexp_ifthenelse (e1, e2, Some e3) ->
      let t1 = tr_exp e1 in
      let t2 = tr_exp e2 in
      let t3 = tr_exp e3 in
      return (Trm_if (t1, t2, t3))
  | Pexp_ifthenelse (e1, e2, None) ->
      let t1 = tr_exp e1 in
      let t2 = tr_exp e2 in
      return (Trm_if (t1, t2, trm_unit ()))
      (* LATER: trm_desc_fixs *)

  | Pexp_apply (e0, aes) when atsign_inv e0 ->
    begin match infix_op_inv ~loc e0 aes with
    | Some (e1, e2, e3) ->
      (* if !Flags.verbose && !Flags.debug then
        begin
          let pr = Printast.expression 0 in
          Format.printf "handling @ %a %a %a:\n" pr e1 pr e2 pr e3;
          (*make task go.sh with "make typer; typer.exe test/..."

          take as argument workspace folder + current path folder.
          *)
        end; *)
      let t1 = tr_exp e1 in
      let t3 = tr_exp e3 in
        return (recognize_infix_op ~loc e2 t1 t3)
    | _ -> unsupported ~loc "@-sign with no custom operator"
    end

  | Pexp_apply (e0, aes) when pvar_inv e0 ->
    begin match aes with
    | [(_, {pexp_desc = Pexp_ident lid_loc})] ->
      return (trm_desc_patvar (var (tr_longident lid_loc.txt)))
    | _ -> unsupported ~loc "pattern variable but wrong argument"
    end

  | Pexp_apply (e0, aes) ->
    let is_labeled lbl =
      match lbl with
      | Labelled _ -> true
      | _ -> false
    in
    let labels,es = List.split aes in
    if (List.exists is_labeled labels) then unsupported ~loc "labeled argument";
    let t0 = tr_exp e0 in
    let ts = List.map tr_exp es in
    return (trm_desc_apps t0 ts)

  | Pexp_sequence (e1, e2) ->
      let t1 = tr_exp e1 in
      let t2 = tr_exp e2 in
      return (trm_desc_seq t1 t2)
  | Pexp_constraint (e1, cty) ->
      let t1 = tr_exp e1 in
      return (Trm_annot (t1, mk_syntyp cty))
  | Pexp_newtype (ty, e1) ->  (* fun (type t) -> e *)
      (* [let x : 'a. 'a list = []] is encoded by ocaml as
         [let x : ty = []]  where ty is a type ['a list]
         and ['a] simply appears as a rigid variable. *)
      (* FIXME: Isn't it with [ty = Ptyp_poly ('a, 'a list)]? *)
      let t1 = tr_exp e1 in
      return (Trm_forall (tconstr ty.txt, t1))
  | Pexp_construct ({txt = Lident "()"; _}, _) ->
      let cst = Cst_unit () in
      return (trm_desc_cst cst)
  | Pexp_construct ({txt = Lident b; _}, _) when b = "true" || b = "false" ->
      let cst = Cst_bool (b = "true") in
      return (trm_desc_cst cst)

  | Pexp_construct (c, None) ->
      return (trm_desc_constr (constr (tr_longident c.txt)) [])
  | Pexp_construct (c, Some ({pexp_desc = Pexp_tuple ts; _})) ->
      add_tuple_arity (List.length ts) ;
      return (trm_desc_constr (constr (tr_longident c.txt)) (List.map tr_exp ts))
  | Pexp_construct (c, Some e) ->
      return (trm_desc_constr (constr (tr_longident c.txt)) [tr_exp e])

  (* | Pexp_tuple ts -> (* TODO: handle Pexp_tuple properly. No need to see this as a specific constructor. We just see a constructor as a label and a list of arguments applied to it. No need to worry. *)
      (* In the tuple case, there is conceptually an infinite number of instances: one for each arity.
       We can't add that many instances at once in the environment, so we add them lazily: each time
       we see a new tuple, we check whether its associated instance is already declared.
       This then enables users to define new overloaded instances of tuples. *)
      let i = List.length ts in
      add_tuple_arity i ;
      return ~annot:(AnnotTuple i) (trm_tuple_flex (List.map tr_exp ts)).trm_desc *)

(*   | Pexp_record (fs, None) ->
      let fs = List.map (fun (f, t) -> (tr_longident f.txt, t)) fs in
      check_uniq ~loc (List.map fst fs) ;
      let fs = List.map (fun (f, t) -> (field f, tr_exp t)) fs in
      let fs = List.sort (fun (f1, _t1) (f2, _t2) -> compare f1 f2) fs in
      return ~annot:AnnotRecordMake (trm_desc_apps
        (trm_var_symbol (SymbolMakeRecord (List.sort compare (List.map fst fs))))
        (List.map snd fs))
  | Pexp_record (fs, Some t) ->
      let fs = List.map (fun (f, t) -> (tr_longident f.txt, t)) fs in
      check_uniq ~loc (List.map fst fs) ;
      let fs = List.map (fun (f, t) -> (field f, tr_exp t)) fs in
      List.fold_left (fun t1 (f, t2) ->
        return ~annot:AnnotRecordWith (trm_desc_apps
          (trm_var_symbol (SymbolRecordWith f))
          [t1; t2])) (tr_exp t) fs
  | Pexp_field (t, f) ->
      let f = field (tr_longident f.txt) in
      return ~annot:AnnotRecordGet (trm_desc_apps
        (trm_var_symbol (SymbolGetField f))
        [tr_exp t])
  | Pexp_setfield (t1, f, t2) ->
      let f = field (tr_longident f.txt) in
      return ~annot:AnnotRecordSet (trm_desc_apps
        (trm_var_symbol (SymbolSetField f))
        [tr_exp t1; tr_exp t2])

 *)
   | Pexp_match (e, cs) ->
      return (trm_desc_match (tr_exp e) (List.map tr_case cs))

  (* TODO *)
  (* | Pexp_array
   * | Pexp_assert *)

  | _ -> unsupported ~loc "tr_exp_desc"


(* TODO
  let get_labeled_arg (name : string) (lblexps : (label * exp) list) : option exp
    List.assoc_opt

  let label_input = get_labeled_arg ...
  let inputs_opt = Option.map ocaml_list_to_modes label_input in
  trm_overload_new inputs_opt
*)

(* The let bindings can be normal declarations or instance declarations.
  In the case of a normal declarations, the returned list is a singleton.
  In the case where the let pattern-matches, it is compiled on the fly to a match-expression. *)
and tr_let (rf : rec_flag) (attrs : attributes) (vb : value_binding) : let_def list =
  let loc = vb.pvb_loc in
  let _symbols = filter_attributes_binding (attrs @ vb.pvb_attributes) in (*TODO : handling @pattern*)
  let (tvars, t) = extract_leading_type_parameters vb.pvb_expr in
  let (args, body) = split_fun_args t in
  let (implicits, non_implicits) =
    let rec aux acc = function
      | ([] : (varsyntyp * (argument_attribute * Ast_fix.loc) list) list) -> (List.rev acc, [])
      | ((x, ty), attrs) :: args ->
        match get_implicit_symbol attrs with
        | Some symbol -> aux ((x, symbol, ty) :: acc) args
        | None ->
          List.iter (fun (x, attrs) ->
            match get_implicit_symbol attrs with
            | Some _ ->
              unsupported ~loc "implicit argument after a non-implicit argument."
            | None -> ()) args ;
          (List.rev acc, (x, ty) :: List.map (fst : _ -> varsyntyp) args) in
    aux [] args in
  let local_instances =
    List.concat_map (fun (x, attrs) ->
      List.map (fun (sym, loc) -> (x, sym, loc)) (get_all_local_instances attrs)) args in
  let body = tr_exp body in
  let body = add_local_instances local_instances body in
  (* The variable [full_body] includes all parameters (including the implicit ones). *)
  let fun_body = trm_funs_if_non_empty ~loc non_implicits body in
  let full_body =
    trm_funs_if_non_empty ~loc (List.map (fun (x, _sym, ty) -> (x, ty)) implicits) fun_body in
  (* The variable [full_def] also includes the for-alls quantifiers. *)
  let full_def = trm_foralls ~loc tvars full_body in
  if _symbols = [] then ( (* TODO: remove symbols, and handle the attributes themselves. Without needing to filter anything basically. *)
    if implicits <> [] then
      unsupported ~loc "[@implicit] can only be used within an instance declaration." ;
    let bind =
      match vb.pvb_pat with
      | { ppat_desc = Ppat_var { txt = x ; _ } ; _ } ->
        Some (Bind_var (var x, None))
      | { ppat_desc = Ppat_constraint ({ ppat_desc = Ppat_var { txt = x ; _ } }, ty) ; _ } ->
        Some (Bind_var (var x, Some (coretype_to_synsch ty)))
      | [%pat? (_ : unit)] | [%pat? ()] -> Some (Bind_anon)
      | _ -> None in
    match bind with
    | Some bind -> [{
        (* This is a normal let-binding declaration. *)
        let_def_rec = rf ;
        let_def_body = full_def ;
        let_def_bind = bind
      }]
    | None ->
      (* This is a let-binding declaration with no instance, but compiled on the fly into a sequence
        of pattern-matching. *)
      let p = tr_pat vb.pvb_pat in
      let x = new_local_variable () in
      let xs = pat_vars p in
      let main_bind = {
        let_def_rec = Nonrecursive ;
        let_def_body =
          trm_foralls ~loc tvars (
            (* The typing may need to know the pattern to be able to type-check, and it thus
              has to appear in the same let-declaration. *)
            let y = new_local_variable () in
            trm_let ~loc rf (y, None) full_body
              (trm_match ~loc (trm_var y)
                [(p, trm_tuple_flex ~loc
                  (* We force the tuple type in order to prevent shadowing with overloaded instances. *)
                  ~typ:(typ_tuple (List.map (fun _x -> typ_nameless ()) xs))
                  (List.map (trm_var ~loc) xs))])
          ) ;
        let_def_bind = Bind_var (x, None)
      } in
      let p' = pat_tuple ~loc (List.map (pat_var ~loc) xs) in
      let later_bind x' = {
        let_def_rec = Nonrecursive ;
        let_def_body = trm_match ~loc (trm_var ~loc x) [(p', trm_var ~loc x')] ;
        let_def_bind = Bind_var (x', None)
      } in
      main_bind :: List.map later_bind xs
  ) else ( assert false;
    (* This let-binding is an instance declaration. *)
    (* let instance_typ = fun_body.trm_typ in
    let instance_sig = {
      instance_tvars = tvars ;
      instance_typ ;
      instance_assumptions =
        List.map (fun (_x, sym, ty) -> {
          assumption_symbol = sym ;
          assumption_typ = ty
        }) implicits
    } in
    let var_case x ty =
      let synsch =
        let syntax =
          match ty with
          | None -> [%type: _]
          | Some sty -> sty in {
          synsch_syntax = (tvars, syntax) ;
          synsch_sch = {
            sch_tvars = tvars ;
            sch_body = instance_typ
          }
        } in
      let x = var x in
      let regular_let = {
          let_def_rec = rf ;
          let_def_bind = Bind_var (x, Some synsch) ;
          let_def_body = full_def
        } in
      let register_lets =
        List.map (fun symbol -> {
          let_def_rec = Nonrecursive ;
          let_def_body = trm_var ~loc ~typ:full_body.trm_typ x ;
          let_def_bind = Bind_register_instance (symbol, instance_sig)
        }) symbols in
      regular_let :: register_lets in
    match vb.pvb_pat.ppat_desc with
    | Ppat_var { txt = x ; _ } -> var_case x None
    | Ppat_constraint ({ ppat_desc = Ppat_var { txt = x ; _ } }, sty) -> var_case x (Some sty)
    | Ppat_any ->
      (* No name is given to the expression: it is just a [let[@instance (+)] _ = ...], which
        is directly compiled into a [let[@register <sig>] _ = ...]. *)
      List.map (fun symbol -> {
        let_def_rec = Nonrecursive ;
        let_def_body = full_body ;
        let_def_bind = Bind_register_instance (symbol, instance_sig)
      }) symbols
    | _ -> unsupported ~loc "Only variable and wildcard pattern are accepted here." *)
  )

and tr_case c =
  match c.pc_guard with
  | Some e -> unsupported ~loc:e.pexp_loc "when clauses within pattern-matching."
  | None -> (tr_pat c.pc_lhs, tr_exp c.pc_rhs)


and tr_structure (s : structure) : topdefs =
  List.concat_map tr_structure_item s

and tr_structure_item (si : structure_item) : topdef list =
  let loc = si.pstr_loc in
  let get_expected_error (attrs : attributes) : expected_error * attributes =
    let (attrs_error, attrs) =
      List.partition (fun attr -> attr.attr_name.txt = "type_error") attrs in
    let error =
      match attrs_error with
      | [] -> None
      | [{ attr_payload = PStr [{ pstr_desc = Pstr_eval
          ({ pexp_desc = Pexp_constant (Pconst_string (message, _, _)) }, _); _ }] }] ->
        Some message
      | [_] -> unsupported ~loc "Unsupported format for expected error."
      | _ -> unsupported ~loc "More than one [@type_error] annotation at the same point." in
    (error, attrs) in
  let (expected_error, si_desc) =
    match si.pstr_desc with
    | Pstr_primitive vd ->
      let (error, attrs) = get_expected_error vd.pval_attributes in
      (error, Pstr_primitive { vd with pval_attributes = attrs })
    | Pstr_value (rf, vb :: vbs) ->
      let (error, attrs) = get_expected_error vb.pvb_attributes in
      begin match List.find_opt (fun vb -> fst (get_expected_error vb.pvb_attributes) <> None) vbs with
      | None -> ()
      | Some _ ->
        unsupported ~loc "In case of a type error in mutually recursive definitions, put the [@type_error] annotation first."
      end ;
      (error, Pstr_value (rf, { vb with pvb_attributes = attrs } :: vbs))
    | Pstr_type (rf, td :: tds) ->
      let (error, attrs) = get_expected_error td.ptype_attributes in
      begin match List.find_opt (fun td -> fst (get_expected_error td.ptype_attributes) <> None) tds with
      | None -> ()
      | Some _ ->
        unsupported ~loc "In case of a type error in mutually-defined types, put the [@type_error] annotation first."
      end ;
      (error, Pstr_type (rf, { td with ptype_attributes = attrs } :: tds))
    | _ -> (None, si.pstr_desc) in
  List.map
    (fun topdef_desc -> { topdef_desc; topdef_loc = loc; topdef_expected_error = expected_error })
    (tr_structure_item_desc si_desc)

and tr_structure_item_desc (sid : structure_item_desc) : topdef_desc list =
  match sid with
  | Pstr_value (rf, [vb]) ->
    let lets = tr_let rf [] vb in
    List.map topdef_desc_val_let_def lets
  | Pstr_value (rf, []) -> assert false
  | Pstr_value (rf, vb :: vbs) -> unsupported ~loc:vb.pvb_loc "mutually recursive definitions"
  | Pstr_primitive vd ->
    [topdef_desc_external (var vd.pval_name.txt) (coretype_to_synsch vd.pval_type) vd.pval_prim]
  | Pstr_type (rf, tds) -> [topdef_desc_typ_def rf tds]
  | Pstr_typext e -> unsupported ~loc:e.ptyext_loc "type extension"
  | Pstr_exception e -> unsupported ~loc:e.ptyexn_loc "exception"
  | Pstr_extension ((l, _), _) -> unsupported ~loc:l.loc "extension"
  | Pstr_eval (e, _) -> unsupported ~loc:e.pexp_loc "eval"
  | Pstr_attribute a -> unsupported ~loc:a.attr_loc "attribute"
  | _ -> unsupported "tr_structure_item_desc"



(*

Later:
  pour récupérer les listes de la forme [x1;x2...;xn],
  il faut récupérer du parsetree des objets construits avec :

  - Pexp_construct(... (Lident "::") ..., Some args)
  - (Lident "[]")

  et faire l'inversion jusqu'au nil.

*)

