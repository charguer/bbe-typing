open Var
open Ast_fix
open Ast_aux
open PPrint
open Printf
open Tools

let debug_binds = true
let debug_binds_verbose = false

type style_types =
  | TypesSubterms
  | TypesVarsAndBinders
  | TypesVars
  | TypesVarsOverloaded
  | TypesVarsOverloadedUnresolved
  | TypesNone

type style_resolution =
  | ResolutionInstanceOrSymbol
  | ResolutionInstanceOrError
  | ResolutionInstanceOrObjMagic
  | ResolutionSymbol

type style_debug =
  | DebugNone
  | DebugResolving of varid
  | DebugResolved of varid

type style_binds =
  | BindsNone
  | BindsToplevel
  | BindsAll

type style = {
  style_types : style_types ;
  style_resolution_full : style_resolution ;
  style_resolution_base : style_resolution ;
  style_resolution_args : style_resolution ;
  style_debug : style_debug ;
  style_print_symbols : bool ;
  style_binds : style_binds
}

(* Printing styles for error messages. *)
let style_debug = {
  style_types = TypesVarsOverloadedUnresolved ;
  style_resolution_full = ResolutionSymbol ;
  style_resolution_base = ResolutionSymbol ;
  style_resolution_args = ResolutionSymbol ;
  style_debug = DebugNone ;
  style_print_symbols = true ;
  style_binds = BindsNone
}


type doc = document

let comment = enclose (lparen ^^ star ^^ blank 1) (blank 1 ^^ star ^^ rparen)

(* Note: useful templates may be found in the file
   ocaml/parsing/pprintast.ml *)

let code_print_width = 100

let indent = ref 1

let print_indent ppf () : unit =
  fprintf ppf "%s" (String.make (2 * !indent) '=')


(* A local version of Repr.get_repr that doesn't check on cycles. *)
let rec repr t =
  match t.typ_desc with
  | Unified t0 -> repr t0
  | _ -> t


(*#########################################################################*)
(* ** Print locations *)

let print_loc (l : loc) = (* TODO: cleanup code ; todo get filename *)
  if Location.is_none l then
    "<internal location>"
  else
    let (file1, line1, char1) = Location.get_pos_info l.loc_start in
    let (file2, line2, char2) = Location.get_pos_info l.loc_end in
    let normalise_file f = if f = "" then "<input>" else f in
    let file1 = normalise_file file1 in
    let file2 = normalise_file file2 in
    if file1 = file2 then
      if line1 = line2 then
        if char1 = char2 then
          sprintf "%s:%d, char %d" file1 line1 char1
        else
          sprintf "%s:%d, chars %d-%d" file1 line1 char1 char2
      else
        sprintf "%s:%d, char %d to line %d char %d"
          file1 line1 char1 line2 char2
    else
      sprintf "%s:%d, char %d to file %s, line %d char %d"
        file1 line1 char1 file2 line2 char2


(*#########################################################################*)
(* ** Print attributes *)

let letter_like c =
  c = '_'
  || (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')

(* Heuristic for whether a type name should be in infix or prefix position. *)
let infix_type t =
  if t = "" then true
  else (
    let c = t.[0] in
    List.mem t ["asr"; "land"; "lor"; "lsl"; "lsr"; "lvor"; "mod"; "or"]
    || not (letter_like c)
  )

(* A heuristic to check whether a symbol is a constructor (in such case, one can't
  print-out its type without OCaml complaining). *)
let is_constructor x = (* = function
  | SymbolName x -> *)
    let x = print_var x in
    if x = "" then true (* Looks like the Rocq-trick to print tuples. *)
    else (
      match x.[0] with
      | 'A'..'Z' -> true
      | 'a'..'z' -> false
      | _ ->
        match x with
        | "::" | "[]" -> true
        | _ -> false
    )(*
  | SymbolTuple _ -> true
  | SymbolMakeRecord _ -> true
  | _ -> false *)

(* Print a variable, adding parentheses on non-letter identifiers. *)
let print_var_parens x =
  let x = print_var x in
  if infix_type x then
    sprintf "( %s )" x
  else x

(* Same than [print_var_parens], but with constructors. *)
let print_constr_parens x =
  let x = print_constr x in
  if infix_type x then
    sprintf "( %s )" x
  else x

(* let symbol_to_attr_doc = function
  | SymbolName x -> blank 1 ^^ string (print_var_parens x)
  | SymbolTuple 0 -> string ":" ^^ blank 1 ^^ string "unit"
  | SymbolTuple i -> blank 1 ^^ string (string_of_int i)
  | SymbolNumericInt -> string ":" ^^ blank 1 ^^ string "int"
  | SymbolNumericFloat -> string ":" ^^ blank 1 ^^ string "float"
  | SymbolString -> string ":" ^^ blank 1 ^^ string "string"
  | SymbolBool -> string ":" ^^ blank 1 ^^ string "bool"
  | SymbolGetField f ->
       string "?" ^^ blank 1 ^^ string "Get" ^^ blank 1
    ^^ string "{" ^^ blank 1 ^^ string (print_field f) ^^ blank 1 ^^ string "}"
  | SymbolSetField f ->
       string "?" ^^ blank 1 ^^ string "Set" ^^ blank 1
    ^^ string "{" ^^ blank 1 ^^ string (print_field f) ^^ blank 1 ^^ string "}"
  | SymbolMakeRecord fs ->
       string "?" ^^ blank 1 ^^ string "Make" ^^ blank 1
    ^^ string "{" ^^ blank 1
    ^^ separate (blank 1 ^^ semi ^^ blank 1) (List.map (fun f -> string (print_field f)) fs)
    ^^ blank 1 ^^ string "}"
  | SymbolRecordWith f ->
       string "?" ^^ blank 1 ^^ string "With" ^^ blank 1
    ^^ string "{" ^^ blank 1 ^^ string (print_field f) ^^ blank 1 ^^ string "}"
 *)

(*#########################################################################*)
(* ** Print types *)

let print_tvar_rigid = print_tconstr

let print_styp (t : styp) : string =
  let buffer = Buffer.create 16 in
  let fo = Format.formatter_of_buffer buffer in
  Pprintast.core_type fo t ;
  Format.pp_print_flush fo () ;
  Buffer.contents buffer

let print_tvars_arg (tvars : tvar_rigid list) =
  if tvars = [] then empty
  else (
       string "fun"
    ^^ blank 1
    ^^ parens (
         string "type"
      ^^ blank 1
      ^^ separate (blank 1) (List.map (fun a -> string (print_tconstr a)) tvars)
      )
    ^^ blank 1
    ^^ string "->"
    ^^ blank 1
  )

(*#########################################################################*)
(* ** Print terms *)


let cst_to_doc c =
  match c with
  | Cst_bool b -> string (if b then "true" else "false")
  | Cst_int n when n < 0 -> parens (string (string_of_int n))
  | Cst_int n -> string (string_of_int n)
  | Cst_float f -> string (string_of_float f)
  | Cst_unit () -> string "()"
  | Cst_string s -> dquotes (string (String.escaped s))

(* let mode_to_string = function
  | Mode_in -> "true"
  | Mode_out -> "false"
 *)
(* let modes_to_string ms =
  sprintf "~input:[%s]" (String.concat "; " (List.map mode_to_string ms))
 *)
(* let modes_io_to_string (ms, mr) =
  sprintf "%s ~output:[%s]" (modes_to_string ms) (mode_to_string mr)
 *)
let var_to_string (x : var) = print_var_parens x

let var_to_doc (x : var) : doc =
  string (var_to_string x)

let put_parens_trm (t : trm) (d : doc) : doc =
  match t.trm_desc with
  | Trm_cst _
  | Trm_var _
  | Trm_annot _ -> d
  | _ -> parens d

(* let symbol_to_string = function
  | SymbolName x -> var_to_string x
  | SymbolTuple i -> sprintf "(*internal: tuple*)__tuple_%i" i
  | SymbolNumericInt -> "(*internal: int*)__numeric_without_dot"
  | SymbolNumericFloat -> "(*internal: float*)__numeric_with_dot"
  | SymbolString -> "(*internal: string*)__double_quote"
  | SymbolBool -> "(*internal: bool*)__truth_value"
  | SymbolGetField f -> "(*internal: record get field*)__get_" ^ print_field f
  | SymbolSetField f -> "(*internal: record set field*)__set_" ^ print_field f
  | SymbolMakeRecord fs ->
    "(*internal: record building*)__make_"
    ^ String.concat "_" (List.sort compare (List.map print_field fs))
  | SymbolRecordWith f -> "(*internal: record with*)__with_" ^ print_field f
 *)
(* let symbol_to_string_message = function
  | SymbolName x -> var_to_string x
  | SymbolTuple i -> sprintf "(%s)" (String.concat "," (List.init i (fun _ -> ".")))
  | SymbolNumericInt -> "<int>"
  | SymbolNumericFloat -> "<float>"
  | SymbolString -> "<string>"
  | SymbolBool -> "<bool>"
  | SymbolGetField f -> "get:" ^ print_field f
  | SymbolSetField f -> "set:" ^ print_field f
  | SymbolMakeRecord fs ->
    "make:" ^ String.concat "," (List.sort compare (List.map print_field fs))
  | SymbolRecordWith f -> "with:" ^ print_field f
 *)
(* let print_symbol sym = string (symbol_to_string sym)
 *)
let pop_last lst =
  match List.rev lst with
  | x :: xs -> (List.rev xs, x)
  | [] -> assert false (* only called when the lst has at least 2 arguments *)

let rec typ_to_doc (t : typ) : doc =
  match t.typ_desc with
  | Flexible v -> string (print_tvar v)
  | Unified t0 -> typ_to_doc t0
  (* TODO "adding tuple type" : write an inversor for this... For some reason OCaml does not have the information that tconstr is a string *)
(*   | Typ_constr ("tuple", ts) ->
       put_parens (List.map typ_to_doc ts) *)
  | Typ_constr (x, ts) ->
      let x = print_tconstr x in
      match ts with
      | [] -> string x
      | t0 :: [] ->
            put_parens t0
         ^^ blank 1
         ^^ string x
      | ts when not (infix_type x) ->
          parens (separate (string "," ^^ blank 1) (List.map typ_to_doc ts))
          ^^ blank 1
          ^^ string x
      | t0 :: t1 :: [] -> (* so x is infix *)
             put_parens t0
          ^^ blank 1
          ^^ string x
          ^^ blank 1
          ^^ put_parens t1
      | ts ->
        if x = "->" then
          let (args, result) = pop_last ts in
        parens (separate (comma ^^ blank 1) (List.map put_parens args)) ^^ blank 1 ^^ string x ^^ blank 1 ^^ (put_parens result)
        else
        separate (blank 1 ^^ string x ^^ blank 1) (List.map put_parens ts)

and put_parens (t : typ) : doc =
  let d = typ_to_doc t in
  let ty = repr t in
  match ty.typ_desc with
  | Unified _ -> assert false
  | Flexible _
  | Typ_constr (_, []) -> d
  | _ -> parens d

(* and instance_to_doc ~style (i : instance) : doc =
     parens (trm_to_doc ~style i.instance_value)
     ^^
       if contains_flexible i.instance_sig.instance_typ then empty
       else (
         string " : "
         ^^ sch_to_doc {
           sch_tvars = i.instance_sig.instance_tvars ;
           sch_body = i.instance_sig.instance_typ }
       )
 *)
(* and overload_to_doc ~style (is : candidates_and_modes) : doc =
  candidates_to_doc ~style is.candidates_and_modes_candidates
 *)
(* and candidates_to_doc ~style (is : candidates) : doc =
     string "overloaded["
  ^^ hardline ^^ blank 4
  ^^ separate (hardline ^^ blank 2 ^^ string "|" ^^ blank 1) (List.map (instance_to_doc ~style) is)
  ^^ blank 1 ^^ string "]"
 *)
and sch_to_doc (sch : sch) : doc =
  let sch =
    let (typ, vars) =
      List.fold_left (fun (typ, acc) x ->
        let str = print_tvar_rigid x in
        assert (String.length str > 0) ;
        if str.[0] = '\'' then (typ, x :: acc)
        else (
          (* Such a type would not be syntactically accepted by OCaml. *)
          let x' = tconstr (sprintf "'%s" str) in
          let typ' = replace_rigid_with x (typ_constr x' []) typ in
          (typ', x' :: acc)
        )
      ) (sch.sch_body, []) sch.sch_tvars in
    { sch_tvars = List.rev vars ; sch_body = typ } in
  (
    if sch.sch_tvars <> [] then (
      separate (blank 1) (List.map (fun v -> string (print_tvar_rigid v)) sch.sch_tvars)
      ^^ string "."
      ^^ blank 1
    ) else empty
  ) ^^ typ_to_doc sch.sch_body

and varid_to_doc ~style ?(is_arg = false) varid : doc =
(*   let is_resolved =
    match varid.varid_resolution with
    | VarRegular | VarResolved _ -> true
    | VarUnknown | VarUnresolved _ -> false in
  let is_overloaded =
    match varid.varid_resolution with
    | VarRegular -> false
    | _ -> true in
  let rec is_full_resolved varid =
    match varid.varid_resolution with
    | VarRegular -> true
    | VarResolved (_value, assumptions) -> List.for_all is_full_resolved assumptions
    | VarUnknown | VarUnresolved _ -> false in
  let is_debug_resolving =
    match style.style_debug with
    | DebugResolving varid' when varid'.varid_unique_int = varid.varid_unique_int -> true
    | _ -> false in
  let is_debug =
    match style.style_debug with
    | (DebugResolving varid' | DebugResolved varid')
      when varid'.varid_unique_int = varid.varid_unique_int -> true
    | _ -> false in
  let d =
    match
      if is_debug_resolving then ResolutionInstanceOrSymbol
      else if is_arg then style.style_resolution_args
      else if is_full_resolved varid then style.style_resolution_full
      else style.style_resolution_base with
    | ResolutionSymbol -> print_symbol varid.varid_symbol
    | (ResolutionInstanceOrSymbol
      | ResolutionInstanceOrError
      | ResolutionInstanceOrObjMagic) when is_resolved ->
      begin match varid.varid_resolution with
      | VarRegular -> print_symbol varid.varid_symbol
      | VarResolved (instance, assumptions) ->
           trm_to_doc ~style instance.instance_value
        ^^ blank 1
        ^^ separate (blank 1) (List.map (varid_to_doc ~style ~is_arg:true) assumptions)
      | _ -> assert false
      end
    (* Now that we have checked the resolved case, every [ResolutionInstanceOr...] are in the
      unresolved case. *)
    | ResolutionInstanceOrSymbol ->
      let list_candidates =
        match varid.varid_resolution with
        | VarUnresolved is ->
             blank 1
          ^^ string "(*"
          ^^ candidates_to_doc ~style is.candidates_and_modes_candidates
          ^^ blank 1
          ^^ string "*)"
        | VarUnknown -> empty
        | _ -> assert false in
      print_symbol varid.varid_symbol ^^ list_candidates
    | ResolutionInstanceOrError ->
      prerr_endline (sprintf "Unresolved overloaded symbol: %s (%s)."
        (symbol_to_string varid.varid_symbol)
        (print_loc varid.varid_loc)) ;
      exit 1
    | ResolutionInstanceOrObjMagic -> string "Obj.magic ()" in
  if (is_debug_resolving || not (is_constructor varid.varid_symbol))
     && (List.mem style.style_types [TypesVars; TypesVarsAndBinders]
         || ((style.style_types = TypesVarsOverloaded || is_debug) && is_overloaded)
         || (style.style_types = TypesVarsOverloadedUnresolved && not is_resolved)) then
    parens (
         d
      ^^ blank 1
      ^^ string ":"
      ^^ blank 1
      ^^ typ_to_doc varid.varid_typ)
  else d *)
  var_to_doc varid

and varid_to_doc_slim varid : doc = var_to_doc varid

and varid_to_string_slim v = doc_to_string (varid_to_doc_slim v)

(* This is a hack because of the order of definition. TODO (low priority) : clean this in a better order, and especially out of the mutual recursion *)
and sch_to_string ty = doc_to_string (sch_to_doc ty)

and env_to_string ~style (e : env) : string =
  (* let print_val =
  function
    | Env_item_var sch -> sch_to_string sch
    | Env_item_overload is -> (doc_to_string (overload_to_doc ~style is))
  in *)
  Env.print (var_to_string) (sch_to_string) e.env_var

and binds_to_doc ~style (binds : env option) : doc =
  comment (
     string "~>"
  ^^ blank 1
  ^^ braces (
    string (Option.fold binds ~none:("") ~some:(env_to_string ~style)))
  )

and with_bindings ~style (t : trm) : doc =
  trm_to_doc_raw ~style t
  ^^ blank 1
  ^^ (binds_to_doc ~style t.trm_binds)


and trm_to_doc ~style t =
  let d = trm_to_doc_raw ~style t in
  if style.style_types = TypesSubterms then (
    parens (d ^^ show_typ_of_trm ~style t)
  ) else d

and trm_to_doc_raw ~style (t : trm) : doc =
  let aux = trm_to_doc_raw ~style in
  let auxs = List.map aux in
(*   let match_trm_apps =
    match t.trm_desc with
    | Trm_apps (t0, ts) -> Some (t0.trm_desc, ts)
    | _ -> None in
 *)
(*
  match style.style_print_symbols, t.trm_annot, match_trm_apps with

  | false, AnnotLiteralUnit,
    Some (Trm_var { varid_symbol = SymbolTuple 0 ; _ },
          [{ trm_desc = Trm_cst (Cst_unit ()); _ }]) ->
      string "()"

  | false, AnnotLiteralBool,
    Some (Trm_var { varid_symbol = SymbolBool ; _ },
          [{ trm_desc = Trm_cst (Cst_bool b); _ }]) ->
      string (if b then "true" else "false")

  | false, AnnotLiteralInt,
    Some (Trm_var { varid_symbol = SymbolNumericInt ; _ },
          [{ trm_desc = Trm_cst (Cst_int n); _ }]) ->
      cst_to_doc (Cst_int n)

  | false, AnnotLiteralFloat,
    Some (Trm_var { varid_symbol = SymbolNumericFloat ; _ },
          [{ trm_desc = Trm_cst (Cst_float f); _ }]) ->
      string (string_of_float f)

  | false, AnnotLiteralString,
    Some (Trm_var { varid_symbol = SymbolString ; _ },
          [{ trm_desc = Trm_cst (Cst_string str); _ }]) ->
      string (sprintf "%S" str)

  | false, AnnotRecordGet,
    Some (Trm_var { varid_symbol = SymbolGetField f ; _ }, [t1]) ->
         put_parens_trm t1 (aux t1)
      ^^ string "."
      ^^ string (print_field f)

  | false, AnnotRecordSet,
    Some (Trm_var { varid_symbol = SymbolSetField f ; _ }, [t1; t2]) ->
         put_parens_trm t (aux t1)
      ^^ string "."
      ^^ string (print_field f)
      ^^ blank 1
      ^^ string "<-"
      ^^ blank 1
      ^^ aux t2

  | false, AnnotRecordWith,
    Some (Trm_var { varid_symbol = SymbolRecordWith f ; _ }, [t1; t2]) ->
         string "{"
      ^^ blank 1
      ^^ aux t1
      ^^ blank 1
      ^^ string "with"
      ^^ blank 1
      ^^ string (print_field f)
      ^^ blank 1
      ^^ string "="
      ^^ blank 1
      ^^ aux t2
      ^^ blank 1
      ^^ string "}"

  | false, AnnotRecordMake,
    Some (Trm_var { varid_symbol = SymbolMakeRecord fs ; _ }, ts) ->
         string "{"
      ^^ blank 1
      ^^ separate (blank 1 ^^ semi ^^ blank 1) (List.map2 (fun f t ->
            string (print_field f)
         ^^ blank 1
         ^^ string "="
         ^^ blank 1
         ^^ aux t) fs ts)
      ^^ blank 1
      ^^ string "}"

  | false, AnnotTuple i,
    Some (Trm_var { varid_symbol = SymbolTuple j ; _ }, ts) when i = j ->
      parens (separate (comma ^^ blank 1) (List.map aux ts))
  | _, _, _ ->
*)
  match t.trm_desc with
  | Trm_cst c -> cst_to_doc c

  | Trm_var varid -> varid_to_doc ~style varid

  | Trm_funs (xs_raw, t1) ->
    (* fun (x1 : ty1) ... : tyr -> t *)
    let xs = List.map fst xs_raw in
    let xs = List.map print_var_parens xs in
    let args =
      if List.mem style.style_types [TypesSubterms; TypesVarsAndBinders; TypesVars] then begin
        let ty = repr (typ_of t) in
        match typ_arrow_inv_opt ty with
        | None ->
          (* This is typically executed when debugging. Falling back the syntactic types. *)
          separate (blank 1) (List.map (fun (x, sty) ->
            parens (
                  string (print_var_parens x)
                  (* OCaml interprets ['a] variables as rigid instead of flexible when in function
                    parameters. *)
              ^^ if sty.syntyp_syntax.ptyp_desc = Ptyp_any then empty
                  else (
                      blank 1
                  ^^ string ":"
                  ^^ blank 1
                  ^^ syntyp_to_doc sty
                  ))) xs_raw)
        | Some (tys, tyr) ->
          if List.length xs <> List.length tys
            then failwith "incorrect arity for function";
          (* This prints the type of argument according to the inferred type of the function,
            and ignores the provided styp of arguments. *)
              separate (blank 1) (List.map2 var_and_typ_to_doc tys xs)
          ^^ blank 1
          ^^ string ":"
          ^^ blank 1
          ^^ parens (typ_to_doc tyr)
      end else begin
        separate (blank 1) (List.map string xs)
      end in
    separate (blank 1) [
      string "fun";
      args;
      string "->";
      aux t1
    ]

  | Trm_if (t0, t1, t2) ->
      let s0 =
        if style.style_binds <> BindsNone then (* In this case, the style is either "BindsToplevel" or "BindsAll" *)
          with_bindings ~style t0
        else aux t0
      in
         string "if"
      ^^ blank 1
      ^^ s0
      ^^ blank 1
      ^^ string "then"
      ^^ hardline
      ^^ aux t1
      ^^ hardline
      ^^ string "else"
      ^^ hardline
      ^^ aux t2

  | Trm_let ({ let_def_bind = Bind_anon; let_def_body = { trm_desc = Trm_funs (xs, t1); _ }; _ }, t2) ->
      failwith "functions cannot appear as first argument of a trm_seq"

  | Trm_let ({ let_def_bind = Bind_var f; let_def_body = { trm_desc = Trm_funs (xs, t1); _ }; let_def_rec }, t2) ->
      let isrec =
        match let_def_rec with
        | Recursive -> true
        | Nonrecursive -> false in
          string (if isrec then "let rec" else "let")
      ^^ blank 1
      ^^ var_to_doc (fst f) (* VERY LATER: snd *)
      ^^ blank 1
      ^^ separate (blank 1) (List.map (fun (x, sty) ->
          parens (
            string (print_var_parens x)
            ^^ blank 1 ^^ string ":" ^^ blank 1
            ^^ syntyp_to_doc sty)) xs)
      ^^ show_typ_of_trm ~style ~is_binder:true t1
      ^^ blank 1
      ^^ string "="
      ^^ blank 1
      ^^ aux t1
      ^^ blank 1
      ^^ string "in"
      ^^ hardline
      ^^ aux t2

  | Trm_let ({ let_def_bind = Bind_anon; let_def_body = t1; _ }, t2) ->
        aux t1 ^^ semi ^^ hardline ^^ aux t2

  | Trm_let ({ let_def_bind = Bind_var (x, bind_sch); let_def_body = t1; let_def_rec }, t2) ->
      let isrec =
        match let_def_rec with
        | Recursive -> true
        | Nonrecursive -> false in
          string (if isrec then "let rec" else "let")
      ^^ blank 1
      ^^ var_to_doc x
      ^^ blank 1
      ^^ show_synsch_or (
          if List.mem style.style_types [TypesSubterms; TypesVarsAndBinders] then string ": _"
          else empty
          ) bind_sch
      ^^ blank 1
      ^^ string "="
      ^^ blank 1
      ^^ wrap_typ_of_trm ~style ~is_binder:true t1
      ^^ blank 1
      ^^ string "in"
      ^^ hardline
      ^^ aux t2

  (* | Trm_let ({ let_def_bind = Bind_register_instance (sym, inst); let_def_body = t1; _ }, t2) ->
          string "let"
      ^^ string "[@register"
      ^^ symbol_to_attr_doc sym
      ^^ string "]"
      ^^ blank 1
      ^^ string "_"
      ^^ blank 1
      ^^ string "="
      ^^ blank 1
      ^^ print_tvars_arg inst.instance_tvars
      ^^ parens (
        if inst.instance_assumptions = [] then (
          wrap_typ_of_trm ~style ~is_binder:true t1
        ) else (
            string "fun"
        ^^ blank 1
        ^^ print_assumptions_arg inst.instance_assumptions
        ^^ blank 1
        ^^ string ":"
        ^^ blank 1
        ^^ parens (typ_to_doc inst.instance_typ)
        ^^ blank 1
        ^^ string "->"
        ^^ blank 1
        ^^ wrap_typ_of_trm ~style ~is_binder:true t1
        ))
      ^^ blank 1
      ^^ string "in"
      ^^ hardline
      ^^ aux t2 *)

  | Trm_apps (t0, ts) ->
      let d0 = aux t0 in
      let d0 = put_parens_trm t0 d0 in
      let ds = auxs ts in
      let ds = List.map2 put_parens_trm ts ds in
      separate (blank 1) (List.map2 (trm_and_typ_to_doc ~style) (t0::ts) (d0::ds))

  | Trm_annot (t0, ty) ->
      parens (
            aux t0
        ^^ blank 1
        ^^ string ":"
        ^^ blank 1
        ^^ syntyp_to_doc ty)
  | Trm_forall (n, t1) ->
      let (n, t1) =
        (* Sometimes a rigid variable's name starts with a quote, but such variable can't appear
          in a [fun (type _)] construct in Ocaml.  In such places, we replace it on the fly. *)
        let str = print_tconstr n in
        assert (String.length str > 0) ;
        if str.[0] = '\'' then
          let n' =
            tconstr (sprintf "rigid_%s (* was %s *)"
              (String.sub str 1 (String.length str - 1)) str) in
          (n', trm_map_typ (replace_rigid_with n (typ_constr n' [])) t1)
        else (n, t1) in
          string "fun (type"
        ^^ blank 1
        ^^ string (print_tconstr n)
        ^^ string ")"
        ^^ blank 1
        ^^ string "->"
        ^^ blank 1
        ^^ parens (aux t1)

  | Trm_match (t, pts) ->
          string "begin match"
        ^^ blank 1
        ^^ aux t
        ^^ blank 1
        ^^ string "with"
        ^^ hardline
        ^^ separate hardline
            (List.map (fun (p, t) ->
                  blank 2
              ^^ string "|"
              ^^ blank 1
              ^^ pat_to_doc p
              ^^ blank 1
              ^^ string "->"
              ^^ blank 1
              ^^ aux t) pts)
        ^^ hardline
        ^^ string "end"

  | Trm_tuple ts -> (* TODO: check if this is the correct result. *)
    parens (separate comma
      (List.map aux ts))
  | Trm_not t0 ->
      let d =
        if style.style_binds = BindsAll then
          with_bindings ~style t0 else
        aux t0
      in
         string "not"
      ^^ blank 1
      ^^ d
  | Trm_and (t1 ,t2) ->
    let d1 =
        if style.style_binds = BindsAll then
          with_bindings ~style t1 else
        aux t1
      in
    let d2 =
        if style.style_binds = BindsAll then
          with_bindings ~style t2 else
        aux t2
      in
         d1
      ^^ blank 1
      ^^ string "&&"
      ^^ blank 1
      ^^ d2
  | Trm_or (t1 ,t2) ->
    let d1 =
      if style.style_binds = BindsAll then
        with_bindings ~style t1 else
      aux t1
    in
    let d2 =
      if style.style_binds = BindsAll then
        with_bindings ~style t2 else
      aux t2
    in
        d1
    ^^ blank 1
    ^^ string "||"
    ^^ blank 1
    ^^ d2
  | Trm_switch cases ->
    let case_to_doc (b1, t2) =
      let d1 =
        if style.style_binds = BindsAll then
          with_bindings ~style b1 else
        aux b1
      in
      let d2 = aux t2 in
         string "_case"
      ^^ blank 1
      ^^ d1
      ^^ blank 1
      ^^ string "@_then"
      ^^ blank 1
      ^^ d2
    in

       string "switch"
    ^^ blank 1
    ^^ brackets (
      separate (semi ^^ hardline)
      (List.map case_to_doc cases)
    )


  | Trm_while (b1, t2) ->
    (* has the form: [while "e1" do "e2" done] *)
    let d1 =
      if style.style_binds <> BindsNone then
        with_bindings ~style b1 else
      aux b1
    in
    let d2 = aux t2 in
       string "while"
    ^^ blank 1
    ^^ parens d1
    ^^ blank 1
    ^^ string "do"
    ^^ hardline
    ^^ blank 2
    ^^ d2
    ^^ hardline
    ^^ string "done"

  | Trm_bbe_is (t1, p2) ->
      let d1 = aux t1 in
      let d2 =
        if style.style_binds = BindsAll then
        with_bindings ~style p2
        else aux p2
      in
      parens (
            d1
        ^^ blank 1
        ^^ string "@_is"
        ^^ blank 1
        ^^ d2
      )

  | Trm_pat_var varid ->
    (* if style.style_binds = BindsAll then
       string "??"
    ^^ varid_to_doc ~style varid
    ^^ blank 1
    ^^ comment (
         string "~>"
      ^^ blank 1
      ^^ braces (
           varid_to_doc ~style varid
        ^^ blank 1
        ^^ string ":"
        ^^ blank 1
        ^^ string "_"
      )
    )
    else *)
       string "??"
    ^^ varid_to_doc ~style varid

  | Trm_pat_wild ->
    (* if style.style_binds = BindsAll then
       string "__"
    ^^ blank 1
    ^^ binds_to_doc ~style None
    else *)
    string "__"

  | Trm_pat_when (p, b) ->
    let d1 =
      if style.style_binds = BindsAll then
        with_bindings ~style p
      else aux p
    in
    let d2 =
      if style.style_binds = BindsAll then
        with_bindings ~style b
      else aux b
    in
    parens (
          d1
      ^^ blank 1
      ^^ string "@_when"
      ^^ blank 1
      ^^ d2
    )


and var_and_typ_to_doc (ty : typ) (x : string) =
  parens (
       string x
    ^^ blank 1
    ^^ string ":"
    ^^ blank 1
    ^^ typ_to_doc ty)

and show_typ_of_trm ~style ?(is_binder=false) (t : trm) =
  let is_forall =
    (* Due to the way forall quantifiers are dealt, printing as-is their type here would lead to
     terms refused by OCaml. *)
    match t.trm_desc with
    | Trm_forall _ -> true
    | _ -> false in
  if not is_forall
    && (style.style_types = TypesSubterms
        || (is_binder && style.style_types = TypesVarsAndBinders)) then (
       blank 1
    ^^ string ":"
    ^^ blank 1
    ^^ typ_to_doc t.trm_typ
  ) else empty

and wrap_typ_of_trm ~style ?(is_binder=false) (t : trm) =
  let d = trm_to_doc ~style t in
  let is_forall =
    (* Due to the way forall quantifiers are dealt, printing as-is their type here would lead to
     terms refused by OCaml. *)
    match t.trm_desc with
    | Trm_forall _ -> true
    | _ -> false in
  if not is_forall
  && (style.style_types = TypesSubterms
      || (is_binder && style.style_types = TypesVarsAndBinders)) then
    parens (d ^^ show_typ_of_trm ~style ~is_binder t)
  else d

(* and print_assumptions_arg assumptions =
   separate (blank 1) (List.map (fun asmpt ->
     parens (
       string "_"
    ^^ string "[@implicit"
    ^^ symbol_to_attr_doc asmpt.assumption_symbol
    ^^ string "]"
    ^^ blank 1
    ^^ string ":"
    ^^ blank 1
    ^^ syntyp_to_doc asmpt.assumption_typ
  )) assumptions) *)

and show_sch_or d = function
  | None -> d
  | Some sch -> string ":" ^^ blank 1 ^^ sch_to_doc sch

and show_synsch_or d = function
  | None -> d
  | Some synsch ->
    (* OCaml doesn't like type scheme with free variables. *)
    if contains_flexible synsch.synsch_sch.sch_body then d
    else (string ":" ^^ blank 1 ^^ sch_to_doc synsch.synsch_sch)

and trm_and_typ_to_doc ~style (t : trm) (p : doc) : doc =
  if style.style_types = TypesSubterms then parens (p ^^ show_typ_of_trm ~style t)
  else p

and syntyp_to_doc sty =
     typ_to_doc sty.syntyp_typ
  ^^ blank 1
  ^^ string "(*"
  ^^ blank 1
  ^^ string "syntactically was"
  ^^ blank 1
  ^^ string (print_styp sty.syntyp_syntax)
  ^^ blank 1
  ^^ string "*)"

and pat_to_doc (p : pat) : doc =
  let aux = pat_to_doc in
  match p.pat_desc with
  | Pat_any -> string "_"
  | Pat_var x -> var_to_doc x
  | Pat_alias (p, x) ->
         parens (aux p)
      ^^ blank 1
      ^^ string "as"
      ^^ blank 1
      ^^ var_to_doc x
  | Pat_constant c -> cst_to_doc c
  | Pat_tuple ps ->
      parens (separate (string "," ^^ blank 1) (List.map aux ps))
  | Pat_construct (c, []) ->
      string (print_constr_parens c)
  | Pat_construct (c, ps) ->
      string (print_constr_parens c)
      ^^ blank 1
      ^^ parens (separate (string "," ^^ blank 1) (List.map aux ps))
  | Pat_constraint (p, ty) ->
      parens (
         aux p
      ^^ blank 1
      ^^ string ":"
      ^^ blank 1
      ^^ syntyp_to_doc ty)
  | Pat_or (p1, p2) ->
         aux p1
      ^^ blank 1
      ^^ string "|"
      ^^ blank 1
      ^^ aux p2


let varid_to_string ~style x = doc_to_string (varid_to_doc ~style x)

let typ_to_string ty = doc_to_string (typ_to_doc ty)

let syntyp_to_string sty = doc_to_string (syntyp_to_doc sty)

let print_typ ppf ty : unit =
  output_string ppf (typ_to_string ty)

let print_typ_option ppf (ty_opt : typ option) : unit =
  match ty_opt with
  | None -> output_string ppf "_ (* no type *)"
  | Some ty -> print_typ ppf ty


(*#########################################################################*)
(* ** Print topdef and global printing *)

let topdef_to_doc ~style (td : topdef) : doc =
  (
    match td.topdef_expected_error with
    | None -> empty
    | Some err ->
         string "(*"
      ^^ blank 1
      ^^ string "meant to fail with error"
      ^^ blank 1
      ^^ string (sprintf {|"%s"|} (String.escaped err))
      ^^ blank 1
      ^^ string "*)"
      ^^ hardline
  ) ^^
  match td.topdef_desc with
  | Topdef_val_def { let_def_bind = Bind_anon ; let_def_body = t } ->
         string "let"
      ^^ blank 1
      ^^ string "_"
      ^^ blank 1
      ^^ string "="
      ^^ blank 1
      ^^ wrap_typ_of_trm ~style ~is_binder:true t
(*   | Topdef_val_def { let_def_bind = Bind_register_instance (sym, inst) ; let_def_body = t } ->
         string "let"
      ^^ string "[@register"
      ^^ symbol_to_attr_doc sym
      ^^ string "]"
      ^^ blank 1
      ^^ string "_"
      ^^ blank 1
      ^^ string "="
      ^^ blank 1
      ^^ print_tvars_arg inst.instance_tvars
      ^^ parens (
        if inst.instance_assumptions = [] then (
          wrap_typ_of_trm ~style ~is_binder:true t
        ) else (
           string "fun"
        ^^ blank 1
        ^^ print_assumptions_arg inst.instance_assumptions
        ^^ blank 1
        ^^ string ":"
        ^^ blank 1
        ^^ parens (typ_to_doc inst.instance_typ)
        ^^ blank 1
        ^^ string "->"
        ^^ blank 1
        ^^ wrap_typ_of_trm ~style ~is_binder:true t
        )
      ) *)

  | Topdef_val_def { let_def_rec = rf ; let_def_bind = Bind_var (x, schopt) ; let_def_body = t } ->
      let isrec =
        match rf with
        | Recursive -> true
        | Nonrecursive -> false
        in
         string (if isrec then "let rec" else "let")
      ^^ blank 1
      ^^ var_to_doc x
      ^^ blank 1
      ^^ show_synsch_or (
           if List.mem style.style_types [TypesSubterms; TypesVarsAndBinders] then string ": _"
           else empty
         ) schopt
      ^^ blank 1
      ^^ string "="
      ^^ blank 1
      ^^ wrap_typ_of_trm ~style ~is_binder:true t
  | Topdef_typ_def { typ_def_td = tds ; typ_def_typs = tcds ; _ } ->
    let print_typedecl tcd td =
      let open Parsetree in
      let params =
        let xs =
          match tcd with
          | Some tcd -> List.map (fun v -> string (print_tvar_rigid v)) tcd.tconstr_tvars
          | None ->
            List.map (fun (sty, _) ->
                 string "(* syntactic *)"
              ^^ blank 1
              ^^ string (print_styp sty)) td.ptype_params in
        match xs with
        | [] -> empty
        | [p] -> p ^^ blank 1
        | ps -> parens (separate (comma ^^ blank 1) ps) ^^ blank 1 in
      let def =
        let print_record lds =
             string "{"
          ^^ blank 1
          ^^ separate (blank 1 ^^ semi ^^ blank 1)
              (List.map (fun ld ->
                string "mutable"
                ^^ blank 1
                ^^ string ld.pld_name.txt
                ^^ blank 1
                ^^ string ":"
                ^^ blank 1
                ^^ string (print_styp (ld.pld_type))
              ) lds)
          ^^ blank 1
          ^^ string "}" in
        match td.ptype_kind, td.ptype_manifest with
        | Ptype_abstract, None -> empty
        | Ptype_abstract, Some c ->
             blank 1
          ^^ string "="
          ^^ blank 1
          ^^ string (print_styp c)
        | Ptype_variant cs, None ->
             blank 1
          ^^ string "="
          ^^ blank 1
          ^^ separate (blank 1 ^^ string "|" ^^ blank 1)
               (List.map (fun c ->
                 let ty_arg =
                   if c.pcd_args = Pcstr_tuple [] then empty
                   else (
                        blank 1
                     ^^ string "of"
                     ^^ blank 1
                     ^^ match c.pcd_args with
                        | Pcstr_tuple [] -> string "unit"
                        | Pcstr_tuple tys ->
                          separate (blank 1 ^^ string "*" ^^ blank 1)
                            (List.map (fun ty -> string (print_styp ty)) tys)
                        | Pcstr_record lds -> print_record lds
                   ) in
                 let name = c.pcd_name.txt in
                 let name = if name = "::" then "(::)" else name in
                 string name ^^ ty_arg) cs)
        | Ptype_record lds, None ->
             blank 1
          ^^ string "="
          ^^ blank 1
          ^^ print_record lds
        | _, _ -> blank 1 ^^ string "(* TODO: printing *)" in
      params ^^ string td.Parsetree.ptype_name.txt ^^ def in
    string "type"
    ^^ blank 1
    ^^ separate (hardline ^^ string "and" ^^ blank 1) (
      if tcds = [] then List.map (print_typedecl None) tds
      else List.map2 (fun tcd -> print_typedecl (Some tcd)) tcds tds
    )
  | Topdef_external { external_def_var = n ; external_def_syntyp = sty ; external_def_def = prim } ->
         string "external"
      ^^ blank 1
      ^^ var_to_doc n
      ^^ blank 1
      ^^ string ":"
      ^^ blank 1
      ^^ (
        let xs = fst sty.synsch_syntax in
        if xs = [] then empty
        else (
            separate (blank 1) (List.map (fun x -> string (print_tvar_rigid x)) xs)
         ^^ string "."
         ^^ blank 1
        ))
      ^^ string (print_styp (snd sty.synsch_syntax))
      ^^ blank 1
      ^^ string "="
      ^^ blank 1
      ^^ separate (blank 1) (List.map (fun s -> dquotes (string s)) prim)

let topdef_to_string ~style td = doc_to_string (topdef_to_doc ~style td)

let ast_to_doc ~style (tds : topdefs) : doc =
  separate (hardline ^^ hardline) (List.map (topdef_to_doc ~style (*env_builtin*)) tds)

let to_outchannel ~style (out : out_channel) (ast : Ast_fix.program) : unit =
  ToChannel.pretty 0.9 code_print_width out (ast_to_doc ~style ast)

let to_file ~style (filename : string) (ast : Ast_fix.program) : unit =
  let out =
    if filename = "-" then stdout
    else open_out filename in
  to_outchannel ~style out ast;
  close_out out

let to_string ~style (ast : Ast_fix.program) : string =
  let out = Buffer.create 16 in
  ToBuffer.pretty 0.9 code_print_width out (ast_to_doc ~style ast) ;
  Buffer.contents out

(*#########################################################################*)
(* ** Printing for debug *)

let trm_to_stdout ~style (t : trm) : unit =
  doc_to_stdout (trm_to_doc ~style t)

let print_trm ~style ppf (t : trm) : unit =
  doc_to_out ppf (trm_to_doc ~style t)

let print_item ~style ppf (s : sch) : unit =
  doc_to_out ppf (sch_to_doc s)

(* let insts_to_string ~style (insts : candidates_and_modes) : string =
  doc_to_string (overload_to_doc ~style insts) *)

(* let instance_to_string ~style (i : instance) : string =
  doc_to_string (instance_to_doc ~style i) *)

(* LATER: if needed a vector
   https://www.lri.fr/~filliatr/ftp/ocaml/ds/vector.mli.html *)

let print_modif (m : typ * typ_desc) : unit =
  let ty, td = m in
  print_string ((typ_to_string ty) ^ "," ^ (typ_to_string {typ_desc = td; typ_mark = no_mark}))

let rec print_modifs (ms : (typ * typ_desc) list) : unit =
  match ms with
  | [] -> print_newline ()
  | m :: ms -> print_string "("; print_modif m; print_string ") "; print_modifs ms

let trm_to_string ~style t =
  doc_to_string (trm_to_doc ~style t)

let pat_to_string t =
  doc_to_string (pat_to_doc t)

