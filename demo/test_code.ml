external print_endline : string -> unit = ""
external string_of_int : int -> string = ""

type ('a, 'b) assoclist =
    Nil
  | Cons of 'a * 'b * ('a, 'b) assoclist

(* Remove later if we do not talk about smart constructors *)
(* let nil = Nil

let cons a b t = Cons (a, b, t)
let __pattern_cons t = (* should be automatically generated *)
  __switch [
    __case ((t @_is (Cons (??x, ??y, ??t'))) @_then (Some (x, y, t')));
    __case (true @_then None);
  ] *)

let rec lookup_table_opt (k : int) (t : (int, int) assoclist) : int option =
  __switch [
    __case ((t @_is (Cons ((?!(__ = k)), ??v, __))) @_then (Some v));
    __case ((t @_is (Cons (__, __, ??t'))) @_then (lookup_table_opt k t'));
    __case (true @_then None);
  ]

let a_big_expression = 2

let double_table_apply k1 t1 t2 f =
  if   (lookup_table_opt k1 t1) @_is (Some ??k2)
    && (lookup_table_opt k2 t2) @_is (Some ??v)
  then f v
  else a_big_expression


let double_table_apply_v2 k1 t1 t2 f =
  if (lookup_table_opt k1 t1) @_is (Some (?!(lookup_table_opt __ t2) ??v))
  then f v
  else a_big_expression

let double_table_apply_v3 k1 t1 t2 f =
  if k1 @_is (?!(lookup_table_opt __ t1) (?!(lookup_table_opt __ t2) ??v)) then f v
  else a_big_expression

let table1 = Cons (2, 200, Cons (3, 300, Nil))
let table2 = Cons (300, 1, Cons (400, 0, Nil))

let double_table_application =
  double_table_apply 3 table1 table2 (fun v -> (print_endline (string_of_int v); 1))

let double_table_application_v2 =
  double_table_apply_v2 3 table1 table2 (fun v -> (print_endline (string_of_int v); 1))

let double_table_application_v3 =
  double_table_apply_v3 3 table1 table2 (fun v -> (print_endline (string_of_int v); 1))









































(* command:
./typer.exe unit_test_debug.ml; ocamlc unit_test_debug_expanded.ml; ./a.out *)


(*
(* Feature focus 2 *)
external a_big_expression : int = ""
external list_mem : int list -> int -> bool = ""
external list_get_opt : int list -> int -> int option = ""

let ocaml_hashtable_get1 t r f =
  match r with
  | Some k when list_mem t k ->
    let v = list_get_opt t k in
    f v
  | _ ->
    a_big_expression

let ocaml_hashtable_get2 t r f =
  let cont () =
    a_big_expression in
  match r with
  | Some k ->
    begin match list_get_opt t k with
      | Some v -> f v
      | _ -> cont ()
    end
  | _ ->
    cont ()


(* TODO YL: replace with 2 get opts, with two tables, in all of the above examples  *)
let hashtable_get h1 h2 k1 f =
  if (r @_is (Some ??k2)) && ((list_get_opt t k2) @_is (Some ??v))
    then f v
    else a_big_expression


 *)