type ('a, 'b) bucket =
    Nil
  | Cons of 'a * 'b * ('a, 'b) bucket

let rec lookup_table_opt (t : (int, int) bucket) (k : int) : int option =
  __switch [
    __case ((t @_is (Cons ((?!(__ = k)), ??v, __))) @_then (Some v));
    __case ((t @_is (Cons (__, __, ??t'))) @_then (lookup_table_opt t' k));
    __case (true @_then None);
  ]

let a_big_expression = 0

let double_table_apply t1 t2 k1 f =
  if   (lookup_table_opt t1 k1) @_is (Some ??k2)
    && (lookup_table_opt t2 k2) @_is (Some ??v)
  then f v
  else a_big_expression

let double_table_apply_v2 t1 t2 k1 f =
  if (lookup_table_opt t1 k1) @_is (Some (?!(lookup_table_opt t2 __) ??v))
  then f v
  else a_big_expression

let double_table_apply_v3 t1 t2 k1 f =
  if k1 @_is (?!(lookup_table_opt t1 __) (?!(lookup_table_opt t2 __) ??v)) then f v
  else a_big_expression

let () =
  let table1 = Cons (2, 200, Cons (3, 300, Nil)) in
  let table2 = Cons (300, 1, Cons (400, 2, Nil)) in

  print_endline (string_of_int (double_table_apply table1 table2 3 (fun x -> x)));
  print_endline (string_of_int (double_table_apply_v2 table1 table2 3 (fun x -> x)));
  print_endline (string_of_int (double_table_apply_v3 table1 table2 3 (fun x -> x)));

  ()




(* Command for PPX: dune build; dune exec ./test_code.exe *)






































(* command:
../typer.exe test_code.ml; ocamlc test_code_expanded.ml; ./a.out *)