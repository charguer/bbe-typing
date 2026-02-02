type ('a, 'b) assoclist =
  | Nil
  | Cons of 'a * 'b * ('a, 'b) assoclist
let rec lookup_table_opt k t =
  match t with
  | Cons (_x4, k, _x6) ->
    if (_x4 = k) then Some k
    else
      begin match t with
        | Cons (_x1, _x2, t') -> lookup_table_opt k t'
        | _ -> None
      end
  | _ ->
    begin match t with
      | Cons (_x1, _x2, t') -> lookup_table_opt k t'
      | _ -> None
    end

let (a_big_expression : _) = 2

let double_table_apply k1 t1 t2 f =
  match lookup_table_opt k1 t1 with
  | Some k2 ->
    begin match lookup_table_opt k2 t2 with
      | Some v -> f v
      | _ -> a_big_expression
    end
  | _ -> a_big_expression

let double_table_apply_v2 k1 t1 t2 f =
  match lookup_table_opt k1 t1 with
  | Some _x2 ->
    begin match lookup_table_opt _x2 t2 with
      | Some v -> f v
      | _ -> a_big_expression
    end
  | _ -> a_big_expression

let double_table_apply_v3 k1 t1 t2 f =
  match lookup_table_opt k1 t1 with
  | Some _x2 ->
    begin match lookup_table_opt _x2 t2 with
      | Some v -> f v
      | _ -> a_big_expression
    end
  | _ -> a_big_expression

let (table1 : _) = Cons (2, 200, (Cons (3, 300, Nil)))
let (table2 : _) = Cons (300, 1, (Cons (400, 0, Nil)))

let (double_table_application : _) =
  double_table_apply 3 table1 table2
    (fun v -> let _ = print_endline (string_of_int v) in 1)






