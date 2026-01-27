type ('a, 'b) assoclist =
  | Nil 
  | Cons of 'a * 'b * ('a, 'b) assoclist 
let rec (lookup_table_opt : _) =
  fun (k : int) ->
    fun (t : (int, int) assoclist) ->
      (match t with
       | Cons (_x4, _x5, _x6) ->
           let _x7 = (fun __arg1 -> __arg1 = k) _x4 in
           if _x7
           then let v = _x5 in Some v
           else
             (match t with
              | Cons (_x1, _x2, _x3) -> let t' = _x3 in lookup_table_opt k t'
              | _ -> if true then None else assert false)
       | _ ->
           (match t with
            | Cons (_x1, _x2, _x3) -> let t' = _x3 in lookup_table_opt k t'
            | _ -> if true then None else assert false) : int option)
let (a_big_expression : _) = 2
let (double_table_apply : _) =
  fun k1 ->
    fun t1 ->
      fun t2 ->
        fun f ->
          let _x3 = lookup_table_opt k1 t1 in
          match _x3 with
          | Some _x4 ->
              let k2 = _x4 in
              let _x1 = lookup_table_opt k2 t2 in
              (match _x1 with
               | Some _x2 -> let v = _x2 in f v
               | _ -> a_big_expression)
          | _ -> a_big_expression
let (double_table_apply_v2 : _) =
  fun k1 ->
    fun t1 ->
      fun t2 ->
        fun f ->
          let _x1 = lookup_table_opt k1 t1 in
          match _x1 with
          | Some _x2 ->
              let _x3 = (fun __arg1 -> lookup_table_opt __arg1 t2) _x2 in
              (match _x3 with
               | Some _x4 -> let v = _x4 in f v
               | _ -> a_big_expression)
          | _ -> a_big_expression
let (double_table_apply_v3 : _) =
  fun k1 ->
    fun t1 ->
      fun t2 ->
        fun f ->
          let _x1 = (fun __arg1 -> lookup_table_opt __arg1 t1) k1 in
          match _x1 with
          | Some _x2 ->
              let _x3 = (fun __arg2 -> lookup_table_opt __arg2 t2) _x2 in
              (match _x3 with
               | Some _x4 -> let v = _x4 in f v
               | _ -> a_big_expression)
          | _ -> a_big_expression
let (table1 : _) = Cons (2, 200, (Cons (3, 300, Nil)))
let (table2 : _) = Cons (300, 1, (Cons (400, 0, Nil)))
let (double_table_application : _) =
  double_table_apply 3 table1 table2
    (fun v -> let _ = print_endline (string_of_int v) in 1)
let (double_table_application_v2 : _) =
  double_table_apply_v2 3 table1 table2
    (fun v -> let _ = print_endline (string_of_int v) in 1)
let (double_table_application_v3 : _) =
  double_table_apply_v3 3 table1 table2
    (fun v -> let _ = print_endline (string_of_int v) in 1)