external print_endline : string -> unit = ""

external string_of_int : int -> string = ""

type ('a, 'b) assoclist = Nil | Cons of 'a * 'b * ('a, 'b) assoclist

let rec lookup_table_opt : (int, ((int, int) assoclist)) -> (int option) = fun k t -> (switch [
__case (t @_is Cons (fun __arg1 -> ( = ) __arg1 k) (??v) (__) (* ~> {v : int} *)) (* ~> {v : int} *) @_then Some v;
__case (t @_is Cons (__) (__) (??t') (* ~> {t' : (int, int) assoclist} *)) (* ~> {t' : (int, int) assoclist} *) @_then lookup_table_opt k t';
__case true (* ~> {} *) @_then None
] : int option (* syntactically was int option *))

let a_big_expression : int = 2

let double_table_apply : (int, ((int, int) assoclist), ((int, int) assoclist), (int -> int)) -> int = fun k1 t1 t2 f -> if (lookup_table_opt k1 t1 @_is Some (??k2) (* ~> {k2 : int} *)) (* ~> {k2 : int} *) && (lookup_table_opt k2 t2 @_is Some (??v) (* ~> {v : int} *)) (* ~> {v : int} *) (* ~> {v : int ; k2 : int} *) then f v else a_big_expression

let double_table_apply_v2 : (int, ((int, int) assoclist), ((int, int) assoclist), (int -> int)) -> int = fun k1 t1 t2 f -> if (lookup_table_opt k1 t1 @_is Some ((fun __arg1 -> lookup_table_opt __arg1 t2) (??v)) (* ~> {v : int} *)) (* ~> {v : int} *) then f v else a_big_expression

let double_table_apply_v3 : (int, ((int, int) assoclist), ((int, int) assoclist), (int -> int)) -> int = fun k1 t1 t2 f -> if (k1 @_is (fun __arg1 -> lookup_table_opt __arg1 t1) ((fun __arg2 -> lookup_table_opt __arg2 t2) (??v)) (* ~> {v : int} *)) (* ~> {v : int} *) then f v else a_big_expression

let table1 : (int, int) assoclist = Cons ((2,200,Cons ((3,300,Nil))))

let table2 : (int, int) assoclist = Cons ((300,1,Cons ((400,0,Nil))))

let double_table_application : int = double_table_apply 3 table1 table2 (fun v -> print_endline (string_of_int v);
1)

let double_table_application_v2 : int = double_table_apply_v2 3 table1 table2 (fun v -> print_endline (string_of_int v);
1)

let double_table_application_v3 : int = double_table_apply_v3 3 table1 table2 (fun v -> print_endline (string_of_int v);
1)