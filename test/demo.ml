let __print = 0
external float_of_int : int -> float = "%floatofint"
external ( < ) : 'a -> 'a -> bool = "%lt"
external ( = ) : 'a -> 'a -> bool = "%equal"
external int_add : int -> int -> int = "%addint"
external float_add : float -> float -> float = "%addfloat"

let __numeric_without_dot = __instance (fun (x : int) : int -> x)
let __numeric_without_dot = __instance (fun (x : int) : float -> float_of_int x)
let __numeric_with_dot = __instance (fun (x : float) : float -> x)
let __empty_parentheses = __instance (fun (x : unit) : unit -> x)
let __double_quote = __instance (fun (x : string) : string -> x)
let __truth_value = __instance (fun (b : bool) : bool -> b)
let (+) = __instance int_add
let (+) = __instance float_add


(**)

(* let __debug = -1 *)

(* ENCODING WITHOUT PAIRS
let ex11 = (fun x -> let a = x in let b = x + (1:int) in a)

 *)


(* TODO: fails because the trm_foralls_inv seems limited
let f (r:int) = fun (type a) (x:a) -> x *)
(*
*)

type 'a list = unit (* dummy *)
external nil : 'a list = "%nil"
external cons : 'a -> 'a list -> 'a list = "%cons"
let exlist1 : int list = cons 3 (cons 2 nil)
let exlist2 = cons 3 (cons (2:int) nil)
let exlist3 = cons 3 (cons 2 (nil:float list))

external list_length : 'a list -> int = "%listlength"
let length = __instance list_length
let length = __instance (fun (x:int) -> x)

let len1 = length (3:int)
let len2 = length exlist3
let len3 = length (3:int) + length exlist3

let listmap = __overload ~input:[false;true]
external listmap : ('a -> 'b) -> 'a list -> 'b list = "%listmap"
let map = __instance listmap
let map = __instance (fun (f:int->int) (x:int) -> x)

let succ (n:int) = n + 1
let mp1 = map succ (3:int)
let mp2 = map float_of_int exlist2

(* TODO/ try to see if "apply" is ambiguous or not with listmap; it shouldn't *)


external int_mul : int -> int -> int = "%int_mul"
external int_div : int -> int -> int = "%int_div"
external int_exp : int -> int -> int = "%int_exp"

external float_exp : float -> float -> float = "%float_exp"
external float_mul : float -> float -> float = "%float_mul"
external float_div : float -> float -> float = "%float_div"

type complex = unit (* dummy *)

external complex_of_float : float -> complex = ""

external complex_add : complex -> complex -> complex = "%complex_add"
external complex_exp : complex -> complex -> complex = "%complex_exp"
external complex_mul : complex -> complex -> complex = "%complex_mul"
external complex_div : complex -> complex -> complex = "%complex_div"

external i : complex = ""

type 'a matrix = unit (* dummy *)
external matrix_int_mul : int -> int matrix -> int matrix = ""
external matrix_float_mul : float -> float matrix -> float matrix = ""
external matrix_complex_mul : complex -> complex matrix -> complex matrix = ""
(*
external matrix_add : 'a matrix -> 'a matrix -> 'a matrix = ""
external matrix_mul : 'a matrix -> 'a matrix -> 'a matrix = ""
external matrix_exp : 'a matrix -> int -> 'a matrix = "%matrix_exp" *)

external matrix_mul_int : int matrix -> int matrix -> int matrix = ""
external matrix_mul_float : float matrix -> float matrix -> float matrix = ""
external matrix_mul_complex : complex matrix -> complex matrix -> complex matrix = ""

external matrix_exp_int : int matrix -> int -> int matrix = ""
external matrix_exp_float : int matrix -> int -> int matrix = ""
external matrix_exp_complex : complex matrix -> int -> complex matrix = ""

external matrix_zero : complex matrix = ""
external matrix_add_complex : complex matrix -> complex matrix -> complex matrix = ""



let __numeric_without_dot = __instance (fun (x : int) : complex -> complex_of_float (float_of_int x))
let __numeric_with_dot = __instance (fun (x : float) : complex -> complex_of_float x)

external pi_val : float = ""
let pi = __instance pi_val
let pi = __instance (complex_of_float pi_val)
external e_val : float = ""
let e = __instance e_val
let e = __instance (complex_of_float e_val)
(* alternative: coercion *)

let (+) = __instance complex_add
let (+) = __instance matrix_add_complex

let ( * ) = __instance int_mul
let ( * ) = __instance float_mul
let ( * ) = __instance complex_mul
(* let ( * ) = __instance matrix_mul *)

let ( * ) = __instance matrix_mul_int
let ( * ) = __instance matrix_mul_float
let ( * ) = __instance matrix_mul_complex




let ( * ) = __instance matrix_int_mul
let ( * ) = __instance matrix_float_mul
let ( * ) = __instance matrix_complex_mul

let ( / ) = __instance int_div
let ( / ) = __instance float_div
let ( / ) = __instance complex_div

let ( ^ ) = __instance int_exp
let ( ^ ) = __instance float_exp
let ( ^ ) = __instance complex_exp
(*
let ( ^ ) = __instance matrix_exp
*)
let ( ^ ) = __instance matrix_exp_int
let ( ^ ) = __instance matrix_exp_float
let ( ^ ) = __instance matrix_exp_complex

(* DEPRECATED
external int_sum_list : 'a list -> ('a -> int) -> int = ""
external float_sum_list : 'a list -> ('a -> float) -> float = ""
external matrix_sum_list : 'a list -> ('a -> 'b matrix) -> 'b matrix = "" (* TODO: implement using list fold *)

(*let sum = __overload ~input:[true;false]*)
let sum = __instance int_sum_list
let sum = __instance float_sum_list
let sum = __instance matrix_sum_list
*)

let complex_of_int n = complex_of_float (float_of_int n)



type 'a monoid = unit (* dummy *)

external mk_monoid : ('a -> 'a -> 'a) -> 'a -> 'a monoid = ""

let int_add_monoid : int monoid = mk_monoid ( + ) 0
let float_add_monoid : float monoid = mk_monoid ( + ) 0
let complex_add_monoid : complex monoid = mk_monoid ( + ) 0
let complex_mul_monoid : complex monoid = mk_monoid ( * ) 1

let complex_matrix_add_monoid : complex matrix monoid = mk_monoid ( + ) matrix_zero

let addmonoid = __overload

let addmonoid = __instance int_add_monoid
let addmonoid = __instance float_add_monoid
let addmonoid = __instance complex_add_monoid
let addmonoid = __instance complex_matrix_add_monoid

external list_reduce : 'b monoid -> ('a -> 'b) -> 'a list -> 'b = ""
external matrix_reduce : 'b monoid -> ('a -> 'b) -> 'a matrix -> 'b = ""

let reduce = __overload ~input:[false;false;true]
let reduce = __instance list_reduce
let reduce = __instance matrix_reduce

let bigsum = __overload

let bigsum (type t) (addmonoid : t monoid)
           (type a u) (reduce : t monoid -> (a -> t) -> u -> t)
           : (a -> t) -> u -> t =
  __instance (fun f e -> reduce addmonoid f e)

(* LATER:
let bigsum (type t) (addmonoid : t monoid)
           (type a u) (reduce : t monoid -> (a -> t) -> u -> t)
           : (a -> t) -> u -> t =
  __instance (@@reduce addmonoid __ __) *)
(* Variant:
let bigsum (type t) (addmonoid : t monoid)
           (type a u) (reduce : t monoid -> (a -> t) -> u -> t)
           : (a -> t) -> u -> t =
  __instance (@^2 reduce addmonoid) *)

(*
  4 approaches to packaging:
  (1) Record projection presented as derived instance:
      addmonoid -> 0, addmonoid -> +
      Note: sometimes several structures apply to the same set (there are several distinct monoids in Z), but some of them are convertible. (how to resolve ambiguous resolution paths.)
  (2) Monoid constructor viewed as derived instance:
      If 0 and +, then addmonoid
  (3) Explicit instances:
      0 and addmonoid added for each type.
  (4) Doing both (1) and (2), but then the algorithm needs to prevent loops.
*)
(* Independently:

(* Compatible with (1), (2), and (4). *)
let bigsum (type t) (addmonoid : t monoid)
           (type a u) (reduce : t monoid -> (a -> t) -> u -> t)
           : (a -> t) -> u -> t =
  __instance (reduce addmonoid)

(* Compatible with (1), (3), and (4), but unexpected with (1) and (4). *)
let bigsum (type t) (zero : t) ((+) : t -> t -> t)
           : (a -> t) -> u -> t =
  __instance (fold (+) zero)
*)

(* [\bigsum_{i \in E} body] is a notation for
   [reduce addmonoid (fun i -> body) E] *)

let indices1 : int list = cons 1 (cons (-1) nil)
let indices2 = cons i (cons (2*i) nil)  (* [indices2 : complex list] *)


let demo1 (m : complex matrix) =
  reduce addmonoid (fun k -> (2:int)) indices2

let demo2 (m : complex matrix) : int =
  reduce addmonoid (fun k -> 2) indices2

let demo3 (m : complex matrix) =
  reduce addmonoid (fun k -> 2 * e ^ (pi * k / 8) ) indices2

let demo4 (m : complex matrix) =
  reduce addmonoid (fun k -> 2 * e ^ (k * pi / 8) ) indices2

let demo5 (m : complex matrix) =
  reduce addmonoid (fun k -> (3:complex) * m ^ (2*k^2)) indices1

let demo6 (m : complex matrix) =
  reduce addmonoid (fun k -> 3 * m) indices1

let demo7 (m : complex matrix) =
  reduce addmonoid (fun k -> 3 * m ^ (2*k^2)) indices1

let demo8 (m : complex matrix) =
  reduce addmonoid (fun k -> 3 * (m ^ (2*k^2))) indices1

let demo9 (m : complex matrix) =
  reduce addmonoid (fun k -> 3 * (e ^ ((complex_of_int k) * pi / 8)) * (m ^ (2*k^2))) indices1

let demo10 (m1 : complex matrix) m2 =
  reduce addmonoid (fun k -> 3 * (e ^ ((complex_of_int k) * pi / 8)) * (m1 ^ (2*k^2)) * m2) indices1


let demo11 (m1:complex matrix) (m2:complex matrix) =
  reduce
    addmonoid
    (fun d -> reduce
              addmonoid
              (fun k -> 3 * (e ^ (d * pi / 8)) * (m1 ^ (2*k^2)) * m2)
              indices1)
    indices2

let demo12 (m1:complex matrix) (m2:complex matrix) =
  reduce
    addmonoid
    (fun d -> reduce
              addmonoid
              (fun k -> 3 * ((e ^ (d * pi / 8)) * (m1 ^ (2*k^2))) * m2)
              indices1)
    indices2


let demo (m:complex matrix) (n:complex matrix) =
  reduce
    addmonoid
    (fun d -> reduce
              addmonoid
              (fun k -> 3 * (e ^ (d * pi / 8)) * (m ^ (2*k^2)) * n)
              (cons 1 (cons (-1) nil)))
    (cons i (cons (2*i) nil))

let exlet3 (f:int->int) (x:int) : int =
  let op = (fun n -> n + 42) in
  f (op (2+x))


let exd0 =
  let x = 0 in
  let y = 1 in
  ((x + x) + y) + (2 + (3:int))

let exd1 =
  let x = 0 in
  let y = 1 in
  let z = (x + x) + y in
  z + (2 + (3:int))

let exd2 (a:int) =
  let x = 0 in
  let z = (1 + 1) + (1 + x) in
  z + (2 + a)

(*
let exd3 (a:int) = (* fails*)
  let x = 0 in
  let z = (1 + 1) + (1 + x) in
  x + (2 + a)

*)


let __print = 1
(*let __debug = 1*)

(*

scalar!
=> special instance for int|float|complex

polymorphic instance for
  multiplication on 'a ==>
  multiplication on matrices: 'a -> 'a matrix -> 'a matrix
*)

(*
*)
(*
let demo (m : complex matrix) =
  reduce addmonoid (fun k -> e ^ (k * pi / 8) * m ^ 2) indices2

let demo4 (m : complex matrix) =
  reduce addmonoid (fun k -> 2 * e ^ (k * pi / 8) ) indices2

  --- TODO.. should we use scalar types?
  (* k is complex, pi could be a float or complex, but cannot be a matrice;
     thus k * pi should be a complex. but that's not inferred *)
*)


(* ############################################################ *)

(* todo:

let indices : int list = cons 1 (cons (-1) nil)

let demo (m : complex matrix) =
  sum indices (fun (k:int) -> m ^ (2*k^2))

let demo (m : complex matrix) =
  sum indices (fun (k:int) -> 2 * e ^ complex_of_int k)
  bug

let demo (m : complex matrix) =
  sum indices (fun (k:int) -> m ^ (2*k^2))

let demo (m : complex matrix) =
  sum indices (fun (k:int) -> 2 * (k * i * pi / 4) * (m ^ (2 * k^2)))

*)
(* sum k \in list_float  2 * (e^ (k * i * pi/4)) * M^(2*n^2) *)

(* ############################################################ *)
(* Polymorphism *)

(*
type 'a list = Nil

external list_map : 'a list -> ('a -> 'b) -> 'b list = "%list_map"

external int_add : int -> int -> int = "%addint"

val List.map :
val Array.map : 'a array -> ('a -> 'b) -> 'b array

let map = __instance Array.map
let map = __instance List.map
let d : list float = [3.2; 4.5]
let ex12 = map (fun x -> 2 * x + 1) d
let map = __overload ~input:[false; true]  (* input-output modes: true = input, false = output *)
*)





(* ############################################################ *)
(* With pairs

let ex11 = (fun x -> (x, (x + (1:int)))) (* inferred to be [int -> int*int] *)

let exZ =
  let p = (0,0) in
  (fst p + (1:int),
   snd p + (1:float))


*)

(*



(* ############################################################ *)
(* ############################################################ *)
(* ############################################################ *)






*)

(*

(**********************************************************************)
(* Generic prefix, to be included for most examples; don't delete it *)

let __numeric_without_dot = __instance (fun (x :int) : int -> x)
let __numeric_with_dot = __instance (fun (x :float) : float -> x)
let __empty_parentheses = __instance (fun (x : unit) : unit -> x)
let __double_quote = __instance (fun (x :string) : string -> x)
external float_of_int : int -> float = "%floatofint"
let __numeric_without_dot = __instance (fun (x : int) : float -> float_of_int x)
external ( = ) : 'a -> 'a -> bool = "%equal"
external (-) : int -> int -> int = "%subint"
let (-) = __instance (-)
external (-) : float -> float -> float = "%subfloat"
let (-) = __instance (-)

(**********************************************************************)
(* Program to debug begins here *)

*)

(*
let[@type_error "not bool instance for 3"] test_if_conditional_not_bool () =
  if () then () else ()
(* TODO: if expected given type, and only one instance with different type:
   => only instance has type foo , but expected type bar *)

let[@type_error "not bool instance for 3"] test_if_conditional_no_instance () =
  if 3 then () else ()
*)

(*
let x : int = 3

let[@type_error "missing information to resolve instance for overloaded name +"] plus4 : int -> int -> int =
  let p = (+) in p
*)



(**********************************************************************)
(* Deprecated material, kept around to use as templates *)
(*

type 'a t = unit

type 'a u = unit

let map1 (type a) (s :a t) (f : a -> a) = ()

let map2 (type a) (s :a u) (f : a -> a) = ()

let map = __overload ~input:[true;false]
let map = __instance map1
let map = __instance map2

let test_input (s : int t) (t : float u) =
  let s2 = map s (fun x -> x-0) in
  let t2 = map t (fun x -> x-1) in
  ()


*)

(* type myintlist = INil | ICons int * myintlist
 *
 * let rec myintlist_length
 *
 * type 'a mylist = Nil | Cons 'a * myintlist
 *
 * let rec mylist_length
 *
 * let rec mylist_map
 *
 * let letpoly () =
 *   let x : 'a list = Nil in
 *   let y = Cons(3, x) in
 *   let z = Cons(true, x) in
 *   () *)


(* let x : 'a list = nil
 *
 * let y = __instance x                       (\* en OCaml, sans annotation, cela devient un ['b list] polymorphe *\)
 * (\* chez nous, cela fait une erreur *\)
 *     let y = __instance (fun (type a ) -> x : a list)
 *
 * let r = map (fun a -> a+1) y    (\* si y : ?b list, alors nécessaire de fournir une information sur a *\)
 *
 * let x = nil, nil
 *
 *
 * let x: int = 0 + 0
 *
 * type t = C of int -> int
 * type u = C of bool -> bool
 *
 * let x : t = C (fun a -> a)
 *
 * let x = C (fun (a : int) -> a) *)
