type 'a list =
  | []
  | (::) of ('a * 'a list)


let[@instance: unit] _ = fun (x : unit) -> x
let[@instance: bool] _ = fun (b : bool) -> b



let () =
  let[@instance g] g1 = fun (type a) (x : a) (y : a) -> x in
  let[@instance g] g2 = fun (type a) (x : a) (y : a -> a) -> x in

  let f1 (type a) (v:a) = g v v in  (* only g1 applies *)
  let f2 (type a) (v:a) = g v (fun (r:a) -> r) in  (* only g2 applies *)
  ()


(*


external int_mul : int -> int -> int = "%mulint"
let[@instance ( * )] _ = int_mul

external int_add : int -> int -> int = "%addint"
let[@instance ( + )] _ = int_add

external float_add : float -> float -> float = "%addfloat"
let[@instance ( + )] _ = float_add

let[@instance: int] _ = (fun (x : int) : int -> x)
external float_of_int : int -> float = "%floatofint"
let [@instance: int] _ = (fun (x : int) : float -> float_of_int x)


let harder_example =
    let x = 0 in
    let y = 1 in
    let z = (1 + x) + y in
    (2 + x) + (3:int)
*)

(*

external int_add : int -> int -> int = "%addint"
let (+) = __instance int_add

type 'a array
external array_fold : ('b -> 'a -> 'b) -> 'b -> 'a array -> 'b = "%array_fold"

type 'a matrix = unit (* dummy *)

external matrix_add : ('a -> 'a -> 'a) -> 'a matrix -> 'a matrix -> 'a matrix = ""

let (+) (type a) ((+) : a -> a -> a) : a matrix -> a matrix -> a matrix =
  (*__instance (fun m1 m2 -> matrix_add (+) m1 m2)*)
  (* TODO: should make that work: __instance (fun m1 m2 -> matrix_add (+) m1 m2)*)
  __instance (fun m1 m2 -> matrix_add ((+) : a->a->a) m1 m2)


let zero = __overload

(* ok but other def follows
let sum (type a) ((+) : a -> a -> a) (zero : a) : a array -> a =
  __instance (fun s -> array_fold (fun acc v -> acc + v) zero s)
*)

type 'a monoid = { op : 'a -> 'a -> 'a ; neutral : 'a }

let addmonoid = __overload

let __numeric_without_dot = __instance (fun (x : int) : int -> x)
external float_of_int : int -> float = "%floatofint"
let __numeric_without_dot = __instance (fun (x : int) : float -> float_of_int x)

(* ok but subsumed
let addmonoid : int monoid = __instance { op = (+); neutral = 0 } *)


let sum (type a) (addmonoid : a monoid) : a array -> a =
  __instance (fun s -> array_fold (fun acc v -> (addmonoid : a monoid).op acc v) addmonoid.neutral s)

external array_of_list : 'a list -> 'a array = "Array.list"

(*let zero : int = __instance (0:int)*)
let addmonoid : int monoid = __instance { op = (+); neutral = 0 } (***)

let addmonoid (type a) ((+) : a -> a -> a) (zero : a) : a monoid =
  __instance ({ op = (+); neutral = zero })

(* let __debug = 1 *)
let[@type_error "instances all mismatch for sum"] result1 =
  (*There are several possible instantiation of addmonoid. *)
  (* TODO: We would like to be able to actually force a specific instance. *)
  sum (array_of_list [ (4:int); 5; 6 ])


(* ok but subsumed




external array_fold : ('b -> 'a -> 'b) -> 'b -> 'a array -> 'b = "%array_fold"
type 'a list
external list_fold : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b = "%list_fold"

let fold (type a) (type x) : (a -> x -> a) -> a -> x array -> a = __instance array_fold (*  *)
let fold (type a) (type x) : (a -> x -> a) -> a -> x list -> a = __instance list_fold (*  *)

let mapreduce = __overload
(* valid but conflicts if uncommented
let mapreduce (type t) (type a) (type x)
  (fold : (a -> x -> a) -> a -> t -> a)
  : (x -> a) -> a monoid -> t -> a =
  __instance (fun f m s -> fold (fun acc x -> m.op acc (f x)) m.neutral s)
*)



let sum (type t) (type a)
  (addmonoid : a monoid)
  (mapreduce : (a -> a) -> a monoid -> t -> a)
  : t -> a =
  __instance (fun (s:t) -> (mapreduce : (a -> a) -> a monoid -> t -> a) (fun x -> x) addmonoid s)

(*
let result2 = sum ([| 4; 5; 6 |] : int array)

*)

*)


(*---------------let f (x:int) = x
*)

(* TODO
let __empty_parentheses = __instance (fun (x : unit) : unit -> x)


type t = A of t | B of int | C of int
type u = A of u | B of float

type 'a p = P of 'a * 'a


  let f v =
    match v with
    | A _ -> ()
    | B _ -> ()
    | C _ -> () (* the first traversal of this pattern forces v:t on *)

*)

(*
  let g v =
    match v with
    | A (B x) -> ()
    | A (B x) -> ignore (x:int)  (* the second traversal of this pattern forces v:t *)
    | _ -> ()
    *)
    (*
let h v =
  match v with
  | P (A y, B x) -> (x:int)
*)

(* TODO: matrix operations are on monoids, not on adds. *)


(*
type 'a p = 'a

type 'a monoid

external monoid_neutral : 'a monoid -> 'a
external monoid_op : 'a monoid -> ('a -> 'a -> 'a) p


let sum = __overload
*)

*)

