
let[@instance: int] id_int = fun (x : int) : int -> x
let[@instance: float] id_float = fun (x : float) : float -> x
let[@instance: unit] id_unit = fun (x : unit) : unit -> x
let[@instance: string] id_string = fun (x : string) : string -> x
let[@instance: bool] id_bool = fun (b : bool) : bool -> b

let cst_bool = true
let cst_unit = ()
let cst_int = 1

let cst_float = (2. : float)
let cst_float_unannot : float = 2.0

let vtrue : bool = true
let vfalse : bool = false

let vunit : unit = ()

let str : string = "test"
let str_unannot = ""

let bbe_is : bool = if true @_is __ then true else false
let bbe_is_bind : bool = if true @_is ?x then x else false


let x1 : int option = Some 1
let x2 : int option = Some 2

let bbe_and : int = if x1 @_is Some ?a && x2 @_is Some ?b then a + b else -1

let y1 : int option = Some 1
let y2 : int option = None

let bbe_or_1 : int = if y1 @_is Some ?c || y2 @_is Some ?c then c else -1
let bbe_or_2 : int = if y2 @_is Some ?c || y1 @_is Some ?c then c else -1

(*Expected to fail*)
let bbe_or_1_fail : int = if y1 @_is Some ?c || y2 @_is Some ?c then c else c
(*Expected to fail*)
let bbe_or_2_fail : int = if y2 @_is Some ?c || y1 @_is Some ?c then c else c

let bbe_not : int = if not (x1 @_is Some ?d) then -1 else d
(*Expected to fail*)
let bbe_is_bind_fail : int = if not (x1 @_is Some ?d) then d else -1

let z1 = Some (Some 2)

let pat_when : int = if x1 @_is (Some ?a @_when a = 1) then a else -1
let pat_deep_when : int = if z1 @_is (Some ?a @_when (a @_is (Some ?n @_when n = 2))) then n else -1


let even x : bool = x mod 2 = 0
let div_4 x : bool = x mod 4 = 0

let even_inv (x : int option) : int option = if x @_is (Some ?a @_when (a @_is even)) then Some a else None
let div_4_inv (x : int option) : int option = if x @_is (Some ?a @_when (a @_is div_4)) then Some a else None

let rest_div_4 (x : int option) : int option = if x @_is Some ?a then a mod 4 else None

let pat_pred : bool = if 2 @_is even then true else false
let x3 : int option = Some 14
let pat_view : int = if x3 @_is div_4_inv ?a then a else -1

let pat_and : bool = if 4 @_is (even & div_4) then true else false
let pat_and_bind : (int * int) option = if x3 @_is (even_inv ?a & rest_div_4 ?b) then Some (a, b) else None
let pat_or : bool = if 2 @_is (even or div_4) then true else false
let pat_or_bind : int option = if x3 @_is (even_inv ?a or rest_div_4 ?a) then Some a else None

(*Expected to fail*)
let pat_or_bind_fail : int option = if x3 @_is (even_inv ?a or rest_div_4 ?b) then Some a else None

let pat_not : int option = if x3 @_is not (Some ?x) then None else x
(*Expected to fail*)
let pat_not : int option = if x3 @_is not (Some ?x) then x else x

let pat_as : int = if z1 @_is Some ((Some ?x) @_as ?y) && y @_is even_inv ?z then z else -1


let y1 : int option = Some 1
let y2 : int option = None

let bbe_or_1 : int = if y1 @_is Some ?c || y2 @_is Some ?c then c else -1
let bbe_or_2 : int = if y2 @_is Some ?c || y1 @_is Some ?c then c else -1

let empty_switch (type a) : a = switch []
let nonempty_switch : unit = switch [
  case y1 @_is None @_then ()
  case y2 @_is None @_then ()
]

let nonempty_switch_bind : int = switch [
  case y1 @_is Some ?x @_then x
  case y2 @_is None @_then -1
]
