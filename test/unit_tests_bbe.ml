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

let bbe_is = if true @_is __ then true else false
let bbe_is_syntyp = if true @_is (__ : bool) then true else false

(* Expected to fail *)
let[@type_error ""]  bbe_is_syntyp_fail = if true @_is (__ : int) then true else false

let bbe_is_bind1 : bool = if true @_is ??x then x else false

let bbe_is_bind2 : bool = if (true @_is ??x) && (true @_is ??y) then x else false


(* Tuple terms *)
let tuple2 = (2,3)
let tuple3 = (2,3,4)

let bbe_and_bind = if true @_is ??x && x then true else false

let bbe_or_bind = if true @_is ??x || false @_is ??x then x else false

(* Expected to fail *)
let[@type_error "unbound variable x"]  bbe_or_bind_fail1 = if false @_is ??x || true then x else false

(* Expected to fail *)
let[@type_error "unable to unify"]  bbe_or_bind_fail2 = if false @_is ??x || 2 @_is ??x then x else false



(* TODO: write a unit-test where the typer tries to unify BBEs *)

(* About inversors : *)

(*

external some : 'a -> 'a option



*)


(* Constructor inversion *)
(* let bbe_is_bind_constr = if (Some true) @_is (Some ??x) then x else false
 *)
(* Custom type definition *)
(* type myoptionint = MyNoneInt | MySomeInt of int
type 'a myoption = MyNone | MySome of 'a
type 'a mylist = MyNil | MyCons of 'a * 'a mylist (* MyCons: typ_arrow ['a; typ_constr "mylist" ['a]] (typ_constr "mylist" ['a]) *)
type 'a mylistp = MyNilp | MyConsp of ('a * 'a list) (* MyCons: typ_arrow [typ_tuple ['a; typ_constr "mylistp" ['a]]] (typ_constr "mylistp" ['a]) *)
type ('a,'b) mypair = 'a * 'b (* typ_tuple ['a; 'b] *)

(* Constructors *)

let mylist0 : int mylist = MyNil

let mylist1 = MyCons (1, MyNil)
let mylistp1 = MyConsp (2, MyNilp)

let myoptionnone : int myoption = MyNone
let myoptionnsome : (int mylist) myoption = MySome mylist1

(* Explicit BBE conversion *)
let inv_lit c =
  bool_of (c @_is 3)

let inv_bool c =
  bool_of (c @_is true)
let inv_tuple2 c =
  bool_of (c @_is (true,false))

let inv_none (c:'a myoption) : bool =
  bool_of (c @_is MyNone)

let alltrue (b1 b2 b3: bool) : bool =
  b1 && b2 && b3

let optioneven (o: option int) : bool =
  (o is Some ??n) && (even n)


(* Deep inversors *)
type trm_desc =
| Trm_bool of bool
| Trm_if of trm_desc * trm_desc * trm_desc

let trm_bool (b : bool) = Trm_bool b

let trm_if t1 t2 t3 = Trm_if (t1, t2, t3)

let trm_and t1 t2 = trm_if t1 t2 (trm_bool false)

let trm_bool_inv (t : trm_desc) : bool option =
  if t @_is (Trm_bool ??b) then Some b else None

let trm_if_inv (t : trm_desc) : (trm_desc * trm_desc * trm_desc) option =
  if t @_is (Trm_if (??t1, ??t2, ??t3)) then Some (t1, t2, t3) else None

let trm_and_inv (t : trm_desc) : (trm_desc * trm_desc) option =
  if t @_is (trm_if_inv (??t1, ??t2, trm_bool_inv false)) then Some (t1, t2) else None

let trm_and_3_inv (t : trm_desc) : (trm_desc * trm_desc * trm_desc) option =
  if t @_is (trm_and_inv (??t1, (trm_and_inv ??t2 ??t3))) then Some (t1,t2,t3) else None

(* Binding boolean expressions *)

let even n = n mod 2 = 0
let even_opt n = if even n then Some n/2 else None

let f (x : int) : int = x

let testing_inv_and (t : int option) =
  if (t @_is Some ??k) && k @_is even_opt v then f v else f 0
 *)

  (*
  if (o is Some ??n) && (even n) then f() else g()

  let r = (o is Some ??n) && (even n) in
  if r then f() else g()

  if (o is Some ??n) && (even n) then f' n else g ()

   let r = (o is Some ??n) && (even n) in
   if r then (* n not in scope *) else g()

   let r () = if (o is Some ??n) && (even n) then Some ?n else None in
   if () is r(??n) then f' n else g()

   same with syntactic sugar:

   let r () = Pattern.make ((o is Some ??n) && (even n)) in
   if r(??n) then f' n else g()

     Pattern.make would take the ??XX in the order of AST-traversal, and put them in "then Some"

     si on met "p" en position de bbe, ça veut dire
     () is p


   let r o = Pattern.make ((o is Some ??n) && (even n)) in
   if o @_is r(??n) then f' n else g()




  *)


  (* sucre pour "if b then true else false" *)



(* let x1 : int option = Some 1
let x2 : int option = Some 2

let bbe_and : int = if x1 @_is Some ??a && x2 @_is Some ??b then a + b else -1

let y1 : int option = Some 1
let y2 : int option = None

let bbe_or_1 : int = if y1 @_is Some ??c || y2 @_is Some ??c then c else -1
let bbe_or_2 : int = if y2 @_is Some ??c || y1 @_is Some ??c then c else -1

(*Expected to fail*)
let bbe_or_1_fail : int = if y1 @_is Some ??c || y2 @_is Some ??c then c else c
(*Expected to fail*)
let bbe_or_2_fail : int = if y2 @_is Some ??c || y1 @_is Some ??c then c else c

let bbe_not : int = if not (x1 @_is Some ??d) then -1 else d
(*Expected to fail*)
let bbe_is_bind_fail : int = if not (x1 @_is Some ??d) then d else -1

let z1 = Some (Some 2)

let pat_when : int = if x1 @_is (Some ??a @_when a = 1) then a else -1
let pat_deep_when : int = if z1 @_is (Some ??a @_when (a @_is (Some ??n @_when n = 2))) then n else -1


let even x : bool = x mod 2 = 0
let div_4 x : bool = x mod 4 = 0

let some_even (x : int option) : int option = if x @_is (Some ??a @_when (a @_is even)) then Some a else None
let div_4_inv (x : int option) : int option = if x @_is (Some ??a @_when (a @_is div_4)) then Some a else None

let rest_div_4 (x : int option) : int option = if x @_is Some ??a then a mod 4 else None

let pat_pred : bool = if 2 @_is even then true else false
let x3 : int option = Some 14
let pat_view : int = if x3 @_is div_4_inv ??a then a else -1

let pat_and : bool = if 4 @_is (even &&& div_4) then true else false
let pat_and_bind : (int * int) option = if x3 @_is (some_even ??a &&& rest_div_4 ??b) then Some (a, b) else None
let pat_or : bool = if 2 @_is (even or div_4) then true else false
let pat_or_bind : int option = if x3 @_is (some_even ??a ||| rest_div_4 ??a) then Some a else None

(*Expected to fail*)
let pat_or_bind_fail : int option = if x3 @_is (some_even ??a ||| rest_div_4 ??b) then Some a else None

let pat_not : int option = if x3 @_is not (Some ??x) then None else x
(*Expected to fail*)
let pat_not : int option = if x3 @_is not (Some ??x) then x else x

let pat_as : int = if z1 @_is Some ((Some ??x) @_as ??y) && y @_is some_even ??z then z else -1


let y1 : int option = Some 1
let y2 : int option = None

let bbe_or_1 : int = if y1 @_is Some ??c || y2 @_is Some ??c then c else -1
let bbe_or_2 : int = if y2 @_is Some ??c || y1 @_is Some ??c then c else -1

let empty_switch (type a) : a = switch []
let nonempty_switch : unit = switch [
  case y1 @_is None @_then ();
  case y2 @_is None @_then ()
]

let nonempty_switch_bind : int = switch [
  case y1 @_is Some ??x @_then x;
  case y2 @_is None @_then -1
]

let nonempty_switch_bind : int = switch [
  case y1 @_is Some ??x @_then begin
      x;
    end
  case y2 @_is None @_then -1
]
 *)
