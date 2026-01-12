(**************************************************************)
(* Typechecking traditional ML constructs *)

(* Literals: bool, unit, int, float, string *)

let cst_bool1 = true
let cst_bool2 = false
let cst_bool_annotated : bool = true

let cst_unit = ()
let cst_int = 1
let cst_float = (2. : float)
let cst_float_unannot : float = 2.0

let str : string = "test"
let str_unannot = ""

(* Tuples *)

let tuple2 = (2,3)
let tuple3 = (2,3,4)

(* Constructors *)

let simple_option1 = Some 2
let simple_option2 = None
let simple_option2_syntyp : bool option = None


(* Let-bindings *)

let let_1 =
  let x = 3 in x
let let_2 =
  let x = 3 in let y = 2 in (x, y)
(* Raises an error:
"When a let-binding has a type annotation and a fun(type) as body it must use the same list of names on both sides. Got a and 'a."
 *)
(* let let_poly =
  let x : type a. a list = [] in x
*)

(* External functions *)
external (* [%from_ocaml] *) (+) : int -> int -> int = "%addint"
(* external (=) : int -> int -> bool = "" *)
external (mod) : int -> int -> int = ""
external (/) : int -> int -> int = ""

(* Function definition *)
let fun_1 (x : int) : int =
  x

let fun_2 (x : int) (y : int) : int * int =
  (x,y)

let fun_poly (type a) (x : a) : a = x

let fun_fun (x : int) : int -> int =
   fun (y : int) -> x + y


(* Function call *)
let call_1 =
  fun_1 3

let call_2 =
  fun_2 3 4

let call_poly_1 =
  fun_poly 4

let call_poly_2 (type a) (x : a) : a =
  fun_poly x

let[@type_error "mistyped application"] fun_fun_fail (f : int -> int -> int) : int -> int =
  f 2

let fun_fun_curried (f : int -> (int -> int) func) : int -> int =
  f 2

let call_poly_fun (type a) (f : a -> (a -> a) func) : a -> (a -> a) func =
  fun_poly f

let call_poly_fun_fun =
  call_poly_fun fun_fun

(* Custom data types *)

(* not there yet. *)


(* Unification of branches *)
let unify_options = if true then Some 2 else Some 3

let[@type_error "branch mismatch in if"] unify_options_fail = if true then Some 2 else (Some false)



(**************************************************************)
(* BBE in if-statement: is, and, or, not *)

let bbe_is = if true @_is __ then true else false
let bbe_is_syntyp = if true @_is (__ : bool) then true else false

(* Expected to fail *)
let[@type_error "term conflicts with context"]  bbe_is_syntyp_fail = if true @_is (__ : int) then true else false

let bbe_is_bind1 : bool = if true @_is ??x then x else false

let bbe_is_bind2 : (bool * bool) = if (true @_is ??x) && (true @_is ??y) then (x, y) else (true, false)
(* to test *)
let[@type_error "unbound variable x"] bbe_is_bind_fail = if (true @_is ??x) && (true @_is ??y) then false else x



let bbe_and_bind = if (true @_is ??x) && x then x else false
let[@type_error "unbound variable x"] bbe_and_bind_fail = if (true @_is ??x) && x then x else x


let bbe_or_bind = if true @_is ??x || false @_is ??x then x else false

let[@type_error "unbound variable x"]  bbe_or_bind_fail1 = if false @_is ??x || true then x else false

let[@type_error "unable to unify"]  bbe_or_bind_fail2 = if false @_is ??x || 2 @_is ??x then x else false

let bbe_not = if not ((Some 2) @_is ??x) then false else true
let[@type_error "unbound variable x"] bbe_not_fail = if not ((Some 2) @_is ??x) then x else (Some 1)

(**************************************************************)
(* Pattern: variable, wildcard, constructor/inversor, predicate, and, or, not *)

(* variable and wildcards : check BBE section *)

(* Tuple constructor pattern *)
let tuple_bind1 = if tuple2 @_is (??x, ??y) then x + y else -1
let tuple_bind2 = if tuple3 @_is (??x1, ??x2, ??x3) then x1 + x3 else -1
let[@type_error "unbound variable x2"] tuple_bind_fail = if tuple3 @_is (??x1, ??x2, ??x3) then x1 + x3 else x2

(* Predicates *)

let even n = ((n mod 2) = 0)
let even_opt n = if n @_is even then (Some (n/2)) else None

(* Option destruction *)
let option_pat = if simple_option1 @_is None then 1 else 0
let option_pat_bind = if simple_option1 @_is (Some ??x) then x else 0

let inv_add (t : int option) f =
  if (t @_is (Some ??k)) && (k @_is (even_opt ??v)) then f v else f 0

let[@type_error "unbound variable x"] option_pat_bind_fail = if simple_option1 @_is (Some ??x) then 2 else x

let bbe_or_some y1 y2 : int = if (y1 @_is (Some ??c)) || (y2 @_is (Some ??c)) then c else -1

(*Expected to fail*)
let[@type_error "unbound variable c"] bbe_or_some_fail y1 y2 : int = if (y1 @_is (Some ??c)) || (y2 @_is (Some ??c)) then c else c
(*Expected to fail*)


(* if (o is Some ??n) && (even n) then f() else g()

let r = (o is Some ??n) && (even n) in
if r then f() else g()

if (o is Some ??n) && (even n) then f' n else g ()

  let r = (o is Some ??n) && (even n) in
  if r then (* n not in scope *) else g()

  let r () = if (o is Some ??n) && (even n) then Some ?n else None in
  if () is r(??n) then f' n else g() *)




(**************************************************************)
(* BBE in when-clauses, while-loops, and switch *)

(* when clauses *)
let pat_when x : int = if x @_is (Some ??a @_when (a = 1)) then a else -1
(* TODO: missing fail case *)

(* while loops *)
let while1 =
  while (true @_is false) do
    ()
  done

let while_bind x f =
  while (x @_is (Some ??y)) do
    f y
  done

let[@type_error "term conflicts with context"] while_bind_type_fail (x : int option) f =
  while (x @_is (Some ??y)) do
    y
  done

let[@type_error "unbound variable z"] while_bind_var_fail (x : int option) f =
  while (x @_is (Some ??y)) do
    z
  done

(* switch statements *)

(* Note on syntax : The parsing is specifically asking for "_case" to be applied to a single argument. This means that it is often necessary to add parentheses on the right. It might be interesting to change the inversion function at some point in the future *)

let switch_empty =
  __switch []

let switch1 =
  __switch [
    _case (true @_then false)
  ]

let switch2 =
  __switch [
    _case (true @_then false);
    _case ((true @_is ??x) @_then x);
    _case (((false, true) @_is (??x, ??y)) @_then y)
  ]

let[@type_error "unbound variable x"] switch_fail =
  __switch [
    _case ((true @_is ??x) @_then x);
    _case (true @_then x)
  ]

let[@type_error "type mismatch in switch"] switch_type_fail =
  __switch [
    _case ((true @_is ??x) @_then ());
    _case (true @_then 3)
  ]

(* Match statements *)
(* Important note: the patterns in match statements use the syntax of OCaml patterns, not expressions. This means in particular that a pattern variable should not have "??" as suffix. As it would cause a syntax error *)
(* Does not pass through the "compilation" process *)
(* let match_empty =
  match false with
  | _ -> ()

let match1 =
  match false with
  | true -> false

let match2 =
  match (true, false) with
  | (false, true) -> false
  | (false, x) -> x
  | (x, y) -> (if x then y else x) *)

(**************************************************************)
(* Nesting of features *)

let difficult_option : ((int option) * (int option)) option = Some (Some 2, Some 3)
let option_pat = if difficult_option @_is (Some (Some ??x, Some ??y)) then y else 3

let pat_deep_when z : int = if z @_is (Some ??a @_when (a @_is (Some ??n @_when (n = 2)))) then n else -1


(**************************************************************)
(* Motivating examples from slides *)


(* Feature focus 1 *)

external list_pop_opt : int list -> int option = ""

let queue_while_pop q f =
  while ((list_pop_opt q) @_is (Some ??x)) do
    f x
  done

(* With smart inversion: *)
let queue_while_pop q f =
  while (q @_is (list_pop_opt ??x)) do
    f x
  done
(* Feature focus 2 *)
external list_get_opt : int list -> int -> int option = ""
let hashtable_get tbl r f g =
  if (r @_is (Some ??k)) && ((list_get_opt tbl k) @_is (Some ??v))
    then f v
    else g ()

(* TODO: write this with switch + when later
 switch
 case (r @_is (Some (??k @_when ((list_get_opt tbl k) @_is (Some ??v))))
 case (r @_is ((Some ??k) @_when ((list_get_opt tbl k) @_is (Some ??v)))
 case
*)

(* About inversors : *)






(* ^ the two cases should behave the same *)





(* Constructor inversion *)
let bbe_is_bind_constr = if (Some true) @_is (Some ??x) then x else false

(* Custom type definition *)
(* ok *)
type myoptionint = MyNoneInt | MySomeInt of int
type 'a myoption = MyNone | MySome of 'a
type 'a mylist = MyNil | MyCons of 'a * 'a mylist
type 'a mylistp = MyNilp | MyConsp of ('a * 'a mylistp)
type ('a,'b) mypair = 'a * 'b (* typ_tuple ['a; 'b] *)

(* Constructors *)
let myint1 = MyNoneInt
let myint2 = MySomeInt 2
let[@type_error "mistyped application"] myint_fail = MySomeInt false

let mylist0 : int mylist = MyNil
let mylist1 = MyCons (1, MyNil)
let mylistp1 = MyConsp (2, MyNilp)

let myoptionnone : int myoption = MyNone
let myoptionnsome : (int mylist) myoption = MySome mylist1

(* Destruction *)
let bbe_view = if mylist0 @_is MyNil then MyNil else MyCons (1, MyNil)
let bbe_view_bind = if (mylist0 @_is (MyCons (1, ??x))) then MyNil else MyCons (1, MyNil)
(* Explicit BBE conversion *)
(* let inv_lit c =
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
 *)

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
let even_opt n = if even n then (Some (n/2)) else None

let testing_inv_and (type a) (t : int option) (f : int -> a) =
  if (t @_is (Some ??k)) && (k @_is (even_opt ??v)) then f v else f 0

(* TODO URGENT: test pattern inversion of custom constructors. *)

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



(*

let bbe_and : int = if x1 @_is Some ??a && x2 @_is Some ??b then a + b else -1



let bbe_not : int = if not (x1 @_is Some ??d) then -1 else d
(*Expected to fail*)
let bbe_is_bind_fail : int = if not (x1 @_is Some ??d) then d else -1

let z1 = Some (Some 2)


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
