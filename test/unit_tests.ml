
external float_of_int : int -> float = "%floatofint"

external int_add : int -> int -> int = "%addint"
external int_sub : int -> int -> int = "%subint"
external int_mul : int -> int -> int = "%mulint"
external int_div : int -> int -> int = "%divint"
external int_mod : int -> int -> int = "%modint"

external float_add : float -> float -> float = "%addfloat"
external float_sub : float -> float -> float = "%subfloat"
external float_mul : float -> float -> float = "%mulfloat"
external float_div : float -> float -> float = "%divfloat"
external float_mod : float -> float -> float = "caml_fmod_float" "fmod"

external ( < ) : 'a. 'a -> 'a -> bool = "%lessthan"
external ( > ) : 'a. 'a -> 'a -> bool = "%greaterthan"
external ( = ) : 'a. 'a -> 'a -> bool = "%equal"

let[@instance: int] id_int = fun (x : int) : int -> x
let[@instance: int] _ = float_of_int
let[@instance: float] id_float = fun (x : float) : float -> x
let[@instance: unit] id_unit = fun (x : unit) : unit -> x
let[@instance: string] id_string = fun (x : string) : string -> x
let[@instance: bool] id_bool = fun (b : bool) : bool -> b

(*let ( + ) = __overload ~input:[true;true]*)
let[@instance ( + )] _ = int_add
let[@instance ( + )] _ = float_add
(*let ( - ) = __overload ~input:[true;true]*)
let[@instance ( - )] _ = int_sub
let[@instance ( - )] _ = float_sub
(*let ( * ) = __overload ~input:[true;true]*)
let[@instance ( * )] _ = int_mul
let[@instance ( * )] _ = float_mul


(* With the instance declared above, strings, units, and booleans behave as usual in OCaml.
  However, numbers without dots (e.g. [1]) can both be [int] or [float], depending on the
  context.
  Numbers with dots (e.g. [1.] or [1.0]) are unambiguously [float]. *)

(* let __debug = -1 *)
(* let __debug = 1 *)


(* ############################################################ *)
(* Basic values *)

let cst_bool = true

let cst_unit = ()

let cst_int : int = 2

let cst_float = (2 : float)

let cst_float_unannot = 2.0

let vtrue : bool = true
let vfalse : bool = false

let vunit : unit = ()

let str : string = "test"
let str_unannot = ""

let[@type_error "instances all mismatch for <bool>"] wrong_type_cst_bool : int = true
let[@type_error "instances all mismatch for ()"] wrong_type_cst_unit : int = ()
let[@type_error "instances all mismatch for <int>"] wrong_type_cst_int : unit = 1
let[@type_error "instances all mismatch for <float>"] wrong_type_cst_float : unit = 1.
let[@type_error "instances all mismatch for <string>"] wrong_type_cst_string : int = ""

let[@type_error "instances all mismatch for <bool>"] wrong_cast_cst_bool = (true : unit)
let[@type_error "instances all mismatch for ()"] wrong_cast_cst_unit = (() : string)
let[@type_error "instances all mismatch for <int>"] wrong_cast_cst_int = (1 : unit)
let[@type_error "instances all mismatch for <float>"] wrong_cast_cst_float = (1. : int)
let[@type_error "instances all mismatch for <string>"] wrong_cast_cst_string = ("" : unit)

let[@type_error "instances all mismatch for (.,.)"] wrong_type_pair : int = (1, 1)
let[@type_error "instances all mismatch for (.,.)"] wrong_cast_pair = ((1, 1) : string)


(* ############################################################ *)
(* Lists *)

let vnil : int list = []
let[@type_error "not fully resolved symbol vnil_unannot"] vnil_unannot = []

let vcons1 : int list = 1 :: []
let vcons2 : int list = [1]
let vcons3 : float list = [1; 2]
let vcons4 : float list = [1; 2.0]
let[@type_error "not fully resolved symbol vcons_unannot"] vcons_unannot = [1; 2]

let unit_list = [()]

let nested_list_float = [[]; [1.]]
let nested_list_int : int list list list = [[[1]]]

let nested_lists = [[[[[[[()]]]]]]]


(* ############################################################ *)
(* Tuples *)

let pair : int * float = (1, 1)
let tuple : int * float * (int * float) = (1, 1, (1, 1))

let tuple_with_mixed_annotations : _ * float * _ =
  ((1 : int), 1, ((1, 1.) : int * _))

let[@type_error "instances all mismatch for (.,.,.)"] mismatch : _ * _ =
  ("", "", "")
let[@type_error "instances all mismatch for (.,.)"] mismatch : _ * _ * _ =
  ("", "")


let pair_str_unit = ("test", ())

let triple_string : _ * _ * _ = ("", "", "")
let triple_string_unannot = ("", "", "")

(* ############################################################ *)
(* Basic function definitions *)

let not b = if b then false else true

let (&&) b1 b2 = if b1 then b2 else false

let ignore (type t) (_ : t) = ()

let[@type_error "not fully resolved symbol inner_missing_info"] inner_missing_info =
  ignore 1

let farity : int -> bool -> _ = fun x y -> (0:int)

let funho1 (f : int -> int -> int) (x : int) : int =
  f x x

let fun_arg_ret f (x : int) = f (f x)

let rec fact (n : int) : int =
  if n = 0 then 1
  else n * fact (n - 1)

let inner_rec =
  let rec loop () : unit = loop () in
  ()

(* This loops if one ever calls it.
  We are here just checking that it does type-check. *)
let funrec1 (x : float) =
  let rec g (x : float) : int =
    g 1. in
  g x

let funarith (add : int -> int -> int) =
  let x : int = 4 in
  let y = (4 : int) in
  add (add 0 (add 1 x)) (add 1  y)

(* Partial application is not allowed in our typer. *)
let[@type_error "mistyped application"] farity_partial = farity 1

let farity_forced_partial = fun i ->
  let r = fun b -> farity i b in
  r

let farity_forced_partial_partial = farity_forced_partial 1

(* Also, application of more arguments than expected (even if it would make sense in
  several applications) is not accepted. *)
let[@type_error "mistyped application"] farity_forced_partial_full =
  farity_forced_partial 1 true

(* Adding parentheses resolves the matter. *)
let farity_forced_partial_full_parentheses = (farity_forced_partial 1) true


(* ############################################################ *)
(* Conditionals *)

let cmp0 (n : int) : bool =
  n = 0

let cond0 (n : int) : int =
  if n = 0 then 1 else 2

(* [cond0] returns an integer instead of a boolean. *)
let[@type_error "term conflicts with context"] no_boolean : float =
  if cond0 1 then 2 else 3

let cond_unit =
  if cmp0 1 then ()

let[@type_error "branch mismatch in if"] cond_mismatch (n : int) =
  if n > 0 then (1 : int)
  else (1 : float)

let[@type_error "instances all mismatch for <int>"] cond_implicit_else =
  if cmp0 1 then 1 (* There is no instance for number litterals of type [unit]. *)

let nested_ifs : float =
  if if true = if true then true else false then true else false then 1 else 2


(* ############################################################ *)
(* Sequence *)

let seq_unit x = x ; ()

let seq_if b x =
  if b then x ; ""

let seq_if a b =
  a ; b ; 1.

(* A [unit] type is expected before a sequence. *)
let[@type_error "instances all mismatch for <int>"] num_seq = 1 ; ()
let[@type_error "term conflicts with context"] string_seq = ("" : string) ; ()


(* ############################################################ *)
(* Let-binding *)

let seq_let x =
  let () = x in 1.

let add_let =
  (let a = 1 in a) + 1.0

let nested_let1 =
  let a =
    let b =
      let c = 1 in
      c + 1 in
    b in
  let b =
    let c = 1 in
    c + a in
  b + a + 1.0

let nested_let2 =
  (
    let a =
      let b =
        let c = 1 in
        c + 1 in
      b in
    let b =
      let c = 1 in
      c + a in
    b + a
  ) + 1.0


(* ############################################################ *)
(* Pattern-matching *)

let int_list_math (l : int list) =
  match l with
  | x :: _ -> x
  | [] -> 42

let head1 (type t) default (l : t list) =
  match l with
  | [] -> default
  | x :: _ -> x

let head2 (type t) (default : t) l =
  match l with
  | [] -> default
  | x :: _ -> x

let head3 (type t) default l =
  match l with
  | [] -> default
  | (x : t) :: _ -> x

let fst (type a b) ((a, b) : a * b) = a

let fst_match (type a b) (p : a * b) =
  match p with (a, b) -> a

let snd (type a b) ((a, b) : a * b) = b

let snd_match (type a b) (p : a * b) =
  match p with (a, b) -> b

let bool_match =
  match true with
  | true -> true
  | false -> false

let unit_match =
  match () with () -> ()

let string_match =
  match "a" with
  | "b" | "c" -> "d"
  | (("a" | "f") as x) | (x : string) -> x

(* In patterns, [1] is considered to be the constant one. *)
let int_match =
  match 1 with
  | 1 -> true
  | _ -> false

let float_match =
  match 1 with
  | 1. -> true
  | _ -> false

let annot_match (type t) x =
  match x with (_ : t) -> x

let list_match =
  match [] with
  | 1 :: _ -> []
  | x -> x

let branch_match =
  match true with
  | true -> 1
  | false -> 0.

let[@type_error "instances all mismatch for <string>"] match_string_unit =
  match "test" with () -> ()

let[@type_error "instances all mismatch for <string>"] match_string_int =
  match "test" with 1 -> ()

let[@type_error "instances all mismatch for <string>"] match_string_float =
  match "test'" with 1. -> ()

let[@type_error "instances all mismatch for ( [] )"] match_string_list =
  match "" with [] -> ()

let[@type_error "not fully resolved symbol list_match_without_annot"] list_match_without_annot =
  match [] with [] -> () | x -> ()

let[@type_error "x must occur on both sides of this pattern"] both_side_pattern1 =
  match () with (x | ()) -> ()

let[@type_error "x must occur on both sides of this pattern"] both_side_pattern2 =
  match () with (() | (_ as x)) -> ()


type 'a c_param = C of 'a

let c_param_project1 (type t) (c : t c_param) =
  match c with C x -> x

let c_param_project2 (type t) c =
  match c with C x -> (x : t)

let c_param_project3 c =
  match c with
  | C true -> true
  | C false -> false

type c_fix = CF of int

let c_fix_project1 (c : c_fix) =
  match c with CF x -> x

let c_fix_project2 c =
  match c with CF x -> x

let one_match1 =
  (match 1 with a -> a) + 1.0

let one_match2 =
  (match 1 with a -> (a : int))

let one_match3 : int =
  match 1 with a -> a

let one_match4 : float =
  match 1 with a -> a

let one_match5 =
  match 1.0 with a -> a

let true_match =
  match true with a -> a

let c_param_match v =
  (match v with C x -> x) + 1.0

let pair_match =
  match (1, 2) with
  | (a, b) -> a + float_of_int b

let match_nested x =
  match
    match
      match x with
      | (a, b) -> a with
    | (a1, a2) -> (a1, x) with
  | (a1, x) ->
    match x with
    | ((_, a2), b) ->
      if a2 = a1 then b else (a1 + 1.0)

type 'a homopairs =
  | H of 'a * 'a

let homopairs_match x =
  match x with
  | H (a, b) ->
    ignore (a + 1.0) ;
    b

let homopairs_inline (H (a, b)) =
  ignore (a + 1.0) ;
  b

type t = Var of string | Let of string * t * t | Load of t
type u = Var of string | Let of string * u * u | Load of string

let rec generate_var_fresh_from (e : t) : string =
  let aux = generate_var_fresh_from in
  match e with
  | Var x -> "__var_" ^ x
  | Let (x, t1, t2) ->
    "__let_" ^ x
    ^ "__" ^ aux t1
    ^ "_in_" ^ aux t2
  | Load t -> "__load_" ^ aux t

let rec norm (e:t) : u =
  match e with
  | Var x -> Var x
  | Let (x, t1, t2) -> Let (x, norm t1, norm t2)
  | Load t1 ->
    match t1 with
    | Var x -> Load x
    | _ -> let x = generate_var_fresh_from t1 in
    Let (x, norm t1, Load x)


(* ############################################################ *)
(* Polymorphism *)

let[@type_error "instances all mismatch for ( + )"] failing (type a) : a -> a -> a = (+)

let[@type_error "not fully resolved symbol id1"] id1 x = x

let id2 (type a) (x:a) = x

let id3 (type a) x = (x:a)

let id4 : int -> _ = (fun x -> x)

let id5 : 'a. 'a -> 'a = fun x -> x

let funho2 (type a) (type b) f (x:a) : b = f x

let id (type z) (x : z) : z =
  x

let apply (type f) (type w) (f : f -> w) (x : f) : w =
  f x

let poly (x : int) (y : bool) : unit =
  let a = id x in
  let b = id y in
  ()

let compose (type a) (type b) (type c) (f : a -> b) (g : b -> c) : a -> c =
  fun (x : a) ->
    let y = f x in
    g y

let cond (type s) (b : bool) (f : unit -> s) (g : unit -> s) : s =
  if b then f () else g ()

let local_ho =
  let app (f : int -> float) (x : int) = f x in
  app (fun x -> 1.) 3


let funho_app (type a) (type b) (f : a -> b) (x : a) = f x

(* Checking an unused `rec' *)
let rec funrec2 (type a) (x : a) : a =
  x

let rec funreccond (n : int) : int =
  if n = 0 then 1 else funreccond (n - 1)

(* Again, this loops if being called.
 We are here checking that we can call oneself with different instantiations of
 the type parameters. *)
let rec funpolyrec (type a) (x : a) : unit =
  funpolyrec (fun (x : a) -> x)


(* Due to the way we built the parser, rightmost parentheses tends to be ignored in types.
  This doesn't change anything in OCaml where partial application is allowed, but it changes
  in this context.
  The following [p] parenthesis-type is a hack to circumvent the issue. *)
type 'a p = 'a
type 'a church_nat = ('a -> 'a) -> ('a -> 'a) p

(* As above, because partial application is not accepted in our setting, we need some
  syntactic circumventions to have the following term return the expected type (with
  the right partial applications). *)
let church_0 (type t) : t church_nat = fun f ->
  let r = fun x -> x in r

let church_1 (type t) : t church_nat = fun f ->
  let r = fun x -> f x in r

let church_1 (type t) : t church_nat = fun f ->
  let r = fun x -> f (f x) in r

let church_succ_left (type t) (n : t church_nat) : t church_nat = fun f ->
  let r = fun x -> f ((n f) x) in r

let church_succ_right (type t) (n : t church_nat) : t church_nat = fun f ->
  let r = fun x -> (n f) (f x) in r

let church_add (type t) (n : t church_nat) (m : t church_nat) : t church_nat = fun f ->
  let r = fun x -> (n f) ((m f) x) in r

let church_mult (type t) (n : t church_nat) (m : t church_nat) : t church_nat = fun f ->
  let r = fun x -> (n (m f)) x in r

let church_exp (type t) (n : _ church_nat) (m : _ church_nat) : t church_nat = fun f ->
  let r = fun x -> ((n m) f) x in r


(* let cons = __overload *)

let[@instance cons] cons_curryfied (type a) (x : a) l = x :: l
let[@instance cons] cons_uncurryfied (type a) p : a list =
  match p with ((x : a), l) -> x :: l

let cons1 = cons "" []
let cons2 = cons ("", [])
let cons3 = cons 1 (cons 2.0 [])
let cons4 = cons (1, cons 2.0 [])

(* There are several mechanisms to introduce type variables, and we check that they can deal
  with the empty list fine. *)
let empty_list1 : 'a. 'a list = []
let empty_list2 (type t) : t list = []
let empty_list3 : 'a. 'a list = empty_list2
let empty_list4 (type t) : t list = empty_list1

(* This fails as a type parameter is missing and can't be inferred from the context. *)
let[@type_error "not fully resolved symbol empty_list_if"] empty_list_if =
  if true then empty_list1 else empty_list2

(* It would in fact also happen if both arguments were the same. *)
let[@type_error "not fully resolved symbol empty_list_if_same1"] empty_list_if_same1 =
  if true then empty_list1 else empty_list1
let[@type_error "not fully resolved symbol empty_list_if_same2"] empty_list_if_same2 =
  if true then empty_list1 else empty_list2

(* A type annotation solves the issue. *)
let empty_list_if : 'a. 'a list = if true then empty_list1 else empty_list2

let empty_list_list_annot1 : 'a. 'a list list = [empty_list1; empty_list2]
let empty_list_list_annot2 (type t) : t list list = [empty_list1; empty_list2]
let[@type_error "not fully resolved symbol empty_list_list_unannot"] empty_list_list_unannot =
  [empty_list1; empty_list2]


(* Forces the unification of both its arguments.
 This function will be frequently used in the rest of this test file. *)
let unify (type t) (a : t) (b : t) = a


(* ############################################################ *)
(* Instance declaration *)

(* let one = __overload *)

let[@instance one] one_int : int = 1
let[@instance one] one_float : float = 1
let[@instance one] one_unit : unit = ()
let[@instance one] one_bool : bool = true

let example_one_int = one + (1 : int)
let example_one_float = one + 1.0
let example_one_unit = one ; ()
let example_one_bool : bool = one

let one_unit_if = if true then one
let one_bool_if = if one then "" else ""
let one_mixed_if = if one then one else 1.0

(* There is no instance of one on string. *)
let[@type_error "instances all mismatch for one"] one_string : string = one

(* However, we can later add one, and this type the same declaration works. *)
let[@instance one] _ = ("one" : string)
let one_string_after_declaration : string = one

let one_unify = unify one 1.0

let one_list = [one; 1.0]

let one_eq = one = ()

let rec one_rec (n : int) : unit =
  if n = 0 then one else one_rec (n - 1)

let rec one_rec_sym (n : int) : unit =
  if n = 0 then one_rec_sym (n - 1) else one

let one_pair = (one, ()) = (true, one)


(* let binop = __overload ~input:[true;true] *)

(* This is an non-recommended example in which the binary operator has very different
  meaning depending on its type. This is just for testing that it is indeed accepted. *)
let[@instance binop] _ = ((+) : int -> int -> int)
let[@instance binop] _ = ((-) : float -> float -> float)
let[@instance binop] _ = (fun () () -> true)
let[@instance binop] _ = fun (type t) (a : t) (l : t list) -> a :: l

let binop_int = binop 1 (1 : int)
let binop_float x y : float = binop x y
let binop_unit x y = if binop x y then true else false
let binop_cons = binop "" [""]

(* This leads to a stack overflow. Arthur thinks that it's because we should increase the stack size.

let[@type_error "not fully resolved symbol binop_none"] binop_none l = binop l l

*)

let binop_list1 (type a) x (l : a list) = binop x l
let binop_list2 (type a) x l : a list = binop x l

let binop_one = binop one (one : unit)

(* In this case, there are two matching instances: (fun () () -> true) and (fun a l -> a :: l). *)
let[@type_error "not fully resolved symbol binop_ambiguous"] binop_ambiguous x =
  binop () x

let binop_unify = unify int_add binop

(* This leads to a stack overflow. Arthur thinks that it's because we should increase the stack size.

let[@type_error "not fully resolved symbol binop_unify_ambiguous"] binop_unify_ambiguous x =
  unify (=) binop

*)

let binop_unify = [binop; float_add]
let binop_eq a = (binop = (fun b () -> a))

let binop_eq_ret x = (binop () x = true)

let rec binop_loop_rec (type t) (n : int) : unit list =
  binop one (binop_loop_rec (n - 1))

let binop_pair l a b =
  (binop ((), "") l, 1) = (binop a b, binop 1 2.0)


(* ############################################################ *)
(* Local instances *)

let scope1 =
  (*let x = __overload in*)
  let[@instance x] _ = 1. in
  let[@instance x] _ = "" in
  x + 1.

let scope2 =
  (*let x = __overload in*)
  let[@instance x] _ = () in
  let[@instance x] _ = 1.0 in
  unify x ()

let[@type_error "unbound variable x"] out_of_scope = unify x ()

let local_alias =
  let plus = (+) in
  plus 1 (2 : int)

(* Local aliases are not polymorph in our tool. *)
let[@type_error "mistyped application"] poly_local_alias =
  let plus = (+) in
  let x = plus 1 (2 : int) in
  let y = plus 3 (4 : float) in
  ()

let[@type_error "instances all mismatch for ( + )"] full_poly_local_alias =
  (* There are no instance of the symbol (+) with a full polymorph type,
    even when considering that it is only used for integers and floats. *)
  let plus (type a) : a -> a -> a = (+) in
  let x = plus 1 (2 : int) in
  let y = plus 3 (4 : float) in
  ()

let local_alias_2 =
  let plus1 = (+) in
  let plus2 = (+) in
  let x = plus1 1 (2 : int) in
  let y = plus2 3 (4 : float) in
  ()


(* ############################################################ *)
(* Conditionnal instances *)

let[@instance (^)] exponent = fun (type t)
    (_[@implicit_instance ( * )] : t -> t -> t)
    (_[@implicit_instance one] : t) ->
  let rec aux a (b : int) : t =
    if b = 0 then one
    else a * aux a (b - 1) in
  aux

let exponent_unit =
  let[@instance ( * )] _ = (fun () () -> ()) in
  () ^ 2
let exponent_int : int = 12 ^ 2
let exponent_float = 42. ^ 5
let string_concat = "abc" ^ "def"
let exponent_bool_local =
  let[@instance: int] _ = (fun (n : int) -> not (n = 0)) in
  let[@instance ( * )] _ = (&&) in
  true ^ 2


type[@ocaml] 'a array

(*let map = __overload ~input:[false;true]*)

let[@instance map] rec list_map (type a b) (f : a -> b) (l : a list) : b list =
  match l with
  | [] -> []
  | x :: l -> f x :: list_map f l


external array_map : 'a 'b. ('a -> 'b) -> 'a array -> 'b array = "OCaml" "Array" "map"

let[@instance map] _ = fun (type a) (type b) -> (array_map : (a -> b) -> _ -> _)

let array_list_map (type a) (type b) (f : a -> b) (l : a array list) =
  map (fun a -> map f a) l

(* TODO: More examples with map. *)


(* let map2 = __overload ~input:[false;true;true] *)

external array_map2 : 'a 'b 'c. ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array =
  "OCaml" "Array" "map2"

let[@instance map2] _ = fun (type t1) (type t2) (type t3) ->
  (array_map2 : (t1 -> t2 -> t3) -> _ -> _ -> _)


type 'a matrix = 'a array array

(* Given any addition operation over a ring, we can add matrices (assuming that they are
  of the same size).
  Here, for the purpose of testing, we accept addition over two different types.
  This is meant to be an instance of the type
    Forall (A : Type) (add : A -> A -> A), matrix A -> matrix A -> matrix A,
  generalised for three kinds of A. *)
let[@instance (+)] matrix_add (type t1 t2 t3)
    (add[@implicit (+)] : t1 -> t2 -> t3) m1 m2 : t3 matrix =
  map2 (fun a1 a2 -> map2 add a1 a2) m1 m2

(* We here present some variants based on the [@implicit_instance] attribute. *)
let local =

  let[@instance (+)] matrix_add1 (type t1 t2 t3)
      (add[@implicit_instance (+)] : t1 -> t2 -> t3) m1 m2 : t3 matrix =
    map2 (fun a1 a2 -> map2 (+) a1 a2) m1 m2 in

  let[@instance (+)] matrix_add2 (type t1 t2 t3)
      (_[@implicit_instance (+)] : t1 -> t2 -> t3) m1 m2 : t3 matrix =
    map2 (fun a1 a2 -> map2 (+) a1 a2) m1 m2 in

  let[@instance (+)] matrix_add3 (type t1 t2 t3)
      (add[@implicit (+)][@instance (+)] : t1 -> t2 -> t3) m1 m2 : t3 matrix =
    map2 (fun a1 a2 -> map2 (+) a1 a2) m1 m2 in

  let[@instance (+)] matrix_add4 (type t1 t2 t3)
      (_[@instance (+)][@implicit (+)] : t1 -> t2 -> t3) m1 m2 : t3 matrix =
    map2 (fun a1 a2 -> map2 (+) a1 a2) m1 m2 in

  ()

(* We can then instanciate the addition over matrices, and the typer will figure out the
  correct parameters. *)
let int_matrix_add : int matrix -> int matrix -> int matrix = (+)

(* And we can of course iterate. *)
let int_matrix_matrix_matrix_add : _ -> _ -> int matrix matrix matrix =
  (+)

(* But some combinations are not resolvable: here, there is no [(+) : bool -> bool -> bool] operator. *)
let[@type_error "instances all mismatch for ( + )"] bool_matrix_add : bool matrix -> bool matrix -> bool matrix = (+)

(* Similarly, there is no [(+) : int -> float -> int]. *)
let[@type_error "instances all mismatch for ( + )"] failing : int matrix -> float matrix -> int matrix = (+)

(* We can define such an operator locally, though. *)
let local_matrix_add : int matrix -> float matrix -> float matrix =
  let[@instance (+)] add_int_float (i : int) (f : float) : float =
    float_of_int i + f in
  (+)

let local_matrix_matrix_add : int matrix matrix -> _ -> float matrix matrix =
  let[@instance (+)] add_int_float (i : int) (f : float) : float =
    float_of_int i + f in
  (+)


(* TODO (Need pattern matching)
let prod (type a) (one : a) ((+) : a -> a -> a)
let sum (type a) (zero : a) ((+) : a -> a -> a)
*)


(* ############################################################ *)
(* Constructor overloading *)

type 'a option =
  | None
  | Some of 'a

type 'a sequence =
  | None
  | Cons of 'a * 'a sequence

let[@type_error "not fully resolved symbol none"] none = None

let none_option : int option = None
let none_sequence : int sequence = None

let different_nones = Cons ((None : int option), None)

let different_nones_nested : int option sequence =
  Cons (None, None)

let none_if = if true then None else Some ()

let none_if_pair = if true then (None, None) else (Cons ((), None), Some ())

let rec none_let_rec () : int sequence =
  Cons (1, if true then None else none_let_rec ())

let none_unify = unify None (Some 1.)
let none_list = [None; Some ""]

let none_eq =
  None = Cons (1., None)


let homopairs_none x =
  match x with
  | H (Some (a : int), None (* overloaded none. *)) -> a
  | _ -> 0

let homopairs_none x =
  match x with
  | H ((None : int option), None (* overloaded none. *)) -> "None"
  | _ -> "otherwise"


(* In practice, we implemented constructors as functions internally (as in Coq).
  We could  *)
let functional_cons : int * int sequence -> int sequence = Cons


type ('a, 'b) either =
  | A of 'a
  | B of 'b

type 'a either2 = ('a, 'a) either

let either_ret : (_, unit) either = A ""
let either_ret2 : int either2 = A 1
let either_ret2_var : _ either2 = A ()

let either_if = if true then A "" else B ()
let either_unify = unify (A "") (B 1.0)
let either_list = [A 1.0; B ""]
let either_eq_if =
  if A () = B "" then A 1
  else if true then B ""
  else A 1.

(* There is an ambiguity on the [1] provided to [A]: it could be [int] or [float].
  But the annotation [_ either2] forces both types ['a] and ['b] in [('a, 'b) either].  *)
let either_eq = A 1 = (B 1. : _ either2)

let[@type_error "instances all mismatch for (.,.)"] rec either_rec_mis
    (type a) (type b) (x : a) : (a, b) either =
  if true then A x
  else either_rec_mis (x, "")

let rec either_rec
    (type a) (type b) (x : a) : (a, b) either =
  if true then A x
  else either_rec x


(* ############################################################ *)
(* Records overloading *)

type record_a = { a : int }
type record_ab = { a : int ; b : int }
type record_abc = { a : int ; b : int ; c : int }

let defining_record_a = { a = 1 }
let defining_record_ab = { a = 1 ; b = 1 }
let defining_record_abc = { a = 1 ; b = 1 ; c = 1 }

let record_with_c x = { x with c = 1 }
let record_with_ac x = { x with a = 1 ; c = 1 }
let record_with_abc x = { x with a = 1 ; b = 1 ; c = 1 }

let projection_c x = x.c
let projection_with_c x = { x with c = x.c }
let[@type_error "not fully resolved symbol projection_a"] projection_a x = x.a

let projection_a_with_annotation (x : record_ab) = x.a

let[@type_error "not fully resolved symbol record_with_a"] record_with_a x =
  { x with a = 1 }
let[@type_error "not fully resolved symbol record_with_ab"] record_with_ab x =
  { x with a = 1 ; b = 1 }

type record_a_float = { a : float }
type record_ab_float = { a : int ; b : float }
type record_abc_float = { a : float ; b : int ; c : float }

let defining_record_a_float = { a = 1.0 }
let defining_record_ab_float = { a = 1 ; b = 1.0 }
let defining_record_abc_float = { a = 1 ; b = 1 ; c = 1.0 }
let defining_record_abc_float2 = { a = 1.0 ; b = 1 ; c = 1 }

let[@type_error "not fully resolved symbol defining_record_a_int_float"] defining_record_a_int_float =
  { a = 1 (* Could be float or int *) }
let[@type_error "not fully resolved symbol defining_record_abc_int_float"] defining_record_abc_int_float =
  { a = 1 ; b = 1 ; c = 1 }

let record_with_c_float x = { x with c = 1.0 }
let record_with_b_float x = { x with b = 1.0 }

let[@type_error "not fully resolved symbol record_with_a1"] record_with_a1 x =
  { x with a = 1 ; b = 1 }
let[@type_error "not fully resolved symbol record_with_a2"] record_with_a2 x =
  { x with a = 1.0 }

let projection_c_float x = [x.c; 1.0]
let projection_b_float x = if x.b = 1.0 then "a" else "b"
let[@type_error "not fully resolved symbol projection_a_float"] projection_a_float x = [x.a; 1.0]

let projection_c_float_ret x : float = x.c
let projection_c_int_ret x : int = x.c

let record_with_ret x : record_abc = { x with a = 1 }
let record_ret : record_a_float = { a = 1 }

type 'a poly_record_poly = { poly : 'a }
type 'a poly_record_poly2 = { poly : 'a * 'a }

let defining_record_poly = { poly = "" }
let[@type_error "not fully resolved symbol defining_record_poly2"] defining_record_poly2 =
  { poly = ("", "") }
let[@type_error "not fully resolved symbol defining_record_poly_arg"] defining_record_poly_arg a =
  { poly = a }

let defining_record_poly_ret : _ poly_record_poly = { poly = ("", "") }

let record_with_poly x = { x with poly = "" }
let[@type_error "not fully resolved symbol record_with_poly2"] record_with_poly2 x = { x with poly = ("", "") }

let record_field_poly x : string = x.poly
let[@type_error "not fully resolved symbol record_field_poly2"] record_field_poly2 x : string * string = x.poly

let defining_record_poly_nested = { poly = { poly = { poly = { poly = () } } } }

let record_with_poly_nested x =
  { x with poly = { x.poly with poly = { x.poly.poly with poly = { x.poly.poly.poly with poly = () } } } }


(* ############################################################ *)
(* Cyclic types *)

let () =
  let[@instance g] g1 = fun (type a) (x : a) (y : a) -> x in
  let[@instance g] g2 = fun (type a) (x : a) (y : a -> a) -> x in
  let f1 (type a) (v:a) = g v v in  (* only g1 applies *)
  let f2 (type a) (v:a) = g v (fun (r:a) -> r) in  (* only g2 applies *)
  ()

