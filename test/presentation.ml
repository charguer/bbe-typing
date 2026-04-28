(* -------------------------------------------------------------- *)
(* binding in boolean expressions (rust if-let) *)

 let find_opt = Hashtbl.find_opt

(* Regular OCaml *)
let double_table_apply t1 t2 k1 f =
  let cont () =
    a_big_expression in
  match find_opt t1 k1 with
  | Some k2 ->
    begin match find_opt t2 k2 with
      | Some v -> f v
      | _ -> cont ()
    end
  | _ -> cont ()



(* Our language (idealized syntax) *)
let double_table_apply t1 t2 k1 f =
  if   ((find_opt t1 k1) is Some ?k2)
    && ((find_opt t2 k2) is Some ?v) then f v
  else a_big_expression


























(* -------------------------------------------------------------- *)
(* binding in while loop conditions *)

(* Regular OCaml *)
let demo_pop q =
  while not (Queue.empty q) do
    let x = Queue.pop q in
    ...
  done

(* Our language (idealized syntax) *)
let demo_pop q =
  while ((Queue.take_opt q) is (Some ?x)) do
    ...
  done























(* -------------------------------------------------------------- *)
(* Views = active patterns = smart deconstructors *)

type trm =
  | Trm_bool of bool
  | Trm_if of trm * trm * trm
  | ...

(* Curried-style smart constructors *)
let trm_bool b = Trm_bool b
let trm_if t1 t2 t3 = Trm_if (t1, t2, t3)

(* Derived smart constructor
   for [t1 && t2] encoded as [if t1 then t2 else false] *)
let trm_and t1 t2 = trm_if t1 t2 (trm_bool false)

(* Client code can use [trm_and] as if it was a builtin constructor like [trm_if] *)



(* But it's not true for pattern matching *)
let demo_match t =
  match t with
  | Trm_if (t1,t2,t3) -> ... (* standard ocaml *)
  | trm_if t1 t2 t3 -> ... (* feasible using Rocq-style syntax *)
  | trm_and t1 t2 -> ... (* desirable yet not possible *)

(* We want smart constructors to be able to appear in pattern position *)

(* Automatically generated smart deconstructors *)
(* In pattern position, [trm_if] is interpreted as [__pattern_trm_if]. *)
let __pattern_trm_bool (t : trm) : bool option =
  match t with Trm_bool ?b -> Some b | _ -> None
let __pattern_trm_if (t : trm) : (trm * trm * trm) option =
  match t with Trm_if (?t1, ?t2, ?t3) -> Some (t1, t2, t3) | _ -> None

(* space *)

(* User-provided smart constructor for [trm_and]. *)
let __pattern_trm_and (t : trm) : (trm * trm) option =
  if t is (trm_if ?t1 ?t2 (trm_bool false)) then Some (t1, t2) else None

(* space *)

(* Example use case of [__pattern_trm_and], to match [t1 && t2 && t3] *)
let __pattern_trm_and_3 (t : trm) =
  if t is (   trm_and ?t1 (trm_and ?t2 ?t3)
           || trm_and (trm_and ?t1 ?t2) ?t3)
    then Some (t1, t2, t3)
    else None




























(* -------------------------------------------------------------- *)
(* implementing bucket lookup with views *)

(* instead of using hashtables, we use instead bucket *)
(* Code OCaml *)
type ('a, 'b) bucket =
    Nil
  | Cons of 'a * 'b * ('a, 'b) bucket

let rec lookup_table_opt k t =
  match t with
  | Cons (k1, v, _) when k = k1 -> Some v (* standard ocaml *)
  | Cons ((fun k -> k = k1), v, _) -> Some v (* desirable boolean predicate notation in pattern position *)
  | Cons (?!(_ = k1), v, _) -> Some v (* with partial application notation *)
  (* where ?!(f _ a2 _)  = (fun x1 x3 -> f x1 a2 x3) *)
  | Cons (_, _, t') -> lookup_table_opt k t'
  | _ -> None

(* Our language (idealized syntax) *)
let rec lookup_table_opt (k : int) (t : (int, int) bucket) : int option =
  switch
    case t is Cons (?!(_ = k), ?v, _) then Some v
    case t is Cons (_, _, ?t') then lookup_table_opt k t'
    case true then None
  end








































(* -------------------------------------------------------------- *)
(* refining double lookup function with views *)

(* recall binding boolean expression version *)
let double_table_apply t1 t2 k1 f =
  if   (find_opt t1 k1 is Some ?k2)
    && (find_opt t2 k2 is Some ?v)
    then f v
    else a_big_expression







































(* we see the second find_opt as a view that filters k2 *)
let double_table_apply_v2 t1 t2 k1 f =
  if   (find_opt t1 k1)
    is Some (?!(find_opt t2 _) ?v)
    then f v
    else a_big_expression







































(* symetrically we can see both find_opt as views *)
let double_table_apply_v3 t1 t2 k1 f =
  if k1 is ?!(find_opt __ t1) (?!(find_opt __ t2) ?v)
    then f v
    else a_big_expression







































(* -------------------------------------------------------------- *)
(* Demo tooling *)

(* PATH 1, standalone transpiler

1. this code, in a hacky ocaml-compatible syntax is parsed using ocaml-parser
2. typecheck in ML with BBE bindings (our typechecker)
3. compile BBE constructs away
4. output standard OCaml code in text file

PATH 2, packaged in a PPX

1. this code parsed, as in path 1
2. variable bindings are checked, but not full typechecking
3. compile BBE constructs away, as in path 1
4. ouptut OCaml parse-tree
*)
