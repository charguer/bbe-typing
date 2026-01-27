(* Extending OCaml's Pattern Matching with binding boolean expressions *)
(* Yanni Lefki and Arthur Charguéraud *)

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

(* space *)

(* In our language (in an idealized syntax, not our PPX syntax) *)
let double_table_apply t1 t2 k1 f =
  if   ((find_opt t1 k1) is Some ?k2)
    && ((find_opt t2 k2) is Some ?v) then f v
  else a_big_expression

(* space *)

(* -------------------------------------------------------------- *)
(* binding in while loop conditions *)

(* Regular OCaml *)
let demo_pop q f =
  while not (Queue.empty q) do
    let x = Queue.take q in
    f x
  done

(* space *)

(* Our language (idealized syntax) *)
let demo_pop q f =
  while ((Queue.take_opt q) is (Some ?x)) do
    f x
  done

(* space *)

(* -------------------------------------------------------------- *)
(* Views = active patterns = smart deconstructors *)

type trm =
  | Trm_bool of bool
  | Trm_if of trm * trm * trm
  | ...

(* Let us present constructors as functions for uniformity with smart constructors *)
let trm_bool (b : bool) = Trm_bool b
let trm_if t1 t2 t3 = Trm_if (t1, t2, t3)

(* Derived construction = smart constructors
   for [t1 && t2] encoded as [if t1 then t2 else false] *)
let trm_and t1 t2 = trm_if t1 t2 (trm_bool false)

(* Client code can use [trm_and] as if it was a builtin constructor like [trm_if] *)

(* But it's not true for pattern matching *)
let demo_match t =
  match t with
  | Trm_if (t1,t2,t3) -> ... (* standard ocaml *)
  | trm_and t1 t2 -> ... (* desirable yet not possible *)

(* We need to register custom smart deconstructors that can appear in patterns *)

(* In pattern position, [trm_bool] is interpreted as [__pattern_trm_bool]. *)

let __pattern_trm_bool (t : trm) : bool option =
  match t with Trm_bool ?b -> Some b | _ -> None

let __pattern_trm_if (t : trm) : (trm * trm * trm) option =
  match t with Trm_if (?t1, ?t2, ?t3) -> Some (t1, t2, t3) | _ -> None

let __pattern_trm_and (t : trm) : (trm * trm) option =
  if t is (trm_if (?t1, ?t2, trm_bool false)) then Some (t1, t2) else None

(* Example use case of [pattern_trm_and], to match both
   [(t1 && t2) && t3] and [t1 && (t2 && t3)] *)

let demo_trm_and (t : trm) =
  if t is (   trm_and (?t1, trm_and (?t2,?t3))
           || trm_and (trm_and (?t1,?t2), ?t3))
    then ..
    else ..


(* -------------------------------------------------------------- *)
(* Implementing bucket lookup with predicates *)

type ('a, 'b) bucket =
    Nil
  | Cons of 'a * 'b * ('a, 'b) bucket

(* Code idiomatique OCaml *)
let rec lookup_table_opt k t =
  match t with
  | Cons (k1, v, _) when k = k1 -> Some v
  | Cons (_, _, t') -> lookup_table_opt k t'
  | _ -> None

(* Same using predicate patterns: boolean functions *)
(* [?!(f _ a _)] is short for [fun x y -> f x a y] *)
(* In particular, [?!(_ = k)] is short for [fun x -> x = k] *)

let rec lookup_table_opt (k : int) (t : (int, int) bucket) : int option =
  match t with
  | Cons (?!(_ = k), ?v, _) -> Some v
  | Cons (_, _, ?t') -> lookup_table_opt k t'
  | _ -> None
  end

(* -------------------------------------------------------------- *)
(* Same with a switch *)

let rec lookup_table_opt (k : int) (t : (int, int) bucket) : int option =
  switch
    case t is Cons (?!(_ = k), ?v, _) then Some v
    case t is Cons (_, _, ?t') then lookup_table_opt k t'
    case true then None
  end

(* Same with cascade of if *)

let rec lookup_table_opt (k : int) (t : (int, int) bucket) : int option =
  if t is Cons (?!(_ = k), ?v, _) then Some v
  else if t is Cons (_, _, ?t') then lookup_table_opt k t'
  else if true then None
  else raise Match_failure

(* -------------------------------------------------------------- *)
(* Exploring expressiveness: revisiting bucket lookup with views *)

(* recall binding boolean expression version *)
let double_table_apply t1 t2 k1 f =
  if   (find_opt t1 k1 is Some ?k2)
    && (find_opt t2 k2 is Some ?v)
    then f v
    else a_big_expression

(* space *)

(* we see the second find_opt as a view that filters k2 *)
let double_table_apply_v2 t1 t2 k1 f =
  if (find_opt t1 k1) is Some (?!(find_opt t1 _) ?v)
    then f v
    else a_big_expression

(* space *)

(* symetrically we can see both find_opt as views *)
let double_table_apply_v3 t1 t2 k1 f =
  if k1 is ?!(find_opt __ t1) (?!(find_opt __ t2) ??v)
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
4. output OCaml parse-tree
*)
