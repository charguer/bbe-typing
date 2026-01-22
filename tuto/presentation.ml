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

(* Our language (idealized syntax) *)
let double_table_apply t1 t2 k1 f =
  if   ((find_opt t1 k1) is Some ?k2)
    && ((find_opt t2 k2) is Some ?v) then f v
  else a_big_expression

(* space *)

(* -------------------------------------------------------------- *)
(* binding in while loop conditions *)

(* Regular OCaml *)
let demo_pop q =
  while not (Queue.empty q) do
    let x = Queue.pop q in
    ...
  done

(* space *)

(* Our language (idealized syntax) *)
let demo_pop q =
  while ((Queue.take_opt q) is (Some ?x)) do
    ...
  done

(* space *)

(* -------------------------------------------------------------- *)
(* Views = active patterns = smart deconstructors *)

type trm =
  | Trm_bool of bool
  | Trm_if of trm * trm * trm
  | ...

(* Automatically generated curried-style constructors *)
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
  | trm_if t1 t2 t3 -> ... (* feasible using Rocq-style syntax *)
  | trm_and t1 t2 -> ... (* desirable yet not possible *)

(* We need to register custom smart deconstructors that can appear in patterns *)

(* Automatically generated smart deconstructors *)
let __pattern_trm_bool (t : trm) : bool option =
  match t with Trm_bool ?b -> Some b | _ -> None
let __pattern_trm_if (t : trm) : (trm * trm * trm) option =
  match t with Trm_if (?t1, ?t2, ?t3) -> Some (t1, t2, t3) | _ -> None

(* User-provided smart constructor for [trm_and].
   In pattern position, [trm_if] is interpreted as [__pattern_trm_if]. *)
let __pattern_trm_and (t : trm) : (trm * trm) option =
  if t is (trm_if (?t1, ?t2, trm_bool false)) then Some (t1, t2) else None

(* Example use case of [pattern_trm_and], to match [t1 && t2 && t3] *)

let demo_trm_and (t : trm) =
  if t is (   trm_and (?t1, trm_and (?t2,?t3))
           || trm_and (trm_and (?t1,?t2), ?t3))
    then ..
    else ..


(* -------------------------------------------------------------- *)
(* implementing bucket lookup with views *)

(* Code OCaml *)
type ('a, 'b) bucket =
    Nil
  | Cons of 'a * 'b * ('a, 'b) bucket

let rec lookup_table_opt k t =
  match t with
  | Cons (k1, v, _) when k = k1 -> Some v
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
(* implementing bucket lookup with views *)

(* recall binding boolean expression version *)
let double_table_apply t1 t2 k1 f =
  if   (find_opt t1 k1 is Some ?k2)
    && (find_opt t2 k2 is Some ?v)
    then f v
    else a_big_expression

(* space *)

(* we see the second find_opt as a view that filters k2 *)
let double_table_apply_v2 t1 t2 k1 f =
  if   (find_opt t1 k1)
    is Some (?!(find_opt t1 _) ?v)
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
4. ouptut OCaml parse-tree


*)


(* -------------------------------------------------------------- *)
(* montrer la version parsable en dessous *)

(* Montrer les features qu'on trouve importantes. *)

(* Liste des features:
- pattern matching à l'intérieur du if, '&&' pour les enchaîner
  -> ou à l'interieur à la place de n'importe quelle condition (comme un while par exemple)
- switch, qui agit comme une cascade de if, version plus expressive du match
- prédicats
- fonctions d'inversion
- application partielle
*)

(* - pattern matching à l'intérieur du if, '&&' pour les enchaîner
 *)



(*
Work plan:
1. Present examples of OCaml code that have restrictons. The big example would be the big hashtable get thing
2. Quickly show the syntax, and some examples
  -> Very quickly show the idea of having booleans that can bind
3. Go from the OCaml definition, and move it to our syntax
  -> would be a good demonstration *)

(* Liste des features:
- pattern matching à l'intérieur du if, '&&' pour les enchaîner
  -> ou à l'interieur à la place de n'importe quelle condition (comme un while par exemple)
- switch, qui agit comme une cascade de if, version plus expressive du match
- prédicats
- fonctions d'inversion
- application partielle
*)

(* Finir avec une demo de 4 minutes disons, où on présente la version qu'on a écrite, potentiellement avec les différentes versions? Et le script. *)


(* Feature focuses 1 and 2 *)

(* Feature focus 1 *)


(* let queue_while_pop q f =
  while ((list_pop_opt q) @_is (Some ??x)) do
    f x
  done

(* With smart inversion: *)
let queue_while_pop q f =
  while (q @_is (list_pop_opt ??x)) do
    f x
  done
 *)
(* Feature focus 2 *)
external a_big_expression : int = ""
external list_mem : int list -> int -> bool = ""
external list_get_opt : int list -> int -> int option = ""

(* let ocaml_hashtable_get1 tbl r f =
  match r with
  | Some k when list_mem tbl k ->
    let v = list_get_opt tbl k in
    f v
  | _ ->
    a_big_expression

let ocaml_hashtable_get2 tbl r f =
  let cont () =
    a_big_expression in
  match r with
  | Some k ->
    begin match list_get_opt tbl k with
      | Some v -> f v
      | _ -> cont ()
    end
  | _ ->
    cont ()
 *)



(* key -> value option *)

(* Some ??k2 @_when k2 @_is (?!(lookup_table_opt __ t2) ??v) *)


(* Avant cet exemple, j'aimerais avoir montré l'application partielle. Mais je pense qu'il faudrait avoir un exemple de code compilé pour ça? *)
let ocaml_double_table_apply t1 t2 k1 f =
  if (lookup_table_opt k1 t1) @_is (Some ??k2) && (lookup_table_opt k2 t2) @_is (Some ??v) then
    f v
  else a_big_expression

(* let ocaml_double_table_apply t1 t2 k1 f =
  if (lookup_table_opt k1 t1) @_is (Some ((?!(lookup_table_opt __ t2) ??v))) then
    f v
  else a_big_expression
 *)


(* TODO YL: replace with 2 get opts, with two tables, in all of the above examples  *)
let hashtable_get h1 h2 k1 f =
  if (r @_is (Some ??k2)) && ((list_get_opt tbl k2) @_is (Some ??v))
    then f v
    else a_big_expression

(* switch *)

(*
- feature focus.
- easy "bbe is" examples
  -> works with constructors (as expected)
- accepts boolean operations such as "&&", "||" or "not"
- native handling of predicates
- inversor functions
-
*)

(* boolean pattern matching test *)

(* "is" works against constructors *)
external (+) : int -> int -> int = ""
external (=) : int -> int -> bool = ""
external (mod) : int -> int -> int = ""

let tuple_bind1 = if (2,3) @_is (??x, ??y) then x + y else -1
let tuple_bind2 = if (2,3,4) @_is (??x1, ??x2, ??x3) then x1 + x3 else -1

let option_pat_bind = if (Some 2) @_is (Some ??x) then x else 0

(* test predicates in pattern position *)


let even n = ((n mod 2) = 0)

let pred_inv k = if k @_is even then 1 else 0

(* Limitation 1: the constructors have to be input by hand (either by predefinition from us, or typedef from the user). This means that not all constructors are accepted. This comes from the fact that the typer looks for constructor inversors, which do not necessarily exist. (this is a work for later, but the "__pattern_" form could be removed, and made into a simple verification that the term is a constructor variable) *)

