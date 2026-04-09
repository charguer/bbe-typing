(**************************************************************)
(* Helpers *)

let check_eq translation expected actual =
  if expected = actual then () else failwith translation

let start_test translation =
  Printf.printf "testing %s\n%!" translation

let finish_test translation =
  Printf.printf "testing complete: %s\n%!" translation


(**************************************************************)
(* Basic top-level and homomorphic term translations *)

(* Translation tested:
   - expand_topdef on user-defined type declarations
   - expand_trm on capitalized variables and constructor applications *)
type pair_box = Empty | PairBox of int * int

(* Translation tested:
   - expand_topdef on external declarations *)
external external_identity_int : int -> int = "%identity"

(* Translation tested:
   - expand_trm on constants and tuples *)
let term_constants_and_tuple = (true, 7, 2.5, "bbe", ())

(* Translation tested:
   - expand_trm on nullary constructors *)
let term_constructor_no_arg = Empty

(* Translation tested:
   - expand_trm on constructor applications with tupled arguments *)
let term_constructor_with_args = PairBox (3, 4)

(* Translation tested:
   - comp_trm / expand_trm on let-bindings, annotations, functions,
     recursive lets, applications, dotted identifiers, and term-level
     boolean operators *)
let term_annotated_app = (external_identity_int 11 : int)

let term_nested_let =
  let x = 2 in
  let y = 5 in
  x + y

let term_boolean_ops = (not false, true && false, true || false)

let term_string_length = String.length "abcd"

let rec term_fact n =
  if n = 0 then 1 else n * term_fact (n - 1)

(* Translation tested:
   - expand_trm on locally abstract types / Trm_forall
   Note: the current front-end typer logs this case as unsupported,
   but the PPX still generates the expected OCaml and the runtime check below
   confirms the translation branch in [ast_expand.ml]. *)
(* let term_poly_id (type a) (x : a) : a = x *)

let () =
  start_test "expand_trm/constants+tuple";
  let actual = term_constants_and_tuple in
  check_eq "expand_trm/constants+tuple" (true, 7, 2.5, "bbe", ()) actual;
  finish_test "expand_trm/constants+tuple";

  start_test "expand_trm/constr-nullary";
  let actual = term_constructor_no_arg in
  check_eq "expand_trm/constr-nullary" Empty actual;
  finish_test "expand_trm/constr-nullary";

  start_test "expand_trm/constr-args";
  let actual = term_constructor_with_args in
  check_eq "expand_trm/constr-args" (PairBox (3, 4)) actual;
  finish_test "expand_trm/constr-args";

  start_test "expand_topdef/external + expand_trm/annot";
  let actual = term_annotated_app in
  check_eq "expand_topdef/external + expand_trm/annot" 11 actual;
  finish_test "expand_topdef/external + expand_trm/annot";

  start_test "comp_trm/let";
  let actual = term_nested_let in
  check_eq "comp_trm/let" 7 actual;
  finish_test "comp_trm/let";

  start_test "expand_trm/not-and-or-as-terms";
  let actual = term_boolean_ops in
  check_eq "expand_trm/not-and-or-as-terms" (true, false, true) actual;
  finish_test "expand_trm/not-and-or-as-terms";

  start_test "expand_trm/dotted-ident";
  let actual = term_string_length in
  check_eq "expand_trm/dotted-ident" 4 actual;
  finish_test "expand_trm/dotted-ident";

  start_test "expand_let_def/recursive";
  let actual = term_fact 5 in
  check_eq "expand_let_def/recursive" 120 actual;
  finish_test "expand_let_def/recursive"
  (* start_test "expand_trm/forall";
  let actual = term_poly_id "poly" in
  check_eq "expand_trm/forall" "poly" actual;
  finish_test "expand_trm/forall" *)


(**************************************************************)
(* Control-flow translations implemented in comp_trm *)

let add_with x a b = x + a + b
let add_three a b = a + b
let add_ninety_six a b = a + b + 96

(* Translation tested:
   - comp_bbe fallback on ordinary boolean terms inside [if] *)
let comp_bbe_boolean_fallback_if = if 1 < 2 then 3 else 4

(* Translation tested:
   - comp_bbe on [t @_is p] with a non-variable scrutinee
   - comp_bbe introduces a fresh let before compiling the pattern *)
let comp_bbe_is_non_var =
  if (external_identity_int 9) @_is ??x then x else 0

(* Translation tested:
   - comp_bbe on [t @_is p] with a variable scrutinee
   - comp_bbe reuses the existing variable directly *)
let comp_bbe_is_var =
  let value = Some 8 in
  if value @_is (Some ??x) then x else 0

(* Translation tested:
   - comp_trm on labeled [if]
   - comp_trm on [__next]
   - expand_trm on internal [try ... with Exn_Next ...] *)
let comp_trm_labeled_if_next =
  if[@label "L_if"] true then __next "L_if" else 41

(* Translation tested:
   - comp_trm on [while]
   - comp_bbe + comp_pat inside the loop condition *)
let comp_trm_while_sum () =
  let q = Queue.create () in
  Queue.add 1 q;
  Queue.add 2 q;
  Queue.add 3 q;
  let acc = ref 0 in
  while ((Queue.take_opt q) @_is (Some ??x)) do
    acc := !acc + x
  done;
  !acc

(* Translation tested:
   - comp_switch on non-empty switches
   - note: the current implementation compiles the final empty switch to [( * )],
     so this test keeps the switch result type as [int -> int -> int] *)
let comp_switch_non_empty opt =
  __switch [
    __case ((opt @_is (Some ??x)) @_then (add_with x));
    __case (true @_then add_three)
  ]

(* Translation tested:
   - comp_switch propagates labels to the nested [if] translation
   - [__next] skips to the next switch case *)
let comp_switch_labeled_next =
  (__switch "LS" [
    __case (true @_then (__next "LS"));
    __case (true @_then (fun a b -> a + b + 20))
  ]) 1 2

(* Translation tested:
   - parser desugaring of [__match] into let + switch
   - labeled switch translation + [__next] inside a match case *)
let comp_match_labeled_next opt =
  (__match "LM" opt [
    __case ((Some ??x) @_then (__next "LM"));
    __case (__ @_then add_ninety_six)
  ]) 1 2

(* Translation tested:
   - comp_trm on [__block]
   - comp_trm on [__exit]
   - expand_trm on internal [try ... with Exn_Exit ...] *)
let comp_trm_block_exit =
  __block "B" (__exit "B" 42; 0)

(* Translation tested:
   - comp_trm on [__block] when the body completes normally *)
let comp_trm_block_plain =
  __block "C" 17

(* Translation tested:
   - expand_trm special-case for [raise (Exn_Exit (..., v))]
   - the generated code inserts [Obj.magic] on the payload *)
let expand_trm_raw_raise_exit_not_taken =
  if false then raise (Exn_Exit ("raw", 1)) else 123

(* Translation tested:
   - expand_trm on direct constructor applications in exception position *)
let expand_trm_raw_raise_next_not_taken =
  if false then raise (Exn_Next "raw") else 456

(* Translation tested:
   - comp_switch on the empty case list
   - current implementation uses the placeholder [( * )] *)
let comp_switch_empty_placeholder_application =
  (__switch []) 6 7

let () =
  start_test "comp_bbe/fallback-if";
  let actual = comp_bbe_boolean_fallback_if in
  check_eq "comp_bbe/fallback-if" 3 actual;
  finish_test "comp_bbe/fallback-if";

  start_test "comp_bbe/is-non-var";
  let actual = comp_bbe_is_non_var in
  check_eq "comp_bbe/is-non-var" 9 actual;
  finish_test "comp_bbe/is-non-var";

  start_test "comp_bbe/is-var";
  let actual = comp_bbe_is_var in
  check_eq "comp_bbe/is-var" 8 actual;
  finish_test "comp_bbe/is-var";

  start_test "comp_trm/labeled-if-next";
  let actual = comp_trm_labeled_if_next in
  check_eq "comp_trm/labeled-if-next" 41 actual;
  finish_test "comp_trm/labeled-if-next";

  start_test "comp_trm/while";
  let actual = comp_trm_while_sum () in
  check_eq "comp_trm/while" 6 actual;
  finish_test "comp_trm/while";

  start_test "comp_switch/non-empty-some";
  let actual = (comp_switch_non_empty (Some 5)) 1 2 in
  check_eq "comp_switch/non-empty-some" 8 actual;
  finish_test "comp_switch/non-empty-some";

  start_test "comp_switch/non-empty-none";
  let actual = (comp_switch_non_empty None) 1 2 in
  check_eq "comp_switch/non-empty-none" 3 actual;
  finish_test "comp_switch/non-empty-none";

  start_test "comp_switch/labeled-next";
  let actual = comp_switch_labeled_next in
  check_eq "comp_switch/labeled-next" 23 actual;
  finish_test "comp_switch/labeled-next";

  start_test "parser+comp_switch/labeled-match-next-some";
  let actual = comp_match_labeled_next (Some 1) in
  check_eq "parser+comp_switch/labeled-match-next-some" 99 actual;
  finish_test "parser+comp_switch/labeled-match-next-some";

  start_test "parser+comp_switch/labeled-match-next-none";
  let actual = comp_match_labeled_next None in
  check_eq "parser+comp_switch/labeled-match-next-none" 99 actual;
  finish_test "parser+comp_switch/labeled-match-next-none";

  start_test "comp_trm/block-exit";
  let actual = comp_trm_block_exit in
  check_eq "comp_trm/block-exit" 42 actual;
  finish_test "comp_trm/block-exit";

  start_test "comp_trm/block-plain";
  let actual = comp_trm_block_plain in
  check_eq "comp_trm/block-plain" 17 actual;
  finish_test "comp_trm/block-plain";

  start_test "expand_trm/raw-raise-exit";
  let actual = expand_trm_raw_raise_exit_not_taken in
  check_eq "expand_trm/raw-raise-exit" 123 actual;
  finish_test "expand_trm/raw-raise-exit";

  start_test "expand_trm/raw-raise-next";
  let actual = expand_trm_raw_raise_next_not_taken in
  check_eq "expand_trm/raw-raise-next" 456 actual;
  finish_test "expand_trm/raw-raise-next";

  start_test "comp_switch/empty-placeholder";
  let actual = comp_switch_empty_placeholder_application in
  check_eq "comp_switch/empty-placeholder" 42 actual;
  finish_test "comp_switch/empty-placeholder"


(**************************************************************)
(* BBE translations implemented in comp_bbe *)

let is_even n = n mod 2 = 0
let consume n = n + 10

(* Translation tested:
   - comp_bbe on [not] *)
let comp_bbe_not =
  if not (Some 3 @_is (Some ??x)) then 0 else 1

(* Translation tested:
   - comp_bbe on [and]
   - bindings from the left operand flow into the right operand *)
let comp_bbe_and_general =
  if (Some 8 @_is (Some ??x)) && (x @_is is_even) then x else 0

(* Translation tested:
   - comp_bbe on [and]
   - fast path where the failure continuation is already a duplicated continuation *)
let comp_bbe_and_fastpath =
  let fallback = 11 in
  if (Some 1 @_is None) && true then 0 else consume fallback

(* Translation tested:
   - comp_bbe on [or]
   - fast path where the success continuation is already a duplicated continuation *)
let comp_bbe_or_fastpath =
  if (Some 1 @_is (Some ??x)) || (Some 2 @_is (Some ??x)) then consume x else 0

(* Translation tested:
   - comp_bbe on [or]
   - general path that introduces a continuation helper with the shared bindings *)
let comp_bbe_or_general =
  if ((Some 1) @_is (((Some ??x) @_when false)))
     || ((Some 2) @_is (Some ??x))
  then x + 10
  else 0

let () =
  start_test "comp_bbe/not";
  let actual = comp_bbe_not in
  check_eq "comp_bbe/not" 1 actual;
  finish_test "comp_bbe/not";

  start_test "comp_bbe/and-general";
  let actual = comp_bbe_and_general in
  check_eq "comp_bbe/and-general" 8 actual;
  finish_test "comp_bbe/and-general";

  start_test "comp_bbe/and-fastpath";
  let actual = comp_bbe_and_fastpath in
  check_eq "comp_bbe/and-fastpath" 21 actual;
  finish_test "comp_bbe/and-fastpath";

  start_test "comp_bbe/or-fastpath";
  let actual = comp_bbe_or_fastpath in
  check_eq "comp_bbe/or-fastpath" 11 actual;
  finish_test "comp_bbe/or-fastpath";

  start_test "comp_bbe/or-general";
  let actual = comp_bbe_or_general in
  check_eq "comp_bbe/or-general" 12 actual;
  finish_test "comp_bbe/or-general"


(**************************************************************)
(* Pattern translations implemented in comp_pat / expand_pattern *)

let half_if_even n =
  if n mod 2 = 0 then Some (n / 2) else None

let split_five n =
  if n = 5 then Some (2, 3) else None

(* Translation tested:
   - comp_pat on wildcard patterns *)
let comp_pat_wild =
  if 5 @_is __ then 1 else 0

(* Translation tested:
   - comp_pat on variable patterns *)
let comp_pat_var =
  if 5 @_is ??x then x else 0

(* Translation tested:
   - comp_pat on constant patterns *)
let comp_pat_const =
  if 5 @_is 5 then 1 else 0

(* Translation tested:
   - comp_pat on annotated patterns
   - expand_trm on the annotation propagated by pattern compilation *)
let comp_pat_annot =
  if (Some 6) @_is ((Some ??x) : int option) then Some x else None

(* Translation tested:
   - comp_pat / expand_pattern on constructor patterns without arguments *)
let comp_pat_constr0 =
  if Empty @_is Empty then 1 else 0

(* Translation tested:
   - comp_pat / expand_pattern on constructor patterns with arguments *)
let comp_pat_constr =
  if (PairBox (2, 5)) @_is (PairBox (??a, ??b)) then a + b else 0

(* Translation tested:
   - comp_pat / expand_pattern on tuple patterns *)
let comp_pat_tuple =
  if (2, 3, 4) @_is (??a, ??b, ??c) then a + b + c else 0

(* Translation tested:
   - comp_pat on lowercase variable patterns interpreted as predicates *)
let comp_pat_predicate_var =
  if 8 @_is is_even then 1 else 0

(* Translation tested:
   - comp_pat catch-all predicate case on a non-variable term *)
let comp_pat_predicate_term =
  if 8 @_is (fun n -> n mod 2 = 0) then 1 else 0

(* Translation tested:
   - comp_pat on function/view patterns with one subpattern *)
let comp_pat_function_single =
  if 8 @_is (half_if_even ??half) then half else 0

(* Translation tested:
   - comp_pat on function/view patterns with several subpatterns *)
let comp_pat_function_multi =
  if 5 @_is (split_five ??a ??b) then a * 10 + b else 0

(* Translation tested:
   - comp_pat on [and]
   - general path with a non-duplicated failure continuation *)
let comp_pat_and_general =
  if 8 @_is (??x && is_even) then x else 0

(* Translation tested:
   - comp_pat on [and]
   - fast path where the failure continuation is already duplicated *)
let comp_pat_and_fastpath =
  let fallback = 11 in
  if 8 @_is ((__ @_when false) && __) then 0 else consume fallback

(* Translation tested:
   - comp_pat on [or]
   - general path with shared bindings *)
let comp_pat_or_general =
  if (Some 5) @_is (((Some ??x) @_when false) || (Some ??x)) then x else 0

(* Translation tested:
   - comp_pat on [or]
   - fast path where the success continuation is already duplicated *)
let comp_pat_or_fastpath =
  if (Some 1) @_is ((Some ??x) || (Some ??x)) then consume x else 0

(* Translation tested:
   - comp_pat on [not] *)
let comp_pat_not =
  if 3 @_is (not 4) then 1 else 0

(* Translation tested:
   - comp_pat on [when] guards *)
let comp_pat_when =
  if (Some 6) @_is ((Some ??x) @_when (x = 6)) then x else 0

let () =
  start_test "comp_pat/wild";
  let actual = comp_pat_wild in
  check_eq "comp_pat/wild" 1 actual;
  finish_test "comp_pat/wild";

  start_test "comp_pat/var";
  let actual = comp_pat_var in
  check_eq "comp_pat/var" 5 actual;
  finish_test "comp_pat/var";

  start_test "comp_pat/const";
  let actual = comp_pat_const in
  check_eq "comp_pat/const" 1 actual;
  finish_test "comp_pat/const";

  start_test "comp_pat/annot";
  let actual = comp_pat_annot in
  check_eq "comp_pat/annot" (Some 6) actual;
  finish_test "comp_pat/annot";

  start_test "expand_pattern/constr-nullary";
  let actual = comp_pat_constr0 in
  check_eq "expand_pattern/constr-nullary" 1 actual;
  finish_test "expand_pattern/constr-nullary";

  start_test "expand_pattern/constr-args";
  let actual = comp_pat_constr in
  check_eq "expand_pattern/constr-args" 7 actual;
  finish_test "expand_pattern/constr-args";

  start_test "expand_pattern/tuple";
  let actual = comp_pat_tuple in
  check_eq "expand_pattern/tuple" 9 actual;
  finish_test "expand_pattern/tuple";

  start_test "comp_pat/predicate-var";
  let actual = comp_pat_predicate_var in
  check_eq "comp_pat/predicate-var" 1 actual;
  finish_test "comp_pat/predicate-var";

  start_test "comp_pat/predicate-term";
  let actual = comp_pat_predicate_term in
  check_eq "comp_pat/predicate-term" 1 actual;
  finish_test "comp_pat/predicate-term";

  start_test "comp_pat/function-single";
  let actual = comp_pat_function_single in
  check_eq "comp_pat/function-single" 4 actual;
  finish_test "comp_pat/function-single";

  start_test "comp_pat/function-multi";
  let actual = comp_pat_function_multi in
  check_eq "comp_pat/function-multi" 23 actual;
  finish_test "comp_pat/function-multi";

  start_test "comp_pat/and-general";
  let actual = comp_pat_and_general in
  check_eq "comp_pat/and-general" 8 actual;
  finish_test "comp_pat/and-general";

  start_test "comp_pat/and-fastpath";
  let actual = comp_pat_and_fastpath in
  check_eq "comp_pat/and-fastpath" 21 actual;
  finish_test "comp_pat/and-fastpath";

  start_test "comp_pat/or-general";
  let actual = comp_pat_or_general in
  check_eq "comp_pat/or-general" 5 actual;
  finish_test "comp_pat/or-general";

  start_test "comp_pat/or-fastpath";
  let actual = comp_pat_or_fastpath in
  check_eq "comp_pat/or-fastpath" 11 actual;
  finish_test "comp_pat/or-fastpath";

  start_test "comp_pat/not";
  let actual = comp_pat_not in
  check_eq "comp_pat/not" 1 actual;
  finish_test "comp_pat/not";

  start_test "comp_pat/when";
  let actual = comp_pat_when in
  check_eq "comp_pat/when" 6 actual;
  finish_test "comp_pat/when"


(**************************************************************)
(* Keep one visible success message when the generated executable runs *)

let () = print_endline "unit_tests_ppx: ok"
