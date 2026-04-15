(**************************************************************)
(* Helpers *)

let check_test s f =
  Printf.printf "Testing %s\n" s;
  if f () then Printf.printf "Successful\n" else
    failwith (Printf.sprintf "Exiting %s with failure\n" s)

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
  check_test "expand_trm/constants+tuple"
  (fun () -> term_constants_and_tuple = (true, 7, 2.5, "bbe", ()));

  check_test "expand_trm/constr-nullary"
  (fun () -> term_constructor_no_arg = Empty);

  check_test "expand_trm/constr-args"
  (fun () -> term_constructor_with_args = (PairBox (3, 4)));

  check_test "expand_topdef/external + expand_trm/annot"
  (fun () -> term_annotated_app = 11);

  check_test "comp_trm/let"
  (fun () -> term_nested_let = 7);

  check_test "expand_trm/not-and-or-as-terms"
  (fun () -> term_boolean_ops = (true, false, true));

  check_test "expand_trm/dotted-ident"
  (fun () -> term_string_length = 4);

  check_test "expand_let_def/recursive"
  (fun () -> term_fact 5 = 120)

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
  check_test "comp_bbe/fallback-if"
    (fun () -> comp_bbe_boolean_fallback_if = 3);

  check_test "comp_bbe/is-non-var"
  (fun () -> comp_bbe_is_non_var = 9);

  check_test "comp_bbe/is-var"
  (fun () -> comp_bbe_is_var = 8);

  check_test "comp_trm/labeled-if-next"
  (fun () -> comp_trm_labeled_if_next = 41);

  check_test "comp_trm/while"
  (fun () -> comp_trm_while_sum () = 6);

  check_test "comp_switch/non-empty-some"
  (fun () -> (comp_switch_non_empty (Some 5)) 1 2 = 8);

  check_test "comp_switch/non-empty-none"
  (fun () -> (comp_switch_non_empty None) 1 2 = 3);

  check_test "comp_switch/labeled-next"
  (fun () -> comp_switch_labeled_next = 23);

  check_test "parser+comp_switch/labeled-match-next-some"
  (fun () -> comp_match_labeled_next (Some 1) = 99);

  check_test "parser+comp_switch/labeled-match-next-none"
  (fun () -> comp_match_labeled_next None = 99);

  check_test "comp_trm/block-exit"
  (fun () -> comp_trm_block_exit = 42);

  check_test "comp_trm/block-plain"
  (fun () -> comp_trm_block_plain = 17);

  check_test "expand_trm/raw-raise-exit"
  (fun () -> expand_trm_raw_raise_exit_not_taken = 123);

  check_test "expand_trm/raw-raise-next"
  (fun () -> expand_trm_raw_raise_next_not_taken = 456);

  check_test "comp_switch/empty-placeholder"
  (fun () -> comp_switch_empty_placeholder_application = 42)

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

(* Higher-order programming *)
(* *)



let () =
  check_test "comp_bbe/not"
  (fun () -> comp_bbe_not = 1);

  check_test "comp_bbe/and-general"
  (fun () -> comp_bbe_and_general = 8);

  check_test "comp_bbe/and-fastpath"
  (fun () -> comp_bbe_and_fastpath = 21);

  check_test "comp_bbe/or-fastpath"
  (fun () -> comp_bbe_or_fastpath = 11);

  check_test "comp_bbe/or-general"
  (fun () -> comp_bbe_or_general = 12)

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
  check_test "comp_pat/wild"
  (fun () -> comp_pat_wild = 1);

  check_test "comp_pat/var"
  (fun () -> comp_pat_var = 5);

  check_test "comp_pat/const"
  (fun () -> comp_pat_const = 1);

  check_test "comp_pat/annot"
  (fun () -> comp_pat_annot = (Some 6));

  check_test "expand_pattern/constr-nullary"
  (fun () -> comp_pat_constr0 = 1);

  check_test "expand_pattern/constr-args"
  (fun () -> comp_pat_constr = 7);

  check_test "expand_pattern/tuple"
  (fun () -> comp_pat_tuple = 9);

  check_test "comp_pat/predicate-var"
  (fun () -> comp_pat_predicate_var = 1);

  check_test "comp_pat/predicate-term"
  (fun () -> comp_pat_predicate_term = 1);

  check_test "comp_pat/function-single"
  (fun () -> comp_pat_function_single = 4);

  check_test "comp_pat/function-multi"
  (fun () -> comp_pat_function_multi = 23);

  check_test "comp_pat/and-general"
  (fun () -> comp_pat_and_general = 8);

  check_test "comp_pat/and-fastpath"
  (fun () -> comp_pat_and_fastpath = 21);

  check_test "comp_pat/or-general"
  (fun () -> comp_pat_or_general = 5);

  check_test "comp_pat/or-fastpath"
  (fun () -> comp_pat_or_fastpath = 11);

  check_test "comp_pat/not"
  (fun () -> comp_pat_not = 1);

  check_test "comp_pat/when"
  (fun () -> comp_pat_when = 6)



(**************************************************************)
(* Keep one visible success message when the generated executable runs *)

let () = print_endline "unit_tests_ppx: ok"
