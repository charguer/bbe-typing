
(** Global flags to change the behavior of the program.
  They are only modified in Typer. **)

let debug = ref false

let quiet = ref false

let continue_on_error = ref false

let force_complete_resolution = ref false

let measure_time = ref false

(* For debugging purposes, we can enforce systematic check for cyclic types;
   because it's too costly, we tests for cycles only at the end of the phase
   of gathering ml constraints, and when testing for candidate instances. *)
let check_cycles_at_every_unification = ref false

(* To fasten the typechecking, we can skip the check for cyclic types when
   filtering the list of candidates. This means for example that if there
   are two instances [f: a -> a -> bool] and [f: a -> (a -> a) -> bool], and we
   have at hand a call of the form [f x x] where [x] has an unconstrained type,
   then we won't be able to rule out the second instance and pick the first one.
   Currently, this flag is only meant for experimenting with execution time.
   (Appears to only gain 5.57s down to 5.43s in one experiment.) *)
let disable_check_cycle_on_resolution_attempts = ref false

(* If disabled, do not write any output. *)
let output = ref true

let print_parsed = ref false

(* Replace overloaded variables by the corresponding instance. *)
let instantiate = ref true

(* Remove terms marked as failing in the output. *)
let remove_failing = ref true

(* Inlining simple functions, doing basic computations, removing most type annotations,
   and calling Ocamlformat. *)
let readable = ref true

(* Print symbols as they are parsed (showing the encoded symbols for records and constants). *)
let print_raw_symbols = ref false

open Ast_print
(* Options on how to print terms. *)
let style_types = ref TypesVarsAndBinders
let style_resolution_full = ref ResolutionInstanceOrSymbol
let style_resolution_base = ref ResolutionInstanceOrSymbol
let style_resolution_args = ref ResolutionInstanceOrSymbol
let style_debug = ref DebugNone


(** When annotating a let-binding with a [let[@type_error "msg"] … = …], do we
  expect the string to be the exact error message or we are fine if there is any
  error. *)
let exact_error_messages = ref true

(* At each loop iteration, how many triggers are examined. *)
let number_of_trigger_passes = ref 10

(* Whether the triggered varid should be cleared after a loop iteration. *)
let clear_triggered_after_loop = ref false

(* Maximum number of trigger associated to a varid. *)
let max_cardinal_trigger = ref 16

(* Maximum dependency depth of varids. *)
let max_varid_depth = ref 20

let print_counters = ref false

(* Hack for gathering more specific statistics *)
let counters_only_for_resolutions_in_last_topdef = ref false

