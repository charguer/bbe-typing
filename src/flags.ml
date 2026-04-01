
(** Global flags to change the behavior of the program.
  They are only modified in Typer. **)

let debug = ref true
let verbose = ref true

let quiet = ref false

let continue_on_error = ref true

(* If enabled, the typer will stop unifying types, and only verify variable scopes *)
(* More precisely, it will do a deep lookup of the ast, and raise trivial shadowing errors with pattern variables.
If a variable is bound in some context, then it can not appear as a pattern variable inside some pattern. *)
let weak_typer = ref true

(* If enabled, halts the typing on error and forces OCaml's verbose runtime trace *)
let halt_on_error = ref false

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

let print_types = ref false
let print_parsed = ref false

(* If enabled, do a second pass and type it through the typechecker *)
let recompile = ref false

(* If enabled, compile back to ocaml *)
let expand = ref true

(* Very specific and temporary flag, to remove the first line of expanded version for a presentation *)
let presentation = ref false

(* Replace overloaded variables by the corresponding instance. *)
let instantiate = ref false

(* Remove terms marked as failing in the output. *)
let remove_failing = ref true

(* Inlining simple functions, doing basic computations, removing most type annotations,
   and calling Ocamlformat. *)
let readable = ref false

(* Print symbols as they are parsed (showing the encoded symbols for records and constants). *)
let print_raw_symbols = ref false

open Ast_print
(* Options on how to print terms. *)
let style_types = ref TypesNone
let style_resolution_full = ref ResolutionInstanceOrSymbol
let style_resolution_base = ref ResolutionInstanceOrSymbol
let style_resolution_args = ref ResolutionInstanceOrSymbol
let style_debug = ref DebugNone
let style_binds = ref BindsNone

(* let _ = style_binds := BindsToplevel *)
let _ = style_binds := BindsAll

(** When annotating a let-binding with a [let[@type_error "msg"] … = …], do we
  expect the string to be the exact error message or we are fine if there is any
  error. *)
let exact_error_messages = ref true

(* Maximum dependency depth of varids. *)
let max_varid_depth = ref 20

let print_counters = ref false

(* Hack for gathering more specific statistics *)
let counters_only_for_resolutions_in_last_topdef = ref false


