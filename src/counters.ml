
(* Counters, for debugging and measures of code complexity. *)

(* optional: use an array instead of independent counters, for better code factorization? *)

(* LATER: split counters between ml_constraints and resolution_passes;
   Current hack: use flag -counters-only-for-resolutions-in-last-topdef
   to reset the counter after the ml-constraint gathering. *)

let counter_history = ref 0
let counter_unify = ref 0
let counter_cycle_visit = ref 0
let counter_repr_visit = ref 0
let counter_varid = ref 0
let counter_resolution_attempt = ref 0
let counter_candidate_instantiable = ref 0
let counter_passes = ref 0

let time_unify = ref 0.
let time_resolution_attempt = ref 0.

let time_ml_constraints = ref 0.
let time_symbol_resolution = ref 0.
let time_check_fully_typed = ref 0.

let reset_stats_except_time_ml_constraints () =
  counter_history := 0;
  counter_unify := 0;
  counter_cycle_visit := 0;
  counter_repr_visit := 0;
  counter_varid := 0;
  counter_resolution_attempt := 0;
  counter_candidate_instantiable := 0;
  counter_passes := 0;
  time_unify := 0.;
  time_resolution_attempt := 0.;
  time_symbol_resolution := 0.;
  time_check_fully_typed := 0.

let compute_and_time (time_acc:float ref) (f:unit->'a) : 'a =
  let time,res = Tools.with_timing f in
  time_acc := !time_acc +. time;
  res

let compute_count_and_time (counter:int ref) (time_acc:float ref) (f:unit->'a) : 'a =
  incr counter;
  let res = compute_and_time time_acc f in
  res

let print_counters () =
  Printf.printf "counter_history %d\n" !counter_history ;
  Printf.printf "counter_unify %d\n" !counter_unify ;
  Printf.printf "counter_cycle_visit %d\n" !counter_cycle_visit ;
  Printf.printf "counter_repr_visit %d\n" !counter_repr_visit ;
  Printf.printf "counter_varid %d\n" !counter_varid ;
  Printf.printf "counter_resolution_attempt %d\n" !counter_resolution_attempt ;
  Printf.printf "counter_candidate_instantiable %d\n" !counter_candidate_instantiable ;
  Printf.printf "counter_passes %d\n" !counter_passes ;
  Printf.printf "time_ml_constraints %f\n" !time_ml_constraints ;
  Printf.printf "time_symbol_resolution %f\n" !time_symbol_resolution ;
  Printf.printf "time_check_fully_typed %f\n" !time_check_fully_typed ;
  Printf.printf "mean_ns_unify %f\n" (1000000000. *. !time_unify /. float_of_int !counter_unify);
  Printf.printf "mean_ns_resolution_attempt %f\n" (1000000000. *. !time_resolution_attempt /. float_of_int !counter_resolution_attempt);
  ()




