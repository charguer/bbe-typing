(* Should execute correctly after translation *)
let test_print_1 = print_string "Correct printing\n"
let test_print_2 = Printf.printf "Correct printing\n"
let test_print_3 = Printf.printf "Correct %s of %d\n" "printing" 1

(* Should be rejected by OCaml's typing *)
let test_print_false_1 = print_int "Correct printing\n"
let test_print_false_2 = Printf.printf "Correct %s o %d\n"
