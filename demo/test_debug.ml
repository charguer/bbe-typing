(* let simple_raise_exit : bool = raise (Exn_Exit ("L", 3)) *)

(* let simple_if_next_1 = if[@label "L"] (2 @_is ??x) then (__next "L"); x else 3 *)
let simple_if_next_2 = if[@label "L"] (2 @_is ??x) then __next "L" else 3

(* Now test : trying, catching, translation into match etc *)