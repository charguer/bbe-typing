

(* -------------------------------------------------------------- *)
(* Demo tooling *)

(* PATH 1, standalone transpiler

1. this code, in a hacky ocaml-compatible syntax is parsed using ocaml-parser
2. typecheck in ML with BBE bindings (our typechecker)
3. compile BBE constructs away, into an OCaml Parsetree
4. output standard OCaml code in text file

PATH 2, packaged in a PPX

1. this code parsed, as in path 1
2. variable bindings are checked, but not full typechecking
3. compile BBE constructs away, as in path 1
4. output OCaml parse-tree
*)
