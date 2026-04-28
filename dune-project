(lang dune 3.0)

(generate_opam_files true)

(package
 (name bbe-typing)
 (allow_empty)
 (synopsis "OCaml PPX extended syntax for pattern matching")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  ppxlib
  ocamlformat-lib
  pprint
  ocaml-compiler-libs)
)
