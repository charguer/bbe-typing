
ppx_bbe
================

Prototype implementation of the ideas described in the research paper:
[*Functional Pearl: Binding Boolean Expressions and Extended Pattern Matching*](https://www.chargueraud.org/research/2026/bbe/bbe-and-extended-matching.pdf),
by Arthur Charguéraud and Yanni Lefki, April 2026, 

The prototype implements:

  - a standalone typechecker (independent of the OCaml typechecker)
  - a PPX extension that compiles the extended language features into standard OCaml code.

### Overview 

### Requirements

- `opam`
- OCaml `4.14.x`
- `dune >= 3.0`

### Installation

From the root of the repository:

```bash
opam switch create . 4.14.1
eval $(opam env)
opam install . --deps-only
```

The project dependencies are declared in `dune-project`. In particular, the
project uses `ppxlib`, `ocamlformat-lib`, `pprint`, and `ocaml-compiler-libs`.

### Compilation

The command `make` from the root build all of the executables of the tool.

### Usage

There are three ways of using this tool:

1. As a standalone typechecker, with `typer.exe`.

It is assumed that the two first options will be used with the flag
`Flags.weak_typer` set to `true`. If the flag is instead set to `false`,
`typer.exe` verifies the type of the input extended OCaml file according to the
typing rules of the [source paper](https://www.chargueraud.org/research/2026/bbe/bbe-and-extended-matching.pdf).

Tested command:

```bash
typer.exe test/unit_tests_bbe.ml
```

For simplicity, both `typer.exe` and `bbe_rewriter.exe` are symlinked to the root directory from `_build/default/src`.

2. As a standalone rewriter, with `bbe_rewriter.exe`.

The executable `bbe_rewriter.exe` takes as input an extended OCaml file, and
outputs its translation by the compilation scheme, with the suffix
`_rewritten.ml`.

Tested command:

```bash
bbe_rewriter.exe test/unit_tests_ppx.ml
```

This generates `test/unit_tests_ppx_rewritten.ml`.

3. As a Dune PPX rewriter.

The ppx `ppx_bbe` can be attached directly in a Dune stanza. In the target
stanza, add:

```ocaml
(preprocess (pps ppx_bbe))
```

See `demo/dune` for a minimal example.

Tested command:

```bash
dune exec demo/test_code.exe
```

This currently prints:

```text
1
1
1
```

Remarks: 
- The typechecker is deactivated by default for both `bbe_rewriter.exe` and
  `ppx_bbe`. It can be reactivated with the flag `-strong-typer`.
- The source code contains currently unused functions, originally used for handling overloading.  