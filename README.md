
ppx_bbe
================

PPX extension for *binding-boolean-expressions*.

Implementation of the language defined in *source paper*

### Overview 

...

### Compilation

Compile the whole project with `make all` in the root directory.

### Usage

There are three ways of using this tool:

1. The executable `bbe_rewriter.exe` takes as input an extended OCaml file, and outputs its translation by the compilation scheme, with the suffix `_translated.ml`.

2. The ppx `ppx_bbe` directly branches into dune, and preprocesses the extended OCaml input before the typecheck. 
In the dune executable stanza, add `(preprocess (pps ppx_bbe))` to preprocess with the ppx (Check `demo/dune` for an example).

3. As a standalone typechecker, with `typer.exe`. It is assumed that the two first options will be used with the flag `Flags.weak_typer` set to `true`. If the flag is instead set to false, `typer.exe` will verify the type of the input extended OCaml file according to the typing rules of *source paper*.





<!--
- 1 sentence overview of the project 
- Small example of the kind of code, 1 small translation example with a non trivial example (that does not translate to a let basically) "if x @_is (Some(__), ??y) then y+1 else 0" translates to ... 
- Installation and usage : 
  -> For installation you have to git clone this project for the moment
  -> For usage, (not yet implemented), but there should be 2 ways:
    => Either an executable that from an extended OCaml gives a rewritten OCaml
    => A ppx add-on that simply branches to your compilation scheme as a preprocess before compilation.
- Syntax
  -> Give a few examples of non trivial usage, and refer to syntax.md & the paper for the full language syntax and the theoretical results
-->
 
