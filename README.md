
ppx_bbe
================

PPX extension for *binding-boolean-expressions*.

Implementation of the language defined in *source paper*

### Overview 

...

### Compilation

Compile the whole project with `make all` in the root directory.

### Usage

For the three tools, the flag ... describes whether we do true typing according 
"scope checking for pattern variables" 

There are three ways of using this tool:

<!-- Ajouter un pointeur vers le fichier / executer telle commande -->

1. As a standalone typechecker, with `typer.exe`. It is assumed that the two first options will be used with the flag `Flags.weak_typer` set to `true`. If the flag is instead set to false, `typer.exe` will verify the type of the input extended OCaml file according to the typing rules of *source paper*.
Example usage: `./typer.exe test/unit_tests_bbe.ml`

2. The executable `bbe_rewriter.exe` takes as input an extended OCaml file, and outputs its translation by the compilation scheme, with the suffix `_rewriten.ml`.
Example usage: `./bbe_rewriter.exe test/unit_tests_ppx.ml`

<!-- Tester bisect_ppx, on a testé toutes les lignes, on a blabla. -->

3. The ppx `ppx_bbe` directly branches into dune, and preprocesses the extended OCaml input before the typecheck. 
In the dune executable stanza, add `(preprocess (pps ppx_bbe))` to preprocess with the ppx (Check `demo/dune` for an example).
Example usage: `dune exec demo/test_code.exe`

Remark: 
- The typechecker is deactivated by default for both `bbe_rewriter.exe` and `ppx_bbe`. It can be reactivated with the compilation flag <!-- Not sure for ppx_bbe? --> `-strong-typer`.

<!-- 
TODO:
1. Chaque outil doit être prêt out of the box, c'est à dire avec les bons flags, et les bons arguments. Ajouter dans les lignes de commande des flags de la forme --xxx qui modifient ce que tu veux
2. Dans le readme, donner l'exemple de ce qu'il faut.
3. Rajouter les dépendances à installer, par exemple arthur avait pas "ocamlformat-lib"
4. Voir comment ajouter -ppx avec ce que je veux. ce serait vachement cool pour usage
5. Changer les unittests avec 1 ou deux examples puis génération sur l'IA pour avoir du higher order programming

-->

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
 
<!-- Check notes :
https://chatgpt.com/share/69de3c01-9b58-8396-9e24-a294df1ab6c4 
-->