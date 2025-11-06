
Typing of Overloading: a Prototype
================


# Installation

## Using Esy


Install esy:
```bash
	npm install -g esy
```

Or update esy (currently using 0.9.2):
```
  npm install --update esy  -g
```

Then, to install the dependencies, compile the project, and test it:
```bash
	esy
```

To run the produced typer on a single file:
```bash
	esy _build/default/src/typer.exe file.ml
```

For example, to print the raw printed term of the unit tests:
```bash
	esy _build/default/src/typer.exe -print-parsed test/unit_tests.ml
	cat test/unit_tests_parsed.ml
```

Or to print the typed unit tests:
```bash
	esy _build/default/src/typer.exe test/unit_tests.ml -o test/unit_tests_typed.ml
	cat test/unit_tests_typed.ml
```


## Using Opam

Install opam:
```bash
	bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
 ```

Don't forget to include in .bashrc if needed:
```bash
	eval `opam config env`
```

Then
```bash
	opam switch create 4.14
	opam install ocaml-compiler-libs ocamlfind ocamlbuild pprint ocamlformat ppxlib dune js_of_ocaml-lwt lwt_ppx
```

### Compilation using Makefile

```bash
	make
```

This command executes `makes typer`, which produces `typer.exe`,
then executes `makes test`, which runs `make` in the `test` folder.

Use `make clean` to remove the generated files.

Note that, internally, the Makefile from `src` invokes dune for
compiling the binary.


### Compilation from the test folder

The `test` folder includes a script for compiling a given file,
or a default file whose name can be edited in that script.

```bash
	cd test
	./go.sh debug.ml
```

Besides, the command `make chk` runs the unit tests against the
expected output, which is commited. Use `make unit_tests.exp`
to manually approve changes, and update the "expected output".


### Debugging the typechecker

```bash
	cd src
	make debug
	make chk
```

With `make debug`, the file `test/debug.ml` is used for debugging.
The filename can be temporarily customized in `src/Makefile`.

With `make chk`, the file `test/unit_tests.ml` is processed and
checked against the expected output stored in `test/unit_tests_exp.ml`.


### Manual compilation using dune

```bash
	dune build
	cat _build/default/test/unit_tests_typed.ml
```

This builds the typechecker as a binary program using the code from `src`,
then executes the unit tests listed in the file `test/dune`.
All output is produced in the `_build` folder.

Use `dun runtest` to rerun the tests after a change.
Use `dune clean` to remove that folder.

Compilation using dune does not coexist well with compilation using
Makefile: after using the Makefile, use `make clean` before using dune.

## Benchmarking

You need `pbench`:

```
  git clone https://github.com/deepsea-inria/pbench ~/shared
```

and useful in your ~/.bashrc  (adapting the path)
```
  alias prun='$HOME/shared/pbench/prun'
  alias pplot='$HOME/shared/pbench/pplot'
```



# Disclaimer

One currently cannot write comments starting with `(**`,
except for annotating definitions that are expected to fail.
Otherwise, one gets an error of the form
```
	Unsupported, <file>: <position>: attribute
```
TODO: handle them properly.

