
DEBUGPROG:=../test/debug

.PHONY: all install uninstall clean

all:
	@dune build @all

typer: 
	@dune build typer.exe
	@cd ..; ln -fs _build/default/src/typer.exe typer.exe
	@echo "Updated typed.exe"

chk: typer
	$(MAKE) -C ../test chk

paper: typer
	$(MAKE) -C ../test paper

tests: typer
	$(MAKE) -C ../test run

clean:
	@ dune clean

clean_all:	
	@rm -Rf *.native _build *.cmo *.cmx *.o *.cmi
	$(MAKE) -C ../test clean

debug: typer
	@../typer.exe $(DEBUGPROG).ml && \
	echo "-----------------------------------"  && \
	sed -i '1,/let __print : _ = 1/d' $(DEBUGPROG)_typed.ml  && \
	cat $(DEBUGPROG)_typed.ml 


############################
# DEPRECATED

# SOURCES := $(wildcard *.ml *.mli)

#     (modules_without_implementation annot asttypes outcometree parsetree)

build_using_ocamlopt:
	@ocamlopt -I +compiler-libs -I `ocamlfind query pprint` ocamlcommon.cmxa pprint.cmxa ast.ml ocaml_to_ast.ml ast_print.ml typer.ml -o typer.exe

build_using_ocamlbuild:
	ocamlbuild -I +compiler-libs-package -package pprint typer.native
	cp typer.native typer.exe

#	ln -s ../_build/default/src/typer.exe ../typer.exe

#    @ocamlformat --inplace --enable-outside-detected-project ../test/test_typed.ml \
# ocamlformat --enable-outside-detected-project 

