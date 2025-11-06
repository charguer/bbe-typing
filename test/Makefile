
# 'make all'    is like 'make chk'
# 'make typ'    typechecks all the unit tests
# 'make chk'    verifies that the typed programs match the expected output
# 'make typer'  to compile the typer.exe program in ../src
# 'make clean'  to clean up everything

# 'make test_foo.out'  to view the output for one given test
# 'make test_foo.chk'  to verify that test_foo produces the expected output
# 'make test_foo.meld' to view why test_foo does not produce the expected output
# 'make test_foo.exp'  to regenerate the expected output file
# 'make exp'           to regenerate all expected output files

# 'make V= command'  for details

##############################################################

# Verbosity for commands
V ?= @

# Path to source code of type-checker
SRC := ../src

# Path to the type-checker binary
TYPER := ../typer.exe

# List of all tests
TESTS := \
  unit_tests.ml \
  paper.ml

# Alternative:
#   TESTS := $(wildcard test_*.ml)
#   TESTS := $(filter-out test_broken.ml $(TESTS))

# The command for calling diff
DIFF := diff --ignore-blank-lines --ignore-all-space -u -I '^//'

##############################################################

.PHONY: all typ chk clean

.PRECIOUS: %.chk %_parsed.ml %_typed.ml %_exp.ml

##############################################################

all: run

run: $(TESTS:.ml=_typed.ml)

paper: paper_typed.ml

debug: debug_typed.ml


####################
# Rules to build the checker

typer: $(TYPER)

$(TYPER): 
	$(MAKE) -C $(SRC) typer.exe

####################
# Rules to process a test

%_parsed.ml %_typed.ml: %.ml $(TYPER)
	$(V)$(TYPER) $<
	@echo "Produced $*_typed.ml"

%.out: %_typed.ml
	echo $*_typed.ml 
	$(V)cat $<


####################
# Special rule for test_error.ml

%_parsed.ml %_typed.ml: %.ml $(TYPER)
	OCAMLRUNPARAM=b $(V)$(TYPER) -continue-on-error $<
	@echo "Produced $*_typed.ml"

####################
# Rules to check against the expected output

chk: $(TESTS:.ml=.chk)

%.chk: %_typed.ml %_exp.ml
	$(V) ($(DIFF) -q $^ > /dev/null && touch $@ && echo "Success for $@") \
	|| (echo "=== ERROR: $< does not match the expected result:" && echo "  make $*.meld   or    make $*.diff    or    make diff")

# Rule for opening meld to compare the output and the expected output
%.meld: %_typed.ml %_exp.ml
	meld $^

####################
# Rules to print differences against the expected output

diff: $(TESTS:.ml=.diff)

%.diff: %_typed.ml %_exp.ml
	echo
	$(V) ($(DIFF) $^)


####################
# Rule for producing expected files

# Rule for producing the expected file after deleting the previous one
%.exp: %_typed.ml
	@rm -f $*_exp.ml && (cp $< $*_exp.ml && \
	echo "(Re)Produced $*_exp.ml from $<.")

exp: $(TESTS:.ml=.exp)


####################
# Rule for cleanup

clean_exp:
	$(V)rm -f *_exp.ml

clean_typer:
	$(V)rm -Rf ../_build

clean:
	$(V)rm -f *_parsed.ml *_typed.ml *.chk
	$(V)rm -f */*_parsed.ml */*_typed.ml */*.chk

clean_all: clean clean_typer

