
.PHONY: all typer

all: typer tests
	
typer:
	$(MAKE) -C src typer

debug:
	make -C src debug

chk:
	make -C test chk

tests:
	make -C test run

clean:
	rm -f typer.exe src/typer.exe
	$(MAKE) -C test clean

