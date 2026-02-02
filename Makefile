
.PHONY: all typer

all: typer ppx

typer:
	$(MAKE) -C src typer

ppx:
	make -C src ppx

debug:
	make -C src debug

chk:
	make -C test chk

# tests:
# 	make -C test run

clean:
	rm -f typer.exe src/typer.exe
	$(MAKE) -C test clean

