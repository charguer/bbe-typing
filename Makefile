
.PHONY: all typer bbe_rewriter ppx

all: typer bbe_rewriter ppx

typer:
	$(MAKE) -C src typer

bbe_rewriter:
	$(MAKE) -C src bbe_rewriter

# ppx:
# 	dune build ppx/ppx_bbe_runner.exe
# 	rm -f ppx_bbe
# 	printf '%s\n' '#!/usr/bin/env sh' 'exec "$$(dirname "$$0")/_build/default/ppx/ppx_bbe_runner.exe" --as-ppx "$$@"' > ppx_bbe
# 	chmod +x ppx_bbe

debug:
	make -C src debug

chk:
	make -C test chk

# tests:

clean:
	rm -f typer.exe bbe_rewriter.exe ppx_bbe src/typer.exe src/bbe_rewriter.exe
	$(MAKE) -C test clean
