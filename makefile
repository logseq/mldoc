DUNE ?= dune

all:
	$(DUNE) build @install @JS @bench @main

check: tests

test:
	$(DUNE) runtest

clean:
	rm -rf _build

run:
	$(DUNE) exec ./bin/main.exe

bench:
	$(DUNE) exec ./bench/bench.exe

.PHONY: pin test all clean check bench
