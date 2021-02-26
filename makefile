DUNE ?= dune

all:
	opam install --deps-only .
	$(DUNE) build --profile=release @install @JS @main @bench

check: tests

test:
	opam install --deps-only -t .
	$(DUNE) runtest

clean:
	rm -rf _build

run:
	$(DUNE) exec ./bin/main.exe

bench:
	$(DUNE) exec ./bench/bench.exe

.PHONY: pin test all clean check bench
