DUNE ?= dune
OPAM ?= opam
.DEFAULT_GOAL := core

all:
	$(OPAM) install --working-dir --deps-only ./mldoc.opam ./mldoc-cli.opam ./mldoc-js.opam ./mldoc-bench.opam
	$(DUNE) build -p mldoc,mldoc-cli,mldoc-js,mldoc-bench @install @JS @main @bench

core:
	$(OPAM) install --working-dir --deps-only ./mldoc.opam
	$(DUNE) build -p mldoc @install

cli:
	$(OPAM) install --working-dir --deps-only ./mldoc.opam ./mldoc-cli.opam
	$(DUNE) build -p mldoc,mldoc-cli @install @main

js:
	$(OPAM) install --working-dir --deps-only ./mldoc.opam ./mldoc-js.opam
	$(DUNE) build -p mldoc,mldoc-js @install @JS

check: test

test:
	$(OPAM) install --working-dir --deps-only -t ./mldoc.opam
	$(DUNE) runtest -p mldoc

clean:
	dune clean
run: cli
	$(DUNE) exec ./bin/main.exe

bench:
	$(OPAM) install --working-dir --deps-only ./mldoc.opam ./mldoc-bench.opam
	$(DUNE) build -p mldoc,mldoc-bench @install @bench

fmt:
	dune build @fmt --auto-promote

.PHONY: core cli js test all clean check run bench fmt
