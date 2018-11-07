DUNE ?= dune

all:
	$(DUNE) build @install @JS @bench

check: tests

test:
	$(DUNE) runtest

clean:
	rm -rf _build

run:
	$(DUNE) exec ./org_parser.exe

benchmark: 
	$(DUNE) exec ./bench/bench.exe
	

.PHONY: test all clean check
