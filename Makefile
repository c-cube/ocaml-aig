
OCAMLBUILD=ocamlbuild -use-ocamlfind

TARGETS=src/aig.cmxa src/aig.cma src/aig.cmxs src/aig.cmi
TEST_TARGETS=test/test_aig.native

all:
	$(OCAMLBUILD) $(TARGETS)

clean:
	$(OCAMLBUILD) -clean

install: all
	ocamlfind install aig META $(addprefix _build/,$(TARGETS))

build-test: all

test: build-test
	./test_aig.native

test-verbose: build-test
	./test_aig.native -v

remove:
	ocamlfind remove aig

.PHONY: all clean install remove build-test test test-verbose

