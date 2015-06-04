
OCAMLBUILD=ocamlbuild -use-ocamlfind

TARGETS=src/aig.cmxa src/aig.cma src/aig.cmxs src/aig.cmi
TEST_TARGETS=test/test_aig.native

all:
	$(OCAMLBUILD) $(TARGETS)

clean:
	$(OCAMLBUILD) -clean

install: all
	ocamlfind install aig META $(addprefix _build/,$(TARGETS))

test: all
	$(OCAMLBUILD) -I src/ $(TEST_TARGETS)
	./test_aig.native

remove:
	ocamlfind remove aig

.PHONY: all clean install remove test

