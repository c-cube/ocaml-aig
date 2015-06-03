
OCAMLBUILD=ocamlbuild -use-ocamlfind

TARGETS=src/aig.cmxa src/aig.cma src/aig.cmxs src/aig.cmi

all:
	$(OCAMLBUILD) $(TARGETS)

clean:
	$(OCAMLBUILD) -clean

install: all
	ocamlfind install aig META $(addprefix _build/,$(TARGETS))

remove:
	ocamlfind remove aig

.PHONY: all clean install remove

