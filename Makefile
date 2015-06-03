
OCAMLBUILD=ocamlbuild -use-ocamlfind

TARGETS=src/aig.cmxa src/aig.cma src/aig.cmxs src/aig.cmi

all:
	$(OCAMLBUILD) $(TARGETS)

clean:
	$(OCAMLBUILD) -clean

.PHONY: all clean

