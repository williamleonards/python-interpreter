MODULES=opython main state parser evaluate utils arithmetic error builtin
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST_DIR=tests/*
TEST=test.byte
MAIN=main.byte
OPYTHON=opython
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix,str,oUnit,qcheck

default: build
	utop

test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST) && $(OCAMLBUILD) opython.byte && mv opython.byte opython && ./run_tests.sh

pytest:
	$(OCAMLBUILD) opython.byte && mv opython.byte opython && ./run_tests.sh

run:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

all:
	$(OCAMLBUILD) opython.byte && mv opython.byte opython

zip:
	zip opython.zip *.ml* _tags Makefile .bashrc tests/* run_tests.sh
	
docs: docs-public docs-private
	
docs-public: all
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: all
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report search_src.zip bisect*.out
