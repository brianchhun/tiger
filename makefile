OCB_FLAGS = -lib str -I src -use-ocamlfind -package extlib
OCB = ocamlbuild $(OCB_FLAGS)

native:
	$(OCB) main.native

clean:
	$(OCB) -clean

test:	native
	$(OCB) -package oUnit lexer_test.native \
						  graph_test.native \
						  make_graph_test.native \
						liveness_test.native

testcases: native
	testcases/runtests.sh main.native

.PHONY: native clean test testcases
