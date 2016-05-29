.PHONY: clean testcases

main.native:
	ocamlbuild -Is src main.native

test:
	ocamlbuild -package oUnit -Is src lexer_test.native

testcases: main.native
	testcases/runtests.sh main.native

clean:
	ocamlbuild -clean
