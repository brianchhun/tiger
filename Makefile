all:
	ocamlbuild -Is src main.native

test:
	ocamlbuild -package oUnit -Is src lexer_test.native

clean:
	ocamlbuild -clean
