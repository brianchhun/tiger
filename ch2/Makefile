driver:		parser.cmo lexer.cmo driver.cmo
	ocamlc -o driver parser.cmo lexer.cmo driver.cmo

parser.cmo:	parser.ml parser.mli parser.cmi
	ocamlc -c parser.ml

lexer.cmo:	lexer.ml
	ocamlc -c lexer.ml

driver.cmo:	driver.ml
	ocamlc -c driver.ml

lexer.ml:	lexer.mll
	ocamllex lexer.mll

parser.ml parser.mli:	parser.mly
	ocamlyacc parser.mly

parser.cmi:	parser.mli
	ocamlc -c parser.mli

clean:
	rm lexer.ml
	rm parser.ml parser.mli
	rm *.cmo *.cmi
