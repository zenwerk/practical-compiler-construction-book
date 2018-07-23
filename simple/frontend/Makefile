FILE = parser.mly lexer.mll ast.ml  

print_ast: $(FILE) print_ast
	ocamlyacc parser.mly
	ocamllex lexer.mll
	ocamlc -c ast.ml
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c print_ast.ml
	ocamlc -o print_ast unix.cma ast.cmo lexer.cmo parser.cmo print_ast.cmo

clean:
	rm  -f *.cmi *.cmo parser.ml lexer.ml parser.mli print_ast

