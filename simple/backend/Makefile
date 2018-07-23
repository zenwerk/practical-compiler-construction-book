FILE = parser.mly lexer.mll sim.ml \
       ast.ml types.ml table.ml semant.ml emitter.ml 

simc: $(FILE)
	ocamlyacc parser.mly
	ocamllex lexer.mll
	ocamlc -c ast.ml
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c types.ml
	ocamlc -c table.ml
	ocamlc -c semant.ml
	ocamlc -c emitter.ml
	ocamlc -c sim.ml
	ocamlc -o simc unix.cma ast.cmo lexer.cmo parser.cmo \
                  types.cmo table.cmo semant.cmo emitter.cmo sim.cmo

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
	rm  -f *.cmi *.cmo parser.ml lexer.ml parser.mli simc print_ast

