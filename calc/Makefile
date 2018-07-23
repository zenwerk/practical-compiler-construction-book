calc: lexer1.mll parser1.mly calc1.ml interp.ml
	ocamllex lexer1.mll
	ocamlyacc parser1.mly
	ocamlc -c interp.ml
	ocamlc -c parser1.mli
	ocamlc -c lexer1.ml
	ocamlc -c parser1.ml
	ocamlc -c calc1.ml
	ocamlc -o calc interp.cmo lexer1.cmo parser1.cmo calc1.cmo

clean:
	\rm -rf lexer1.ml parser1.ml parser1.mli *.cmo calc
