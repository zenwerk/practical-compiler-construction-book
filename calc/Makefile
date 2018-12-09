calc: lexer1.mll parser1.mly calc1.ml interp.ml
	ocamllex lexer1.mll
	ocamlyacc parser1.mly
	ocamlc -c -bin-annot interp.ml
	ocamlc -c -bin-annot parser1.mli
	ocamlc -c -bin-annot lexer1.ml
	ocamlc -c -bin-annot parser1.ml
	ocamlc -c -bin-annot calc1.ml
	ocamlc -o -bin-annot calc interp.cmo lexer1.cmo parser1.cmo calc1.cmo

clean:
	\rm -rf lexer1.ml parser1.ml parser1.mli *.cmo *.cmi *.cmt *cmti calc
