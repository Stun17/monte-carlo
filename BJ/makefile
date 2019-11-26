ORG = shuffle.mli shuffle.ml 

shuffle.cmo: $(ORG)
	ocamlfind ocamlc -c $^

a.out: $(ORG) shuffle.cmo main.ml
	ocamlfind ocamlc shuffle.cmo -open Shuffle main.ml

clean:
	@rm -f *.cm[io] 
	@test -z "a.out" || rm -f a.out 

build: shuffle.cmo
	ocamlbuild -use-ocamlfind -no-hygiene shuffle.cmo main.native
