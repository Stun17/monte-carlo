ORG = shuffle.mli shuffle.ml 

shuffle.cmo: $(ORG)
	ocamlfind ocamlc -package batteries -c $^

a.out: $(ORG) shuffle.cmo main.ml
	ocamlfind ocamlc \
	shuffle.cmo \
	-open Batteries -open Shuffle \
	-package batteries -linkpkg main.ml

clean:
	@rm *.cm[io] 
	@test -z "a.out" || rm -f a.out 

build:
	ocamlbuild -use-ocamlfind -no-hygiene -pkg batteries *.cmo main.native
