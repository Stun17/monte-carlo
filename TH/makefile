ORG = decisions.mli decisions.ml shuffle.mli shuffle.ml 
FST = decisions.cmo 
SND = arbitrage.mli arbitrage.ml

decisions.cmo shuffle.cmo: $(ORG)
	ocamlfind ocamlc -package batteries -c $^

arbitrage.cmo: $(FST) $(SND) 
	ocamlfind ocamlc -c -open Batteries -open Decisions -package batteries $(SND)

a.out: $(ORG) arbitrage.cmo main.ml
	ocamlfind ocamlc \
	shuffle.cmo decisions.cmo arbitrage.cmo \
	-open Batteries -open Shuffle -open Decisions -open Arbitrage \
	-package batteries -linkpkg main.ml

clean:
	@rm *.cm[io] 
	@test -z "a.out" || rm -f a.out 

build:
	ocamlbuild -use-ocamlfind -no-hygiene -pkg batteries *.cmo main.native

stat1: acesstat.ml
	ocamlfind ocamlc shuffle.cmo -open Batteries -open Shuffle \
         -package batteries -linkpkg $^ -o $@

stat2: preflop.ml
	ocamlfind ocamlc shuffle.cmo -open Batteries -open Shuffle \
         -package batteries -linkpkg $^ -o $@
