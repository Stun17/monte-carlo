ORG0 = bat.mli bat.ml 
ORG1 = shuffle.mli shuffle.ml 
ORG2 = decisions.mli decisions.ml  
ORG3 = arbitrage.mli arbitrage.ml
ORG4 = bat.cmo shuffle.cmo decisions.cmo arbitrage.cmo main.ml 

bat.cmo : $(ORG0) 
	ocamlc -c $^

shuffle.cmo: $(ORG1) bat.cmo
	ocamlc -open Bat -c $^

decisions.cmo : $(ORG2) bat.cmo
	ocamlc -open Bat -c $^

arbitrage.cmo: $(ORG3) bat.cmo decisions.cmo 
	ocamlc -c -open Bat -open Decisions $(ORG3)

evaluations.cmo : bat.cmo evaluations.mli evaluations.ml
	ocamlc -c -open Bat $^

a.out: $(ORG4) 
	ocamlc -open Bat -open Shuffle -open Decisions -open Arbitrage $^ 

clean:
	@rm *.cm[io] 
	@test -z "a.out" || rm -f a.out 
	@test -z "stat1" || rm -f stat1 
	@test -z "stat2" || rm -f stat2 

build:
	ocamlbuild -use-ocamlfind -no-hygiene *.cmo main.native

stat1: bat.cmo shuffle.cmo statsstreet.ml 
	ocamlc bat.cmo shuffle.cmo -open Bat -open Shuffle statsstreet.ml -o $@

stat2: bat.cmo shuffle.cmo streetstats.ml 
	ocamlc bat.cmo shuffle.cmo -open Bat -open Shuffle streetstats.ml -o $@
