BAT = bat.mli bat.ml 
ORG1 = shuffle.mli shuffle.ml 
ORG2 = decisions.mli decisions.ml  
FST = bat.cmo decisions.cmo 
SND = arbitrage.mli arbitrage.ml

bat.cmo bat.cmi: $(BAT) 
	ocamlc -c bat.mli
	ocamlc -c bat.ml

shuffle.cmo: $(ORG1)
	ocamlc -open Bat -c $^

decisions.cmo : $(ORG2)
	ocamlc -open Bat -c $^

arbitrage.cmo: $(FST) $(SND) 
	ocamlc -c -open Bat -open Decisions $(SND)

a.out: $(ORG1) $(ORG2) bat.cmo arbitrage.cmo main.ml
	ocamlc bat.cmo shuffle.cmo decisions.cmo arbitrage.cmo \
	-open Bat -open Shuffle -open Decisions -open Arbitrage main.ml

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
