BAT = bat.mli bat.ml 
ORG = decisions.mli decisions.ml shuffle.mli shuffle.ml 
FST = bat.cmo decisions.cmo 
SND = arbitrage.mli arbitrage.ml

bat.cmo bat.cmi: $(BAT) 
	ocamlc -c bat.mli
	ocamlc -c bat.ml

decisions.cmo shuffle.cmo: $(ORG)
	ocamlc -open Bat -c $^

arbitrage.cmo: $(FST) $(SND) 
	ocamlc -c -open Bat -open Decisions $(SND)

a.out: $(ORG) bat.cmo arbitrage.cmo main.ml
	ocamlc bat.cmo shuffle.cmo decisions.cmo arbitrage.cmo \
	-open Bat -open Shuffle -open Decisions -open Arbitrage main.ml

clean:
	@rm *.cm[io] 
	@test -z "a.out" || rm -f a.out 

build:
	ocamlbuild -use-ocamlfind -no-hygiene *.cmo main.native

stat1: bat.cmo shuffle.cmo statsstreet.ml 
	ocamlfind ocamlc bat.cmo shuffle.cmo -open Bat -open Shuffle statsstreet.ml -o $@

stat2: bat.cmo shuffle.cmo streetstats.ml 
	ocamlfind ocamlc bat.cmo shuffle.cmo -open Bat -open Shuffle streetstats.ml -o $@
