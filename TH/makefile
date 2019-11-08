BAT = bat.mli bat.ml 
ORG = decisions.mli decisions.ml shuffle.mli shuffle.ml 
FST = bat.cmo decisions.cmo 
SND = arbitrage.mli arbitrage.ml

bat.cmo bat.cmi: $(BAT) 
	ocamlfind ocamlc -c bat.mli
	ocamlfind ocamlc -c bat.ml

decisions.cmo shuffle.cmo: $(ORG)
	ocamlfind ocamlc -open Bat -c $^

arbitrage.cmo: $(FST) $(SND) 
	ocamlfind ocamlc -c -open Bat -open Decisions $(SND)

a.out: $(ORG) bat.cmo arbitrage.cmo main.ml
	ocamlfind ocamlc bat.cmo shuffle.cmo decisions.cmo arbitrage.cmo \
	-open Bat -open Shuffle -open Decisions -open Arbitrage main.ml

clean:
	@rm *.cm[io] 
	@test -z "a.out" || rm -f a.out 

build:
	ocamlbuild -use-ocamlfind -no-hygiene *.cmo main.native

stat1: acesstat.ml
	ocamlfind ocamlc shuffle.cmo -open Shuffle $^ -o $@

stat2: preflop.ml
	ocamlfind ocamlc shuffle.cmo -open Shuffle $^ -o $@
