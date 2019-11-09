ORG0 = bat.mli bat.ml			# my own batteries for 'take' and 'drop' lists functions
ORG1 = shuffle.mli shuffle.ml		# module for shuffling the deck
ORG2 = decisions.mli decisions.ml	# module for combinations detection
ORG3 = evaluations.mli evaluations.ml	# module for combinations ranking
ORG4 = arbitrage.mli arbitrage.ml	# module for prizing the winner

CMN0 = bat.cmo shuffle.cmo treatment.cmo decisions.cmo evaluations.cmo arbitrage.cmo main.ml 

bat.cmo : $(ORG0) 
	ocamlc -c $^

shuffle.cmo: $(ORG1) bat.cmo
	ocamlc -c -open Bat $^

decisions.cmo : $(ORG2) bat.cmo
	ocamlc -c -open Bat $^

evaluations.cmo : $(ORG3) bat.cmo
	ocamlc -c -open Bat $^

treatment.cmo : treatment.mli treatment.ml
	ocamlc -c $^

arbitrage.cmo: $(ORG4) bat.cmo decisions.cmo treatment.cmo 
	ocamlc -c -open Bat -open Decisions -open Evaluations -open Treatment $(ORG4)

a.out: $(CMN0) 
	ocamlc -open Bat -open Shuffle -open Decisions -open Evaluations -open Arbitrage \
        -open Treatment $^ 


# to clean stuff
clean:
	@rm *.cm[io] 
	@test -z "a.out" || rm -f a.out 
	@test -z "stat1" || rm -f stat1 
	@test -z "stat2" || rm -f stat2 


# utilities for different practical cases her

build:
	ocamlbuild -use-ocamlfind -no-hygiene *.cmo main.native

stat1: bat.cmo shuffle.cmo statsstreet.ml 
	ocamlc bat.cmo shuffle.cmo -open Bat -open Shuffle statsstreet.ml -o $@

stat2: bat.cmo shuffle.cmo streetstats.ml 
	ocamlc bat.cmo shuffle.cmo -open Bat -open Shuffle streetstats.ml -o $@
