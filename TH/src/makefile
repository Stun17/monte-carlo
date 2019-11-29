ORG0 = bat.mli bat.ml			# my own batteries for 'take' 'drop' '--' 
ORG1 = shuffle.mli shuffle.ml		# module for shuffling the deck
ORG2 = decisions.mli decisions.ml	# module for combinations detection
ORG3 = evaluations.mli evaluations.ml	# module for combinations ranking
ORG4 = arbitrage.mli arbitrage.ml	# module for prizing the winner
ORG5 = treatment.mli treatment.ml       # module with hash store for results
ORG6 = dealing.mli dealing.ml           # module for dealing deck
CMN  = bat.cmo shuffle.cmo treatment.cmo dealing.cmo \
       decisions.cmo evaluations.cmo arbitrage.cmo main.ml 
CMT  = bat.cmo treatment.cmo shuffle.cmo dealing.cmo \
       decisions.cmo evaluations.cmo arbitrage.cmo 


test.out:  $(CMT) test_suite.ml
	ocamlfind ocamlc -open Treatment -open Bat -open Shuffle -open Dealing \
	-open Decisions -open Evaluations -open Bat -package oUnit -linkpkg \
	-o $@ $^
	mv $@ ../
	@echo "done with test.out"

a.out: $(CMN)
	@echo "start with a.out"
	ocamlc -o $@ -open Bat -open Shuffle -open Decisions \
	-open Evaluations -open Arbitrage -open Treatment $^
	mv $@ ../
	@echo "done with a.out" 

bat.cmo : $(ORG0) 
	ocamlc -c $^

shuffle.cmo: $(ORG1) bat.cmo
	ocamlc -c -open Bat $^

decisions.cmo : $(ORG2) bat.cmo
	ocamlc -c -open Bat $^

evaluations.cmo : $(ORG3) 
	ocamlc -c $^

treatment.cmo : $(ORG5) 
	ocamlc -c $^

dealing.cmo: $(ORG6) treatment.cmo
	ocamlc -c -open Shuffle -open Treatment $^

arbitrage.cmo: $(ORG4) bat.cmo decisions.cmo treatment.cmo 
	ocamlc -c -open Bat -open Decisions -open Evaluations -open Treatment $(ORG4)

clean:
	@rm *.cm[io] 
	@rm oUnit*

