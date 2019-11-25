ORG0 = bat.mli bat.ml			              # my own batteries for 'take' 'drop' '--' 
ORG1 = shuffle.mli shuffle.ml		        # module for shuffling the deck
ORG2 = decisions.mli decisions.ml	      # module for combinations detection
ORG3 = evaluations.mli evaluations.ml	  # module for combinations ranking
ORG4 = arbitrage.mli arbitrage.ml	      # module for prizing the winner
ORG5 = treatment.mli treatment.ml       # module with hash store for results
ORG6 = dealing.mli dealing.ml           # module for dealing deck
CMN0 = bat.cmo shuffle.cmo treatment.cmo dealing.cmo \
       decisions.cmo evaluations.cmo arbitrage.cmo main.ml 

a.out: $(CMN0) 
	ocamlc -open Bat -open Shuffle -open Decisions -open Evaluations -open Arbitrage \
        -open Treatment $^ 


bat.cmo : $(ORG0) 
	ocamlc -c $^

shuffle.cmo: $(ORG1) bat.cmo
	ocamlc -c -open Bat $^

decisions.cmo : $(ORG2) bat.cmo
	ocamlc -c -open Bat $^

evaluations.cmo : $(ORG3) bat.cmo
	ocamlc -c -open Bat $^

treatment.cmo : $(ORG5) 
	ocamlc -c $^

dealing.cmo: $(ORG6) treatment.cmo
	ocamlc -c -open Shuffle -open Treatment $^

arbitrage.cmo: $(ORG4) bat.cmo decisions.cmo treatment.cmo 
	ocamlc -c -open Bat -open Decisions -open Evaluations -open Treatment $(ORG4)

clean:
	@rm *.cm[io] 
	@test -z "a.out" || rm -f a.out 
