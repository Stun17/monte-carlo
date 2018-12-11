ORG  = convert.ml decisions.mli decisions.ml shuffle.mli shuffle.ml 
FST = shuffle.cmo decisions.cmo
DER = arbitrage.mli arbitrage.ml

arbitrage.cmo: $(FST)  $(DER) 
	ocamlfind ocamlc -c \
	-open Batteries -open Shuffle -open Decisions -open Convert \
	-package batteries $(DER)

$(FST): $(ORG) 
	ocamlc -c $^

a.out: arbitrage.cmo main.ml
	ocamlfind ocamlc \
	convert.cmo shuffle.cmo decisions.cmo arbitrage.cmo \
	-open Batteries -open Shuffle -open Decisions -open Arbitrage \
	-package batteries -linkpkg main.ml

clean:
	@rm *.cm[io] 
	@if [ -e a.out ] ; then rm a.out ; fi
