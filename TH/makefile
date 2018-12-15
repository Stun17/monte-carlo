ORG = decisions.mli decisions.ml shuffle.mli shuffle.ml 
FST = decisions.cmo rangir.cmo
SND = rangir.cmo
BER = rangir.mli rangir.ml
DER = arbitrage.mli arbitrage.ml

rangir.cmo: $(BER)
	ocamlfind ocamlc -package batteries -c $^

arbitrage.cmo: $(FST) $(SND) $(DER)
	ocamlfind ocamlc -c \
	-open Batteries -open Decisions -open Rangir \
	-package batteries $(DER)

%.cmo: $(ORG)
	ocamlfind ocamlc -package batteries -c $^

a.out: $(ORG) rangir.cmo arbitrage.cmo main.ml
	ocamlfind ocamlc \
	shuffle.cmo decisions.cmo rangir.cmo arbitrage.cmo \
	-open Batteries -open Shuffle -open Decisions -open Rangir -open Arbitrage \
	-package batteries -linkpkg main.ml

clean:
	@rm *.cm[io] 
	@if [ -e a.out ] ; then rm a.out ; fi
