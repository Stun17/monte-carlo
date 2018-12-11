ORIG = convert.ml decisions.ml shuffle.ml decisions.mli shuffle.mli
DERIV = arbitrage.ml

arbit: prep $(DERIV)
	ocamlc -c arbitrage.mli
	ocamlfind ocamlc -open Batteries -open Shuffle -open Decisions -open Convert \
	-package batteries -c arbitrage.ml

clear:
	rm *.cm[io] a.out

prep: $(ORIG) 
	ocamlc -c shuffle.mli ; ocamlc -c shuffle.ml
	ocamlc -c decisions.mli ; ocamlc -c decisions.ml
	ocamlc -c convert.ml

main: arbit
	ocamlfind ocamlc convert.cmo shuffle.cmo decisions.cmo arbitrage.cmo \
	-open Batteries -open Shuffle -open Decisions -open Convert -open Arbitrage \
	-package batteries -linkpkg main.ml

 
