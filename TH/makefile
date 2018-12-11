doit:
	ocamlfind ocamlc -open Batteries -open Shuffle -open Decisions -package batteries -c arbitrage.ml

clear:
	rm *.cm[io]

prep: 
	ocamlc -c shuffle.mli ; ocamlc -c shuffle.ml
	ocamlc -c decisions.mli ; ocamlc -c decisions.ml

teit:
	ocamlfind ocamlc convert.cmo shuffle.cmo decisions.cmo -open Batteries -open Shuffle -open Decisions -open Convert -package batteries -linkpkg arbitrage.ml
