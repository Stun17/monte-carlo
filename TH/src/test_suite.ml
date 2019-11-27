open Bat ;;
open Shuffle ;;
open Dealing ;;
open Treatment ;;
open Decisions ;;
open Evaluations ;;
open OUnit2 ;;

let rank = fun n (r, s) -> s == n ;;
let not_unique =
  fun xs ->
    List.sort compare xs |>
    fun ys -> List.map2 (fun y1 y2 -> y1 == y2 ) ((-1) :: ys) (ys @ [13]) |>
    List.exists (fun f -> f == true)
;;

let cs = shuffle () ;;
Treatment.init_hash () ;;

(* test of shuffling module  *)
let f cs n =
  assert_equal false ( List.filter (rank n) cs |> List.split |> fst |> not_unique )
;;

let t1_shuffle test_ctxt = f cs 0 ;;
let t2_shuffle test_ctxt = f cs 1 ;;
let t3_shuffle test_ctxt = f cs 2 ;;
let t4_shuffle test_ctxt = f cs 3 ;;

(*  test of dealing module  *)
let g ps n =
  assert_equal n (dealing n ps |>  List.length)
;;

let t1_dealing test_ctxt = g cs  2 ;;
let t2_dealing test_ctxt = g cs  4 ;;
let t3_dealing test_ctxt = g cs  6 ;;
let t4_dealing test_ctxt = g cs  8 ;;
let t5_dealing test_ctxt = g cs 10 ;;

(* test of decisions module *)
let t1_decisions test_ctxt =
  assert_bool "Flush"  (isFlush    [(1,0); (3,0); (3,1); (5,0); (8,2); (10,0); (12,0)])
;;
let t2_decisions test_ctxt =
  assert_bool "Str8"   (isStraight [(1,0); (3,0); (3,1); (5,0); (2,2); (4,3); (12,0)])
;;
let t3_decisions test_ctxt =
  assert_bool "Caree"  (isCaree    [(1,0); (3,0); (1,1); (5,0); (1,2); (10,0); (1,3)])
;;
let t4_decisions test_ctxt =
  assert_bool "Full"   (isFull     [(1,0); (5,0); (3,1); (5,2); (2,2); (5,3); (1,3)])
;;
let t5_decisions test_ctxt =
  assert_bool "Set "   (isSet      [(1,3); (3,0); (1,1); (5,2); (3,2); (10,0); (3,3)])
;;
let t6_decisions test_ctxt =
  assert_bool "Dupal"  (isDupal    [(1,0); (5,0); (3,1); (5,2); (1,2); (7,3); (9,3)])
;;
let t7_decisions test_ctxt =
  assert_bool "Pair"   (isPair     [(8,0); (5,0); (3,1); (5,2); (1,2); (7,3); (9,3)])
;;
let t8_decisions test_ctxt =
  assert_bool "Color"  (isColor    [(3,1); (5,2); (1,2); (7,3); (9,2)])
;;
let t9_decisions test_ctxt =
  assert_bool "Wet"    (isWet      [(3,1); (5,2); (1,2); (5,3); (9,3)])
;;

(* test of evaluation module *)
let t1_evaluations test_ctxt =
  assert_equal 3  (priceFlushStr8  [(1,0); (3,0); (3,1); (2,0); (8,2); (12,0); (0,0)])
;;
let t2_evaluations test_ctxt =
  assert_equal 10 (priceFlushStr8  [(10,0); (8,0); (6,0); (9,0); (8,2); (12,0); (7,0)])
;;
let t3_evaluations test_ctxt =
  assert_equal 124 (priceCaree  [(7,0); (8,0); (7,1); (9,0); (7,2); (12,2); (7,3)])
;;
let t4_evaluations test_ctxt =
  assert_equal 66 (priceCaree  [(3,0); (8,0); (3,1); (9,0); (3,2); (10,2); (3,3)])
;;
let t5_evaluations test_ctxt =
  assert_equal 63 (priceFull  [(3,0); (7,0); (3,1); (9,0); (3,2); (10,2); (7,3)])
;;
let t6_evaluations test_ctxt =
  assert_equal 54 (priceFull  [(2,0); (12,0); (2,1); (9,0); (2,2); (12,2); (7,3)])  
;;
let t7_evaluations test_ctxt =
  assert_equal 100 100
;;



let t0 = "suite" >::: [ "testShuffleSpades uniq" >:: t1_shuffle
                      ; "testShuffleClubs  uniq" >:: t2_shuffle
                      ; "testShuffleDiams  uniq" >:: t3_shuffle
                      ; "testShuffleHearts uniq" >:: t4_shuffle
                      ; "testDeal             2" >:: t1_dealing
                      ; "testDeal             4" >:: t2_dealing
                      ; "testDeal             6" >:: t3_dealing
                      ; "testDeal             8" >:: t4_dealing
                      ; "testDeal            10" >:: t5_dealing
                      ; "testDecisions    Flush" >:: t1_decisions
                      ; "testDecisions  Stright" >:: t2_decisions
                      ; "testDecisions    Caree" >:: t3_decisions
                      ; "testDecisions     Full" >:: t4_decisions
                      ; "testDecisions      Set" >:: t5_decisions
                      ; "testDecisions    Dupal" >:: t6_decisions
                      ; "testDecisions     Pair" >:: t7_decisions
                      ; "testDecisions    Color" >:: t8_decisions
                      ; "testDecisions      Wet" >:: t9_decisions
                      ; "testEvaluations FluStr" >:: t1_evaluations
                      ; "testEvaluations FluStr" >:: t2_evaluations
                      ; "testEvaluations  Caree" >:: t3_evaluations
                      ; "testEvaluations  Caree" >:: t4_evaluations
                      ; "testEvaluations  Caree" >:: t5_evaluations
                      ; "testEvaluations  Caree" >:: t6_evaluations
                      ]

;;

run_test_tt_main t0 ;;
