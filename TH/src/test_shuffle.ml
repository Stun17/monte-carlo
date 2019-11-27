open Shuffle ;; 
open OUnit2 ;;  

let rank = fun n (r, s) -> s == n ;;
let not_unique =
  fun xs -> 
    List.sort compare xs |> 
    fun ys -> List.map2 (fun y1 y2 -> y1 == y2 ) ((-1) :: ys) (ys @ [13]) |> 
    List.exists (fun f -> f == true) 
;;

let f cs n = 
  assert_equal false (List.filter (rank n) cs |> List.split |> fst |> not_unique) 
;;

let cs = shuffle () ;;

let t1_shuffle test_ctxt = f cs 0 ;;
let t2_shuffle test_ctxt = f cs 1 ;;
let t3_shuffle test_ctxt = f cs 2 ;;
let t4_shuffle test_ctxt = f cs 3 ;;




let t0 = "suite" >::: [ 
                        "testSpades uniq" >:: t1_shuffle
                      ; "testClubs  uniq" >:: t2_shuffle
                      ; "testDiams  uniq" >:: t3_shuffle
                      ; "testHearts uniq" >:: t4_shuffle
                      ] 
;;

run_test_tt_main t0 ;;
