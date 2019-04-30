module Decisions =
struct

  type hand = (int * int) list

  let countSuit cs n =
    List.filter (fun (_,s) -> s = n) cs |> List.length ;;

  let countRank cs k =
    List.filter (fun (r,_) -> r = k) cs |> List.length ;;
                                                                                  
  let isFlush cs =
    List.map (countSuit cs) [0;1;2;3] |> List.exists (fun x -> x >= 5) ;;
       
  let isCaree cs =
    List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |> List.exists (fun x -> x = 4) ;;

  let isFull cs =
    (List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |> List.exists (fun x -> x = 3)) &&
    (List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |> List.exists (fun x -> x = 2))
  ;;
    
  let isStraight cs =
      List.split  cs |> fst |> List.sort compare |> fun zs ->
                    ((List.nth zs 0) - (List.nth zs 4) = 4)
                 || ((List.nth zs 1) - (List.nth zs 5) = 4)
                 || ((List.nth zs 2) - (List.nth zs 6) = 4)
                 || (  (List.nth zs 0) = 12 &&
                         (List.nth zs 3) =  3 &&
                           (List.nth zs 4) =  2 &&
                             (List.nth zs 5) =  1 &&
                               (List.nth zs 6) =  0 
                         )
  ;;

  let isFlushStr8 cs = (isFlush cs) && (isStraight cs) ;;
    
  let isSet cs =
    (List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |> List.exists (fun x -> x = 3)) ;;

  let isDupal     = fun cs -> true ;; 
  let isPair      = fun cs -> true ;; 

  let isHigh      = fun cs -> true ;; 
      
end ;;

(* (\* test suite  *\) *)
  
(* print_newline () ;; *)
  
(*   (\* good one *\) *)
(* let te1 = Decisions.isFluStr8 [(1,1);(2,1);(0,1);(3,1);(4,1);(5,1);(9,1)] *)
(* let te2 = Decisions.isFluStr8 [(5,1);(4,1);(0,1);(2,1);(1,1);(3,1);(7,1)] *)
(* let te3 = Decisions.isFluStr8 [(7,1);(6,1);(5,0);(5,1);(4,1);(3,1);(2,1)] *)
(* let te4 = Decisions.isFluStr8 [(6,1);(5,1);(4,1);(3,1);(2,1);(7,1);(5,0)] *)
(* let teW = Decisions.isFluStr8 [(12,1);(1,1);(10,3);(5,0);(3,1);(2,1);(0,1)]  *)
(* let tu1 = Decisions.isFluStr8 [(12,3);(1,3);(4,3);(2,3);(7,1);(3,3);(0,3)] *)
(* let tu2 = Decisions.isFluStr8 [(4,1);(1,1);(3,1);(7,3);(7,1);(5,1);(2,1)] *)
(* let tu3 = Decisions.isFluStr8 [(3,0);(2,0);(12,0);(12,1);(6,3);(1,0);(0,0)] *)
(* let tu4 = Decisions.isFluStr8 [(10,0);(3,0);(8,2);(7,0);(5,0);(6,0);(4,0)] *)
(* let tu5 = Decisions.isFluStr8 [(4,1);(2,1);(1,1);(2,0);(0,1);(3,1);(8,2)] *)
(* let tu6 = Decisions.isFluStr8 [(3,2);(2,2);(4,2);(8,3);(10,0);(0,2);(1,2)] *)
(* let tu7 = Decisions.isFluStr8 [(3,0);(2,0);(5,0);(6,0);(8,2);(4,0);(12,0)] *)
(* let tu8 = Decisions.isFluStr8 [(7,3);(4,3);(5,3);(6,3);(8,3);(11,0);(3,3)] *)
(* let tu9 = Decisions.isFluStr8 [(7,3);(4,3);(12,0);(5,3);(8,3);(7,2);(6,3)] *)
(* let tua = Decisions.isFluStr8 [(1,2);(0,2);(4,2);(8,0);(3,2);(2,2);(11,3)] *)
(* let tub = Decisions.isFluStr8 [(4,1);(3,1);(1,1);(2,1);(5,1);(3,0);(5,3)] ;; *)
  
(* print_newline () ;; *)
  
(* (\*   (\\* bad one *\\) *\) *)
(* let tb1 = Decisions.isFluStr8 [(1,1);(2,1);(7,1);(3,1);(4,1);(6,1);(9,1)] *)
(* let tb2 = Decisions.isFluStr8 [(6,1);(4,1);(11,1);(2,1);(1,1);(3,1);(7,1)] *)
(* let tb3 = Decisions.isFluStr8 [(10,3);(6,4);(5,0);(7,4);(10,4);(3,4);(8,4)] *)
(* let tb4 = Decisions.isFluStr8 [(10,3);(7,1);(5,1);(1,1);(3,1);(11,1);(9,1)] *)
(* let tb5 = Decisions.isFluStr8 [(11,1);(1,1);(10,3);(5,1);(3,1);(2,1);(0,1)] *)
(* let tb6 = Decisions.isFluStr8 [(0,1);(1,1);(2,1);(3,1);(4,2);(5,1);(11,1)] *)
(* let tb7 = Decisions.isFluStr8 [(0,1);(4,1);(5,1);(6,1);(7,1);(8,2);(9,3)]  *)
(* let tb8 = Decisions.isFluStr8 [(3,0);(4,2);(5,3);(6,3);(7,3);(8,3);(11,3)]  *)
(* let tb9 = Decisions.isFluStr8 [(4,1);(7,1);(7,2);(8,1);(9,2);(10,1);(11,1)] ;;  *)

(* print_newline () ;; *)
