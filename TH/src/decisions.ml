open Bat ;;

(* module to detect combinations in hands *)
module Decisions =
struct

  type hand = (int * int) list  (* list of pairs rank/suit *)
            
  let countSuit cs n = List.filter (fun (_,s) -> s == n) cs |> List.length ;;

  let countRank cs k = List.filter (fun (r,_) -> r == k) cs |> List.length ;;

  (* is there three [or more] cards in suit on board *)
  let isColor xs =
    List.map (countSuit xs) [0;1;2;3] |>
       List.filter (fun x -> x > 2) |>
       List.length |> fun x -> x > 0
  ;;

  (* is there pair on board *)
  let isWet xs =
    List.map (countRank xs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |>
      List.filter (fun x -> x > 1) |> List.length |> fun x -> x > 0
  ;;

  (* simple enough *)
  let isHigh cs =
    true
  ;;

  (*  is there pair in hand *)
  let isPair cs = 
    List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |>
      List.filter (fun x -> x == 2) |> List.length |> fun x -> x == 1
  ;;

  (* is there two pairs in hand *)
  let isDupal cs =
    (3 < List.length cs) &&
    (List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |>
       List.filter (fun x -> x == 2) |> List.length |> fun x -> x > 1)
  ;;

  (* is there set in hand *)
  let isSet cs =
    (2 < List.length cs) &&
      (List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |>
         List.exists (fun x -> x == 3))
  ;;

  (* is there straight in hand *)
  let isStraight cs =
    (4 < List.length cs) &&
      (List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |>
         fun [x2;x3;x4;x5;x6;x7;x8;x9;xt;xj;xq;xk;xa] ->
         (x2 > 0 && x3 > 0 && x4 > 0 && x5 > 0 && xa > 0) ||         
         (x2 > 0 && x3 > 0 && x4 > 0 && x5 > 0 && x6 > 0) ||
         (x3 > 0 && x4 > 0 && x5 > 0 && x6 > 0 && x7 > 0) ||
         (x4 > 0 && x5 > 0 && x6 > 0 && x7 > 0 && x8 > 0) ||
         (x5 > 0 && x6 > 0 && x7 > 0 && x8 > 0 && x9 > 0) ||
         (x6 > 0 && x7 > 0 && x8 > 0 && x9 > 0 && xt > 0) ||
         (x7 > 0 && x8 > 0 && x9 > 0 && xt > 0 && xj > 0) ||
         (x8 > 0 && x9 > 0 && xt > 0 && xj > 0 && xq > 0) ||
         (x9 > 0 && xt > 0 && xj > 0 && xq > 0 && xk > 0) ||
         (xt > 0 && xj > 0 && xq > 0 && xk > 0 && xa > 0) 
      )
  ;;

  (* is there flush in hand *)
  let isFlush cs =
    (4 < List.length cs) &&
      (
        List.map (countSuit cs) [0;1;2;3] |>
          List.exists (fun x -> x > 4)
      )
  ;;

  (* is there full house in hand *)
  let isFull cs =
    (4 < List.length cs) && isSet cs && isPair cs
  ;;

  (* is there four of the kind in hand *)
  let isCaree cs =
    (3 < List.length cs) &&
      (List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |>
         List.exists (fun x -> x == 4))
  ;;

  (* is there flush-straight in hand *)
  let isFlushStr8 cs =
      isFlush cs && isStraight cs &&
      (
        let pfunc n = (List.filter (fun (_, s) -> s == n) cs) |> isStraight
        in (List.map pfunc [0;1;2;3]) |> List.exists (fun x -> x == true)
      )
  ;;

end
