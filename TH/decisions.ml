open Bat ;;

module Decisions =
struct

  type hand = (int * int) list  (* list of pairs rank/suit *)
            
  let countSuit cs n = List.filter (fun (_,s) -> s == n) cs |> List.length ;;

  let countRank cs k = List.filter (fun (r,_) -> r == k) cs |> List.length ;;

  (* is there three or more cards in suit *)
  let isColor xs =
    (4 < List.length xs) &&
      (List.map (countSuit xs) [0;1;2;3] |>
         List.filter (fun x -> x > 2) |>
         List.length |> fun x -> x > 0
      )
  ;;

  (* is there pair *)
  let isWet xs =
    (3 < List.length xs) &&
    (List.map (countRank xs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |>
       List.filter (fun x -> x > 1) |> List.length |> fun x -> x > 0
    )
  ;;

  let isHigh cs = true ;;

  let isPair cs = 
    List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |>
      List.filter (fun x -> x == 2) |> List.length |> fun x -> x == 1
  ;;

  let isDupal cs =
    (3 < List.length cs) &&
    (List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |>
       List.filter (fun x -> x == 2) |> List.length |> fun x -> x == 2)
  ;;
    
  let isSet cs =
    (2 < List.length cs) &&
    (List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |> List.exists (fun x -> x == 3))
  ;;

  let isStraight cs =
    (4 < List.length cs) &&
    (List.split cs |> fst |> List.sort compare |>
       fun xs ->
       match xs with
       | [m0;m1;m2;m3;m4;m5;m6] ->
             ( m4 - m0 == 4 && m4 - m1 == 3 && m4 - m2 == 2 && m4 - m3 == 1 )
          || ( m5 - m1 == 4 && m5 - m2 == 3 && m5 - m3 == 2 && m5 - m4 == 1 )
          || ( m6 - m2 == 4 && m6 - m3 == 3 && m6 - m4 == 2 && m6 - m5 == 1 )
          || ( m6 == 12 && m3 == 3  && m2 == 2  && m1 == 1  && m0 == 0 )
       | [m0;m1;m2;m3;m4;m5] ->
             ( m5 - m1 == 4 && m5 - m2 == 3 && m5 - m3 == 2 && m5 - m4 == 1 )
          || ( m4 - m0 == 4 )
       | [m0;m1;m2;m3;m4] ->
             ( m4 - m0 == 4 )           
       | _ -> false
    )
  ;;

  let isFlush cs =
    (5 > List.length cs) && (List.map (countSuit cs) [0;1;2;3] |> List.exists (fun x -> x > 4))
  ;;
      
  let isFull cs = (4 < List.length cs) && isSet cs && isPair cs ;;
    
  let isCaree cs = 
    (3 < List.length cs) &&
    (List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |> List.exists (fun x -> x == 4))
  ;;

  let isFlushStr8 cs = 
      isFlush cs && isStraight cs &&
      (
        let p n = (List.filter (fun (_, s) -> s == n) cs) |> isStraight
        in (List.map p [0;1;2;3]) |> List.exists (fun x -> x == true)
      )
  ;;
    
end ;;
