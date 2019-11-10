open Bat ;;

module Decisions =
struct

  type hand = (int * int) list  (* list of pairs rank/suit *)
            
  let countSuit cs n = List.filter (fun (_,s) -> s == n) cs |> List.length ;;

  let countRank cs k = List.filter (fun (r,_) -> r == k) cs |> List.length ;;

  (* is there three or more cards in suit *)
  let isColor xs =
    List.map (countSuit xs) [0;1;2;3] |>
    List.filter (fun x -> x > 2) |> 
    List.length |> fun x -> x > 0
  ;;

  (* is there pair *)
  let isWet xs =
    List.map (countRank xs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |>
    List.filter (fun x -> x > 1) |> 
    List.length |> fun x -> x > 0
  ;;

  let isHigh cs = true ;;

  let isPair cs = 
    List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |>
      List.filter (fun x -> x == 2) |> List.length |> fun x -> x == 1
  ;;

  let isDupal cs =
    List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |>
      List.filter (fun x -> x == 2) |> List.length |> fun x -> x == 2
  ;;
    
  let isSet cs =
    List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |>
      List.exists (fun x -> x == 3)
  ;;
    
  let isStraight cs =
    List.split cs |> fst |> List.sort compare |> List.rev |>
      fun [m0;m1;m2;m3;m4;m5;m6] ->
             ( m0 - m4 == 4 && m1 - m4 == 3 && m2 - m4 == 2 && m3 - m4 == 1)
          || ( m1 - m5 == 4 && m2 - m5 == 3 && m3 - m5 == 2 && m4 - m5 == 1)
          || ( m2 - m6 == 4 && m3 - m6 == 3 && m4 - m6 == 2 && m5 - m6 == 1)
          || ( m0 == 12 && m3 == 3  && m4 == 2  && m5 == 1  && m6 == 0)
  ;;

  let isFlush cs =
    List.map (countSuit cs) [0;1;2;3] |> List.exists (fun x -> x >= 5)
  ;;
      
  let isFull cs =
    let bs = List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12]
    in List.exists (fun x -> x == 3) bs && List.exists (fun x -> x == 2) bs
  ;;
       
  let isCaree cs =
    List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12] |>
      List.exists (fun x -> x == 4)
  ;;

  let isFlushStr8 cs =
    let proc n =
      List.filter (fun (_, s) -> s == n) cs |> List.split |> fst |> 
      List.sort compare |> List.rev |>
        fun zs ->
        match zs with
        | [m0;m1;m2;m3;m4] -> 
            (m0 - m4 == 4) || ([0;1;2;3;12] == List.rev zs)
        | [m0;m1;m2;m3;m4;m5] -> 
            (m0 - m4 == 4) || (m1 - m5 == 4) || 
            ([0;1;2;3] == (List.rev zs |> Bat.take 4) && 12 == m0)
        | [m0;m1;m2;m3;m4;m5;m6] -> 
            (m0 - m4 == 4) || (m1 - m5 == 4) || (m2 - m6 == 4) || 
            ([0;1;2;3] == (List.rev zs |> Bat.take 4) && 12 == m0)
        | _ -> false
    in (isFlush cs) &&
         (isStraight cs) &&
           (List.map proc [0;1;2;3] |> List.exists (fun x -> x == true))
    ;;
    
end ;;
