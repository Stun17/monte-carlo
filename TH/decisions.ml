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
    let bs = List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12]
    in List.filter (fun x -> x == 2) bs |> List.length |> fun x -> x == 1
  ;;

  let isDupal cs =
    let bs = List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12]
    in List.filter (fun x -> x == 2) bs |> List.length |> fun x -> x == 2
  ;;
    
  let isSet cs =
    let bs = List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12]
    in List.exists (fun x -> x == 3) bs
  ;;
    
  let isStraight cs =
    List.split cs |> fst |> List.sort compare |> List.rev |>
      fun zs ->
             ( (List.nth zs 0) - (List.nth zs 4) == 4 &&
               (List.nth zs 1) - (List.nth zs 4) == 3 &&
               (List.nth zs 2) - (List.nth zs 4) == 2 &&
               (List.nth zs 3) - (List.nth zs 4) == 1
             )
          || ( (List.nth zs 1) - (List.nth zs 5) == 4 &&
               (List.nth zs 2) - (List.nth zs 5) == 3 &&
               (List.nth zs 3) - (List.nth zs 5) == 2 &&
               (List.nth zs 4) - (List.nth zs 5) == 1
             )
          || ( (List.nth zs 2) - (List.nth zs 6) == 4 &&
               (List.nth zs 3) - (List.nth zs 6) == 3 &&
               (List.nth zs 4) - (List.nth zs 6) == 2 &&
               (List.nth zs 5) - (List.nth zs 6) == 1
             )
          || ( (List.nth zs 0) == 12 &&
               (List.nth zs 3) == 3  &&
               (List.nth zs 4) == 2  &&
               (List.nth zs 5) == 1  &&
               (List.nth zs 6) == 0
             )
  ;;

  let isFlush cs =
    List.map (countSuit cs) [0;1;2;3] |> List.exists (fun x -> x >= 5)
  ;;
      
  let isFull cs =
    let bs = List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12]
    in List.exists (fun x -> x == 3) bs && List.exists (fun x -> x == 2) bs
  ;;
       
  let isCaree cs =
    let bs = List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12]
    in List.exists (fun x -> x == 4) bs 
  ;;

  let isFlushStr8 cs =
    let proc n =
      List.filter (fun (_, s) -> s == n) cs |> List.split |> fst |> 
      List.sort compare |> List.rev |>
        fun zs ->
        match List.length zs with
        | 5 ->    ((List.nth zs 0) - (List.nth zs 4) == 4)
                  || ([0;1;2;3;12] == List.rev zs)
        | 6 ->    ((List.nth zs 0) - (List.nth zs 4) == 4)
                  || ((List.nth zs 1) - (List.nth zs 5) == 4)
                  || ([0;1;2;3] == (List.rev zs |> Bat.take 4) && (12 == List.nth zs 0))
        | 7 ->    ((List.nth zs 0) - (List.nth zs 4) == 4)
                  || ((List.nth zs 1) - (List.nth zs 5) == 4)
                  || ((List.nth zs 2) - (List.nth zs 6) == 4)
                  || ([0;1;2;3] == (List.rev zs |> Bat.take 4) && (12 == List.nth zs 0))
        | _ -> false
    in if (isFlush cs) && (isStraight cs) 
       then List.map proc [0;1;2;3] |> List.exists (fun x -> x == true)
       else false 
    ;;
    
end ;;
