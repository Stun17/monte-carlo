module Decisions =
struct

  open Batteries ;;
    
  type hand = (int * int) list

            
  let countSuit cs n = List.filter (fun (_,s) -> s = n) cs |> List.length ;;

  let countRank cs k = List.filter (fun (r,_) -> r = k) cs |> List.length ;;

    
  let isFlush cs =
    if (List.map (countSuit cs) [0;1;2;3] |> List.exists (fun x -> x >= 5))
    then cs
    else []
  ;;
       
  let isCaree cs =
    let bs = List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12]
    in if (List.exists (fun x -> x = 4) bs) 
       then cs
       else []
  ;;

  let isFull cs =
    let bs = List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12]
    in if (List.exists (fun x -> x = 3) bs && List.exists (fun x -> x = 2) bs)
       then cs
       else []
  ;;
    
  let isStraight cs =
    if (List.split cs |> fst |> List.sort compare |> List.rev |> fun zs ->
                    ((List.nth zs 0) - (List.nth zs 4) = 4)
                 || ((List.nth zs 1) - (List.nth zs 5) = 4)
                 || ((List.nth zs 2) - (List.nth zs 6) = 4)
                 || (  (List.nth zs 0) = 12 &&
                       (List.nth zs 3) =  3 &&
                       (List.nth zs 4) =  2 &&
                       (List.nth zs 5) =  1 &&
                       (List.nth zs 6) =  0 
                    ))
    then cs
    else []
  ;;

  let isFlushStr8 cs =
    let proc n = List.filter (fun (_,s) -> s = n) cs |>
                   List.split |> fst |> List.sort compare |> List.rev |>
                   fun zs ->
                   match List.length zs with
                   | 5 ->    ((List.nth zs 0) - (List.nth zs 4) = 4)
                          || ([0;1;2;3;12] = List.rev zs)
                   | 6 ->    ((List.nth zs 0) - (List.nth zs 4) = 4)
                          || ((List.nth zs 1) - (List.nth zs 5) = 4)
                          || ([0;1;2;3] = (List.rev zs |> List.take 4) && (12 = List.nth zs 0))
                   | 7 ->    ((List.nth zs 0) - (List.nth zs 4) = 4)
                          || ((List.nth zs 1) - (List.nth zs 5) = 4)
                          || ((List.nth zs 2) - (List.nth zs 6) = 4)
                          || ([0;1;2;3] = (List.rev zs |> List.take 4) && (12 = List.nth zs 0))
                   | _ -> false
    in match isFlush cs with
       | [] -> []
       | _ -> match isStraight cs with
              | [] -> []
              | _ -> if List.map proc [0;1;2;3] |> List.exists (fun x -> x = true) then cs else []
    ;;
    
  let isSet cs =
    let bs = List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12]
    in if (List.exists (fun x -> x = 3) bs)
       then cs
       else []
  ;;

  let isDupal cs =
    let bs = List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12]
    in if (List.filter (fun x -> x = 2) bs |> List.length |> fun x -> x = 2)
       then cs
       else []
  ;;
    
  let isPair cs = 
    let bs = List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12]
    in if (List.filter (fun x -> x = 2) bs |> List.length |> fun x -> x = 1)
       then cs
       else []
  ;;

  let isHigh cs = cs ;; 
      
end ;;

