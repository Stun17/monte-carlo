module Decisions =
struct

  type hand = (int * int) list

            
  let countSuit cs n = List.filter (fun (_,s) -> s = n) cs |> List.length ;;

  let countRank cs k = List.filter (fun (r,_) -> r = k) cs |> List.length ;;
                                                                                  
  let isFlush cs =
    List.map (countSuit cs) [0;1;2;3] |> List.exists (fun x -> x >= 5) ;;
       
  let isCaree cs =
    let bs = List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12]
    in List.exists (fun x -> x = 4) bs ;;

  let isFull cs =
    let bs = List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12]
    in List.exists (fun x -> x = 3) bs && List.exists (fun x -> x = 2) bs
  ;;
    
  let isStraight cs =
    List.split cs |> fst |> List.sort compare |> fun zs ->
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

  let isFlushStr8 cs =
    if isFlush cs && isStraight cs
    then    isStraight (List.filter (fun (r,s) -> s = 0) cs)
         || isStraight (List.filter (fun (r,s) -> s = 1) cs)
         || isStraight (List.filter (fun (r,s) -> s = 2) cs)
         || isStraight (List.filter (fun (r,s) -> s = 3) cs)
    else false ;;
    
  let isSet cs =
    let bs = List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12]
    in List.exists (fun x -> x = 3) bs ;;

  let isDupal cs =
    let bs = List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12]
    in List.filter (fun x -> x = 2) bs |> List.length |> fun x -> x = 2 ;;
    
  let isPair cs = 
    let bs = List.map (countRank cs) [0;1;2;3;4;5;6;7;8;9;10;11;12]
    in List.filter (fun x -> x = 2) bs |> List.length |> fun x -> x = 1 ;;

  let isHigh cs = true ;; 
      
end ;;

