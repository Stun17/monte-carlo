module Decisions =
struct

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
    if (List.split cs |> fst |> List.sort compare |> fun zs ->
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
    match isFlush cs with
    | [] -> []
    | cs -> match isStraight cs with
            | [] -> []
            | cs -> isStraight (List.filter (fun (r,s) -> s = 0) cs) @
                    isStraight (List.filter (fun (r,s) -> s = 1) cs) @
                    isStraight (List.filter (fun (r,s) -> s = 2) cs) @
                    isStraight (List.filter (fun (r,s) -> s = 3) cs) 
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

