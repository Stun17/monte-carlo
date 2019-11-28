open Bat ;;
  
module Evaluations =
  struct

    (* 
       important! prices for different combinations never-ever compaired
       only prices just for the same combination compaired 
       so - it doesnt matter how low or how high price comparing with another combination
       pls - take it into account
       but in just the same combination win hand will always have the higher price
    *)
    
    type hand = (int * int) list ;; (* list of pairs rank/suit *)

      
    let takeRanks xs = List.sort compare xs |> List.rev |> List.split |> fst ;;

    let priceHigh (xs : hand) = takeRanks xs |> List.hd ;;

    let pricePair (xs : hand) =
      let [x0;x1;x2;x3;x4;x5;x6] = takeRanks xs
      in let n1 =
           if x0 == x1 then x0 else
             if x1 == x2 then x1 else
               if x2 == x3 then x2 else 
                 if x3 == x4 then x3 else 
                   if x4 == x5 then x4 else x5
      in let k1 = List.filter (fun (r,_) -> r != n1) xs |> priceHigh
         in let k2 = List.filter (fun (r,_) -> r != n1 && r != k1) xs |> priceHigh
            in let k3 = List.filter (fun (r,_) -> r != n1 && r != k1 && r != k2) xs |> priceHigh
               in 3000 * (n1 + 1) + 200 * (k1 + 1) + 14 * (k2 + 1) + k3
    ;;

    let priceDupal (xs : hand) =
      let [x0;x1;x2;x3;x4;x5;x6] = takeRanks xs
      in let n1 =
           if x0 == x1 then x0 else
             if x1 == x2 then x1 else
               if x2 == x3 then x2 else x3
         in let [y0;y1;y2;y3;y4] = List.split xs |> fst |> List.filter (fun x -> x != n1) |>
                                     List.sort compare |> List.rev
            in let n2 =
                 if y0 == y1 then y0 else
                   if y1 == y2 then y1 else
                     if y2 == y3 then y2 else y3
               in let k = List.filter (fun (r,_) -> r != n1 && r != n2) xs |> priceHigh 
                  in 300 * (n1 + 1) + 14 * (n2 + 1) + k
    ;;

    let priceSet (xs : hand) =
      let [x0;x1;x2;x3;x4;x5;x6] = takeRanks xs
      in let n =
           if x0 == x1 && x0 == x2 then x0 else
             if x1 == x2 && x2 == x3 then x1 else
               if x2 == x3 && x2 == x4 then x2 else
                 if x3 == x4 && x3 == x5 then x3 else x4
      in let k1 = List.filter (fun (r, _) -> r != n) xs |> priceHigh
         in let k2 = List.filter (fun (r, _) -> r != n && r != k1) xs |> priceHigh
            in 300 * (n + 1) + 14 * (k1 + 1) + k2
    ;;

    let priceStr8 (xs : hand) =
      takeRanks xs |> 
      fun ys ->
        List.map2 (fun s1 s2 -> if s2 - s1 = 1 then s2 else 0) (ys @ [0]) (0 :: ys) |>
        List.sort compare |> List.rev |> List.hd  

    let priceFlush (xs : hand) =
      let f n  = List.filter (fun (_, s) -> s == n) xs
      and g ys = takeRanks ys |> List.hd 
      in let spades_s = f 0
         and clubs__s = f 1
         and diams__s = f 2 
         and hearts_s = f 3
         in if 4 < List.length spades_s then g spades_s else
              if 4 < List.length clubs__s then g clubs__s else
                if 4 < List.length diams__s then g diams__s else g hearts_s 
    ;;

    let priceFull (xs : hand) =
      let [x0;x1;x2;x3;x4;x5;x6] = takeRanks xs
      in let fset =
           if x0 == x1 && x0 == x2 then x0 else
             if x1 == x2 && x2 == x3 then x1 else
               if x2 == x3 && x2 == x4 then x2 else
                 if x3 == x4 && x3 == x5 then x3 else x4
         and fpar =
           if x0 == x1 && x1 != x2 then x0 else
             if x1 == x2 && x2 != x3 then x1 else
               if x2 == x3 && x2 != x4 then x2 else
                 if x3 == x4 && x3 != x5 then x3 else
                   if x4 == x5 && x5 != x6 then x4 else x5
         in 14 * (fset + 1) + fpar
    ;;

    let priceCaree (xs : hand) =
      let [x0;x1;x2;x3;x4;x5;x6] = takeRanks xs
      in let n =
           if x0 == x1 && x0 == x2 && x0 == x3 then x0 else
             if x1 == x2 && x1 == x3 && x1 == x4 then x1 else
               if x2 == x3 && x2 == x4 && x2 = x5 then x2 else x3
            in let k = List.filter (fun (r,_) -> r != n) xs |> priceHigh
               in 14 * (n + 1) + k
    ;;

    let priceFlushStr8 (xs : hand) =
      let f n  = List.filter (fun (_, s) -> s == n) xs
      and g ys =
        let zs = takeRanks ys
        in match zs with
           | [x7;x6;x5;x4;x3;x2;x1] ->
              if x7 - x6 == 1 && x6 - x5 == 1 && x5 - x4 == 1 then x7 else
                if x6 - x5 == 1 && x5 - x4 == 1 && x4 - x3 == 1 then x6 else
                  if x7 == 12 && x1 == 0 && x5 - x4 > 1 then x4 else x5
           | [x6;x5;x4;x3;x2;x1] ->
              if x6 - x5 == 1 && x5 - x4 == 1 && x4 - x3 == 1 then x6 else
                if x6 == 12 && x1 == 0 && x5 - x4 > 1 then x4 else x5              
           | _ ->
              if 12 == List.hd zs && 11 == (List.hd (List.tl zs)) then 12 else
                if 12 == List.hd zs && 0 == (List.hd (List.rev zs)) then 3 else List.hd zs
      in let spades_s = f 0
         and clubs__s = f 1
         and diams__s = f 2 
         and hearts_s = f 3
         in if 4 < List.length spades_s then g spades_s else
              if 4 < List.length clubs__s then g clubs__s else
                if 4 < List.length diams__s then g diams__s else g hearts_s 
    ;;
     
  end
;;  
