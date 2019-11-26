open Bat ;;
  
module Evaluations =
  struct

    type hand = (int * int) list ;; (* list of pairs rank/suit *)

    let takeRanks xs = List.sort compare xs |> List.rev |> List.split |> fst ;;
              
    let priceHigh (xs : hand) = takeRanks xs |> List.hd ;;

    let pricePair (xs : hand) =
      let k0 = takeRanks xs |> 
      fun ys -> 
        List.fold_right (fun x acc -> if x == acc then acc else x)
        (List.tl ys)
        (List.hd ys)
      in let k1 = List.filter (fun (r,_) -> r != k0) xs |> priceHigh
         in let k2 = List.filter (fun (r,_) -> r != k0 && r != k1) xs |> priceHigh
            in let k3 = List.filter (fun (r,_) -> r != k0 && r != k1 && r != k2) xs |> priceHigh
               in 3000 * (k0 + 1) + 200 * (k1 + 1) + 14 * (k2 + 1) + k3
    ;;

    let priceDupal (xs : hand) =
      let (n1, n2) = takeRanks xs |> 
        fun ys ->
          List.map2 (fun s1 s2 -> if s1 = s2 then s1 else 0) (ys @ [0]) (0 :: ys) |>
          List.sort compare |> List.rev |>
          fun zs -> List.hd zs , List.hd (List.tl zs)
      in let k = List.filter (fun (r,_) -> r != n1 && r != n2) xs |> priceHigh 
         in 300 * (n1 + 1) + 14 * (n2 + 1) + k
    ;;

    let priceSet (xs : hand) =
      let n = takeRanks xs |> 
        fun ys ->
          List.map2 (fun s1 s2 -> if s1 = s2 then s1 else 0) (ys @ [0]) (0 :: ys) |>
          List.sort compare |> List.rev |> List.hd
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
      and g ys = List.sort compare ys |> List.rev |> List.hd |> fst 
      in let spades_s = f 0
         and clubs__s = f 1
         and diams__s = f 2 
         and hearts_s = f 3
         in if 4 < List.length spades_s then g spades_s else
              if 4 < List.length clubs__s then g clubs__s else
                if 4 < List.length diams__s then g diams__s else g hearts_s 
    ;;

    let priceFull (xs : hand) =
      let countRank n = List.filter (fun (r, _) -> r == n) xs |> List.length
      in let ys = List.map countRank [0;1;2;3;4;5;6;7;8;9;10;11;12]
         in let fset = List.filter (fun x -> x == 3) ys |> List.hd 
            and fpar = List.filter (fun x -> x == 2) ys |> List.hd 
            in 14 * (fset + 1) + fpar
    ;;

    let priceCaree (xs : hand) =
      let countRank n = List.filter (fun (r, _) -> r == n) xs |> List.length
      in let n =
           List.map countRank [0;1;2;3;4;5;6;7;8;9;10;11;12] |>
             List.filter (fun x -> x == 4) |> List.hd
         in let k = List.filter (fun (r,_) -> r != n) xs |> priceHigh
            in 14 * (n + 1) + k
    ;;

    let priceFlushStr8 (xs : hand) = min (priceFlush xs) (priceStr8 xs) ;;
     
  end
;;  
