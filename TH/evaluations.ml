open Bat ;;
  
module Evaluations =
  struct

    type hand = (int * int) list (* list of pairs rank/suit *)
    
    let priceHigh (xs : hand) =
      List.sort compare xs |> List.rev |> List.split |> fst |> List.hd
    ;;

    let pricePair (xs : hand) =
      let n = List.sort compare xs |> List.rev |> List.split |> fst |>
                fun ys ->
                List.fold_right
                  (fun x acc -> if x == acc then acc else x)
                  (List.tl ys)
                  (List.hd ys)
      in let k1 = List.filter (fun (r,_) -> r != n) xs |> priceHigh
         in let k2 = List.filter (fun (r,_) -> r != n && r != k1) xs |> priceHigh
            in let k3 = List.filter (fun (r,_) -> r != n && r != k1 && r != k2) xs |> priceHigh
               in 3000 * (n + 1) + 200 * (k1 + 1) + 13 * (k2 + 1) + k3
    ;;

    let priceDupal (xs : hand) =
      let (n1, n2) =
        List.sort compare xs |> List.rev |> List.split |> fst |>
          fun ys ->
          let ys1 = ys @ [0]
          and ys2 = 0 :: ys
          in List.map2 (fun s1 s2 -> if s1 = s2 then s1 else 0) ys1 ys2 |>
               List.sort compare |> List.rev |>
               fun zs -> List.hd zs , List.hd (List.tl zs)
      in let k = List.filter (fun (r,_) -> r != n1 && r != n2) xs |> priceHigh 
         in 3000 * (n1 + 1) + 14 * (n2 + 1) + k
    ;;

    let priceSet (xs : hand) =
      let n = List.sort compare xs |> List.rev |> List.split |> fst |>
                fun ys ->
                List.map2 (fun s1 s2 -> if s1 = s2 then s1 else 0) (ys @ [0]) (0 :: ys) |>
                  List.sort compare |> List.rev |> List.hd
      in let k1 = List.filter (fun (r, _) -> r != n) xs |> priceHigh
         in let k2 = List.filter (fun (r, _) -> r != n && r != k1) xs |> priceHigh
            in 3000 * (n + 1) + 14 * (k1 + 1) + k2
    ;;

    let priceStr8 (xs : hand) =
      List.sort compare xs |> List.rev |> List.split |> fst |>
        fun ys ->
        let ys1 = ys @ [0]
        and ys2 = 0 :: ys
        in List.map2 (fun s1 s2 -> if s2 - s1 = 1 then s2 else 0) ys1 ys2 |>
             List.sort compare |> List.rev |> List.hd  

    let priceFlush (xs : hand) =
      let f n  = List.filter (fun (_, s) -> s == n) xs
      and g ys = List.sort compare ys |> List.rev |> List.hd |> fst 
      in let ss = f 0
         and cs = f 1
         and ds = f 2 
         and hs = f 3
         in if 4 < List.length ss then g ss else
              if 4 < List.length cs then g cs else
                if 4 < List.length ds then g ds else g hs 
    ;;

    let priceFull (xs : hand) =
      let countRank n = List.filter (fun (r, _) -> r == n) xs |> List.length
      in let ys = List.map countRank [0;1;2;3;4;5;6;7;8;9;10;11;12]
         in let fset = List.filter (fun x -> x == 3) ys |> List.hd 
            and fpar = List.filter (fun x -> x == 2) ys |> List.hd 
            in 200 * (fset + 1) + fpar
    ;;

    let priceCaree (xs : hand) =
      let countRank n = List.filter (fun (r, _) -> r == n) xs |> List.length
      in let n =
           List.map countRank [0;1;2;3;4;5;6;7;8;9;10;11;12] |>
             List.filter (fun x -> x == 4) |> List.hd
         in let k = List.filter (fun (r,_) -> r != n) xs |> priceHigh
            in 200 * (n + 1) + k
    ;;

    let priceFlushStr8 (xs : hand) =
      let fd = priceFlush xs
      and sd = priceStr8  xs
      in min fd sd
    ;;
     
  end ;;  
