open Bat ;;
  
module Evaluations =
  struct

    type hand = (int * int) list (* list of pairs rank/suit *)
    
    let priceHigh (xs : hand) =
      List.sort compare xs |> List.rev |> List.split |> fst |> List.hd
    ;;

    let pricePair (xs : hand) =
      List.sort compare xs |> List.rev |> List.split |> fst |>
        fun ys ->
        List.fold_right
          (fun x acc -> if x == acc then acc else x)
          (List.tl ys)
          (List.hd ys) 
    ;;

    let priceDupal (xs : hand) =
      List.sort compare xs |> List.rev |> List.split |> fst |>
        fun ys ->
        let ys1 = ys @ [0]
        and ys2 = 0 :: ys
        in List.map2 (fun s1 s2 -> if s1 = s2 then s1 else 0) ys1 ys2 |>
             List.sort compare |> List.rev |>
             fun zs -> 13 * (1 + (List.hd zs)) + (List.hd (List.tl zs))
    ;;

    let priceSet (xs : hand) =
      List.sort compare xs |> List.rev |> List.split |> fst |>
        fun ys ->
        List.map2 (fun s1 s2 -> if s1 = s2 then s1 else 0) (ys @ [0]) (0 :: ys) |>
          List.sort compare |> List.rev |> List.hd
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
            in 13 * (fset + 1) + fpar
    ;;

    let priceCaree (xs : hand) =
      8
    ;;

    let priceFlushStr8 (xs : hand) =
      9
    ;;
     
  end ;;  
