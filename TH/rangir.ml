open Batteries ;;

module Rangir =
  struct

    type cards = (int * int) list

    (* получаем информацию о максимальном ранге смежных карт в xs  *)
    let adjacentRanks = fun xs ->
             (* лист xs должнен быть отранжирован в нисходящем порядке *)
     let xs2 = 0 :: xs
     and xs1 = xs @ [0]
             (* собираем инфо где в руке ранги совпадают или смежны *)
     in List.combine xs1 xs2 |> List.tl |> List.filter (fun (x, y) -> y - x <= 1) |> List.hd
   ;;

   let twoPocketsCompare = fun [(r1,_); (r2,_)] [(t1,_); (t2,_)] ->
      if r1 > t1 then 1 else
        if r1 == t1 && r2 > t2 then 1 else
          if r1 < t1 then -1 else
            if r1 == t1 && r2 < t2 then -1 else 0
    ;;

    let compareRank = fun (r1, _) (r2, _) -> if r1 >= r2 then 1 else -1 ;;

    let compareSuit = fun (_, s1) (_, s2) -> if s1 >= s2 then 1 else -1 ;;

    let listPocketSort = fun xs -> match xs with
      | [(r1, _); (r2, _)] -> if r1 > r2 then xs else List.rev xs
      | _ -> xs
    ;;

    let rangeHigh = fun xss ->
      let pockets = List.map (List.take 2) xss |> List.map listPocketSort |>
                      List.sort compare |> List.rev
      in let maxRank = List.max pockets |> List.hd |> fst
         in let goodPockets = List.take_while (fun [(r1, _) ; _] -> r1 == maxRank) pockets
            in List.fold_left
                 (fun (v, st) xs ->
                  (* здесь мы существенно полагаемся на то что в списках нет пар! *)
                   match xs with
                   | [_ ; (r2, _)] ->
                      if r2 > v then (r2, [xs]) else
                        if r2 == v then (v, xs :: st) else (v, st)
                   | _ -> (v, st)
                 ) (0, []) goodPockets |> snd
    ;;

    let rangePair = fun xss ->
      let ranks = List.map (fun xs ->
                      List.fold_left
                   (* здесь мы существенно полагаемся на то, что в списках нет дупалов! *)
                        (fun (ra, sta) (ru, _) ->
                          if ra == ru then (ru, ru) else (ru, sta)
                        ) (-1, -1) xs
                    ) (List.map (List.sort compareRank) xss) |> List.map snd
      in let maxPar = List.max ranks
         in List.mapi (fun i x -> if x == maxPar then i else -1) ranks |>
              List.filter (fun x -> x > -1) |> List.map (List.nth xss) |>
              List.map (List.take 2)
    ;;

    let rangeDupal = fun xss ->
      let ranks = List.map (fun xs ->
                      List.fold_left
                        (fun (ra, sta) (ru,_) ->
                  (* здесь мы существенно полагаемся на то, что в списках нет сетов! *)
                          if ra == ru then (ru, ru :: sta) else (ru, sta)
                        ) (-1, []) xs
                    ) (List.map (List.sort compareRank) xss) |>
                    List.map snd |> List.map (fun xs -> (List.sort compare xs |> List.rev))
      in let maxDup = List.concat ranks |> List.max
         in List.mapi (fun i xs ->
                if 2 == List.length xs && maxDup == List.hd xs then i else -1) ranks |>
              List.filter (fun x -> x > -1) |> List.map (List.nth xss) |>
              List.map (List.take 2) |> List.sort twoPocketsCompare |> List.rev
    ;;

      (* получаем информацию о совпадающих рангах руки *)
    let ranksEquality = fun n xs ->
              (* лист xs должнен быть отранжирован в восходящем порядке *)
      let sta = Array.make 13 0
              (* собираем инфо о рангах руки в мутабельный массив sta  *)
      in let _ = List.iter (fun (ru, _) -> Array.set sta ru ((Array.get sta ru) + 1)) xs
         in Array.to_list sta
              (* получаем ранг на котором есть сет, т.к. индекс совпадает с номиналом карты *)
            |> (fun xs -> try List.findi (fun i x -> x == n) xs with _ -> (-1, 0))
            |> fst
    ;;
      
    let rangeSet = fun xss ->
      (* здесь мы существенно полагаемся на то, что в списках нет каре! *)
      let ranks = List.map (ranksEquality 3) @@ List.map (List.sort compareRank) xss 
      in let maxSet = List.max ranks
         in let (k, _) = List.findi (fun i x -> maxSet == x) ranks
            in [List.nth xss k] |> List.map (List.take 2) 
    ;;

    let rangeStr xss =
      let ord =
        List.map (fun xs -> List.sort compareRank xs |> List.rev |> List.map fst) xss
        (* здесь мы существенно полагаемся на то, что стрит в списках есть! *)
      in let ranks = List.map adjacentRanks ord |> List.map snd
         in let maxStr = List.max ranks
            in List.mapi (fun i xs -> if maxStr == xs then i else -1) ranks |>
                 List.filter (fun x -> x > -1) |> List.map (List.nth xss) |>
                 List.map (List.take 2) |> List.sort twoPocketsCompare |> List.rev
    ;;

    let rangeFlush xss =
      let ord = List.map (fun xs -> List.sort compareSuit xs |> List.rev |> List.map snd) xss
                (* мы существенно полагаемся на то, что флеш в листах есть! *)
      in let rez = List.map
             (fun xs ->
               let xs2 = 0 :: xs and xs1 = xs @ [0] and suit = Array.make 4 0
               in List.combine xs1 xs2 |> List.tl |> List.filter (fun (x, y) -> y == x) |>
                    List.map fst |>
                    List.iter (fun x -> Array.set suit x ((Array.get suit x) + 1)) ;
                  List.map (fun x -> x > 3) (Array.to_list suit) |>
                    List.exists (fun x -> x == true)
             ) ord
         in let (k, _) = List.findi (fun i x -> x == true) rez
            in  [List.nth xss k] |> List.map (List.take 2)
    ;;

    let rangeFull xss =
      let ord = List.map (List.sort compareRank) xss
      in let ranks3 = List.map (ranksEquality 3) ord
         and ranks2 = List.map (ranksEquality 2) ord
         in let maxSet = List.max ranks3
            and maxPar = List.max ranks2
            in let (k3, _) = List.findi (fun i x -> maxSet == x) ranks3
               and (k2, _) = List.findi (fun i x -> maxPar == x) ranks2
               in if k3 > -1 && k2 > -1 then [List.nth xss k3] |> List.map (List.take 2)
                     else []
    ;;
                       
    let rangeCare xss =
     (* здесь мы существенно полагаемся на то, что в списках нет каре! *)
      let ranks = List.map (ranksEquality 4) @@ List.map (List.sort compareRank) xss
      in let maxSet = List.max ranks
         in let (k, _) = List.findi (fun i x -> maxSet == x) ranks
            in [List.nth xss k] |> List.map (List.take 2) 
    ;;
                      
    let rangeFuSt xss = xss

  end ;;

  (* test suite *)
let te  =
  let board = [(2,0) ; (2,1) ; (7,1) ; (8,1) ; (12,3)]
  in  Rangir.rangeFull [
      [( 5, 3); ( 5, 2)] @ board ;
      [( 2, 2); ( 9, 1)] @ board ;
      [( 4, 3); ( 7, 1)] @ board ;
    ]

