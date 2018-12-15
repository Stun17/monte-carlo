open Batteries ;;

module Rangir =
  struct
    
    type cards = (int * int) list

    let dRank = fun [(r1,_);(r2,_)] [(t1,_);(t2,_)] ->
      if r1 > t1 then 1 else
        if r1 == t1 && r2 > t2 then 1 else
          if r1 < t1 then -1 else
            if r1 == t1 && r2 < t2 then -1 else 0
    ;;
                  
    let compareRank = fun (r1, _) (r2, _) -> if r1 >= r2 then 1 else -1 ;;

    let compareSuit = fun (_, s1) (_, s2) -> if s1 >= s2 then 1 else -1 ;;      
               
    let pocketRank = fun xs ->
      match xs with
      | [(r1,_); (r2,_)] -> if r1 > r2 then xs else List.rev xs 
      | _ -> xs
    ;;

    let rangeHigh = fun xss ->
        (* здесь мы существенно полагаемся на то что в списках нет пар! *)
      let pockets = List.map (List.take 2) xss |>
                      List.map pocketRank |>
                      List.sort compare |> List.rev
      in let maxRank = List.max pockets |> List.hd |> fst
         in let goodPockets = List.take_while (fun [(r1, _) ; _] -> r1 == maxRank) pockets
            in List.fold_left
                 (fun (v, st) xs ->
                   match xs with
                   | [_ ; (r2, _)] ->
                      if r2 > v then (r2, [xs]) else
                        if r2 == v then (v, xs :: st) else (v, st)
                   | _ -> (v, st)
                 ) (0, []) goodPockets |> snd
    ;;
            
    let rangePair = fun xss ->
         (* здесь мы существенно полагаемся на то, что в списках нет сетов! *)
      let ranks = List.map (fun xs ->
                      List.fold_left
                        (fun (ra, state) (ru, _) ->
                          if ra == ru then (ru, ru) else (ru, state)
                        ) (-1, -1) xs
                    ) (List.map (List.sort compareRank) xss) |> List.map snd
      in let maxPar = List.max ranks 
         in List.mapi (fun i x -> if x == maxPar then i else -1) ranks |>
              List.filter (fun x -> x > -1) |> List.map (List.nth xss) |>
              List.map (List.take 2)
    ;;

    let rangeDupal = fun xss ->
        (* здесь мы существенно полагаемся на то, что в списках нет сетов! *)
      let ranks = List.map (fun xs -> 
                      List.fold_left 
                        (fun (ra, st) (ru,_) -> 
                          if ra == ru then (ru, ru :: st) else (ru, st) 
                        ) (-1, []) xs
                    ) (List.map (List.sort compareRank) xss) |>
                    List.map snd |> List.map (fun xs -> (List.sort compare xs |> List.rev))
      in let maxDup = List.concat ranks |> List.max 
         in List.mapi (fun i xs ->
                if 2 == List.length xs && maxDup == List.hd xs then i else -1) ranks |>
              List.filter (fun x -> x > -1) |> List.map (List.nth xss) |> 
              List.map (List.take 2) |> List.sort dRank |> List.rev
    ;;

    let rangeSet = fun xss ->
     (* здесь мы существенно полагаемся на то, что в списках нет каре! *)
      let ranks = List.map (fun xs ->
                      List.fold_left
                        (fun (flag, state) (ru, _) ->
                          if List.is_empty state then (1, ru :: state) else
                            if ru == List.hd state then (flag + 1, state) else
                              if ru != List.hd state && flag != 3 then (0, [])
                              else (flag, state)
                        ) (0, []) xs
                    ) (List.map (List.sort compareRank) xss) |> List.map snd 
     in let maxSet = List.concat ranks |> List.max
        in List.mapi (fun i xs ->
               if not (List.is_empty xs) && maxSet == List.hd xs then i else -1) ranks |>
             List.filter (fun x -> x > -1) |> List.map (List.nth xss) |> 
             List.map (List.take 2) |> List.sort dRank |> List.rev
    ;;
      
    let rangeStr xss =
      let ord =
        List.map (fun xs -> List.sort compareRank xs |> List.rev |> List.map fst) xss
        (* здесь мы существенно полагаемся на то, что стрит в списках есть! *)
      in let ranks =
           (List.map (fun xs ->
                let xs2 = 0 :: xs and xs1 = xs @ [0] in
                List.combine xs1 xs2 |> List.tl |> List.filter (fun (x,y) -> y - x < 2) |> List.hd
              ) ord
           ) |> List.map snd
         in let maxStr = List.max ranks
            in List.mapi (fun i xs -> if maxStr == xs then i else -1) ranks |>
                 List.filter (fun x -> x > -1) |> List.map (List.nth xss) |> 
                 List.map (List.take 2) |> List.sort dRank |> List.rev
    ;;
      
    let rangeFlush xss =
      let ord =
        List.map (fun xs -> List.sort compareSuit xs |> List.rev |> List.map snd) xss
      in let rez =
           List.map
             (fun xs ->
               let xs2 = 0 :: xs and xs1 = xs @ [0] and suit = Array.make 4 0
               in List.combine xs1 xs2 |> List.tl |> List.filter (fun (x,y) -> y == x) |>
                    List.map fst |>
                    List.iter (fun x -> Array.set suit x ((Array.get suit x) + 1)) ;
                  List.map (fun x -> x > 3) (Array.to_list suit) |>
                    List.exists (fun x -> x == true)
             ) ord
         in let (k,_) = List.findi (fun i x -> x == true) rez
            in  [List.nth xss k] |> List.map (List.take 2)
    ;;
      

    let rangeFull xss = xss
    let rangeCare xss = xss
    let rangeFuSt xss = xss

  end ;;

  (* test suite *)
let board = [(2,0) ; (5,1) ; (6,1) ; (8,1) ; (10,0)] ;;

let tuFlush = Rangir.rangeFlush
              [
              [( 4, 1); ( 9, 3)] @ board ;
              [( 7, 1); ( 9, 1)] @ board ;
              [( 4, 2); ( 7, 1)] @ board ;
            ]
        
