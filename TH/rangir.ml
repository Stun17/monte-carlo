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
                  
    let compareRank = fun (r1,s1) (r2,s2) -> if r1 >= r2 then 1 else -1 ;;
               
    let pocketRank = fun xs ->
      match xs with
      | [(r1,_); (r2,_)] -> if r1 > r2 then xs else List.rev xs 
      | _ -> xs
    ;;

        (* здесь мы существенно полагаемся на то что в списках нет пар! *)
    let rangeHigh = fun css ->
      let pockets = List.map (List.take 2) css |>
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
            
    let rangePair = fun css ->
         (* здесь мы существенно полагаемся на то, что в списках нет сетов! *)
      let ranks = List.map (fun cs ->
                      List.fold_left
                        (fun (ra, st) (ru, _) ->
                          if ra == ru then (ru, ru) else (ru, st)
                        ) (-1, -1) cs
                    ) (List.map (List.sort compareRank) css) |> List.map snd
      in let maxV = List.max ranks 
         in List.mapi (fun i x -> if x == maxV then i else -1) ranks |>
              List.filter (fun x -> x > -1) |> List.map (List.nth css) |> List.map (List.take 2)

    let rangeDupal = fun css ->
        (* здесь мы существенно полагаемся на то, что в списках нет сетов! *)
      let ranks = List.map (fun cs -> 
                      List.fold_left 
                        (fun (ra, st) (ru,_) -> 
                          if ra == ru then (ru, ru :: st) else (ru, st) 
                        ) (-1, []) cs
                    ) (List.map (List.sort compareRank) css) |>
                    List.map snd |> List.map (fun xs -> (List.sort compare xs |> List.rev))
      in let maxVV = List.concat ranks |> List.max 
         in List.mapi (fun i xs ->
                if 2 == List.length xs && maxVV == List.hd xs then i else -1) ranks |>
              List.filter (fun x -> x > -1) |> List.map (List.nth css) |> 
              List.map (List.take 2) |> List.sort dRank |> List.rev (* |> List.hd  *)

    let rangeSet = fun css ->
     (* здесь мы существенно полагаемся на то, что в списках нет каре! *)
      let ranks = List.map (fun cs ->
                      List.fold_left
                        (fun (flag, state) (ru, _) ->
                          if List.is_empty state then (1, ru :: state) else
                            if ru == List.hd state then (flag + 1, state) else
                              if ru != List.hd state && flag != 3 then (0, [])
                              else (flag, state)
                        ) (0, []) cs
                    ) (List.map (List.sort compareRank) css) |> List.map snd 
     in let maxVVV = List.concat ranks |> List.max
        in List.mapi (fun i xs ->
               if not (List.is_empty xs) && maxVVV == List.hd xs then i else -1) ranks |>
             List.filter (fun x -> x > -1) |> List.map (List.nth css) |> 
              List.map (List.take 2) |> List.sort dRank |> List.rev (* |> List.hd  *)

    let rangeStr css = css
    let rangeFlush css = css
    let rangeFull css = css
    let rangeCare css = css
    let rangeFuSt css = css

  end ;;


                   

  (* test suite *)
let board = [(3,1);(12,3);(12,1);(11,3);(3,2)] ;;

let tu1= Rangir.rangeHigh
            [
              [(11, 1); (11, 2)] @ board ;
              [(10, 2); (10, 1)] @ board ;
              [( 12, 2); ( 7, 1)] @ board ;
            ]

let tu2 = Rangir.rangePair
            [
              [(11, 1); (11, 2)] @ board ;
              [(10, 2); (10, 1)] @ board ;
              [( 12, 2); ( 7, 1)] @ board ;
            ]
        
let tu3 = Rangir.rangeDupal
            [
              [(11, 1); (11, 2)] @ board ;
              [(10, 2); (10, 1)] @ board ;
              [( 12, 2); ( 7, 1)] @ board ;
            ]

        
let tu4= Rangir.rangeSet
            [
              [(11, 1); (11, 2)] @ board ;
              [(10, 2); (10, 1)] @ board ;
              [( 12, 2); ( 7, 1)] @ board ;
            ]
        
