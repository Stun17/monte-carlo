open Batteries ;;

module Rangir =
  struct
    
    type cards = int * int list

    let dRank = fun [(r1,_);(r2,_)] [(t1,_);(t2,_)] ->
      if r1 > t1 then 1 else
        if r1 == t1 && r2 > t2 then 1 else
          if r1 < t1 then -1 else
            if r1 == t1 && r2 < t2 then -1 else 0
    ;;
                  
    let cRank = fun (r1,s1) (r2,s2) -> if r1 > r2 then 1 else -1 ;;
               
    let pRank = fun xs ->
      match xs with
      | [(r1,_); (r2,_)] -> if r1 >= r2 then xs else List.rev xs 
      | _ -> xs
    ;;

           
    let rangeHigh = fun css ->
      let pockets = List.map (List.take 2) css |> List.map pRank |> List.sort compare |> List.rev
      in let maxRank = List.max pockets |> List.hd |> fst
         in let goodPockets = List.take_while (fun [(r1, _) ; _] -> r1 == maxRank) pockets
            in List.fold_left ( fun (v, st) xs ->
                                match xs with
                                | [_ ; (r2, _)] ->
                                   if r2 > v then (r2, [xs]) else
                                     if r2 == v then (v, xs :: st) else (v, st)
                                | _ -> (v, st)
                              )
                              (0, []) goodPockets |> snd
    ;;
            
    let rangePairs = fun css ->
      let ranks = List.map (fun cs ->
                      List.fold_left
                        (fun (ra, st) (ru, _) ->
                          if ra == ru then (ru, ru) else (ru, st)
                        ) (-1, -1) cs
                    ) (List.map (List.sort cRank) css) |> List.map snd
      in let maxV = List.max ranks 
         in List.mapi (fun i x -> if x == maxV then i else -1) ranks |>
              List.filter (fun x -> x > -1) |> List.map (List.nth css) |> List.map (List.take 2)

    let rangeDupal = fun css ->
      let ranks = List.map (fun cs ->
                    List.fold_left
                      (fun (ra, st) (ru,_) ->
                        if ra = ru then (ru, ru :: st) else (ru, st)
                      ) (-1, []) cs
         ) (List.map (List.sort cRank) css) |> List.map snd |>
                    List.map (fun xs -> List.sort compare xs |> List.rev)
     in let maxVV = List.concat ranks |> List.max
        in let indecies = List.mapi (fun i xs ->
                              if 2 == List.length xs && maxVV == List.hd xs then i else -1) ranks
           in List.filter (fun x -> x > -1) indecies |> List.map (List.nth css) |>
                List.map (List.take 2) |> List.sort dRank |> List.rev |> List.hd
      
  end ;;


  (* test suite *)
let board = [(3,1);(7,3);(0,1);(5,1);(2,1)] ;;
let tu2 = Rangir.rangeDupal
            [
              [( 4, 1); ( 1, 1)] @ board ;
              [( 7, 2); ( 5, 2)] @ board ;
              [( 7, 2); ( 5, 2)] @ board ;
            ]
