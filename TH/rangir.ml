open Batteries ;;

module Rangir =
  struct
    
    type cards = int * int list

    let cRank = fun (r1,s1) (r2,s2) -> if r1 > r2 then 1 else -1 ;;
               
    let pRank =
      fun xs ->
      match xs with
      | [(r1,_); (r2,_)] -> if r1 >= r2 then xs else List.rev xs 
      | _ -> xs

           
    let rangeHigh =
      fun css ->
      let rez1 = List.map (fun cs -> List.take 2 cs) css |>
                   List.map pRank |> List.sort compare |> List.rev
      in let maxRank = List.max rez1 |> List.hd |> fst
         in let rez2 = List.take_while (fun [(r1, _) ; _] -> r1 == maxRank) rez1
            in List.fold_left ( fun (v, st) xs ->
                                match xs with
                                | [_ ; (r2, _)] ->
                                   if r2 > v then (r2, [xs]) else
                                     if r2 == v then (v, xs :: st) else (v, st)
                                | _ -> (v, st)
                              )
                              (0, [])
                              rez2
               |> snd 
            
    let rangePairs =
      fun css ->
      (* if List.length css == 1 then List.take 1 css |> List.take 2 *)
      (* else *)
        let rez1 = List.map (List.sort cRank) css
        in let rez2 = List.map (fun cs ->
               List.fold_left
                    (fun (ra, st) (ru, _) -> if ra == ru then (ru, ru) else (ru, st))
                    (-1, -1) cs
             ) rez1 |> List.map snd 
              in let maxV = List.max rez2
                 in let (k,_) = List.findi (fun i x -> x == maxV) rez2
                    in List.nth css k |> List.take 2

                                  (* in if List.length rez1 == 1 then rez1 else rez1 *)
                 
                       
  end ;;


  (* test suite *)
let board = [(3,1);(7,3);(0,1);(5,1);(2,1)] ;;
let tu2 = Rangir.rangePairs
            [
              [( 4, 1); ( 1, 1)]   @ board ;
              [( 5, 0); ( 6, 1)]   @ board ;
              [( 8, 3); ( 3, 3)]   @ board ;
            ]
