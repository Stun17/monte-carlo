open Batteries ;;

module Rangir =
  struct
    
    type cards = int * int list

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
      if List.length css == 1 then List.take 1 css
      else List.map (fun cs -> List.take 2 cs) css |> List.map pRank
                       
  end ;;


  (* test suite *)
let board = [(3,1);(7,3);(0,1);(5,1);(2,1)] ;;
let tu2 = Rangir.rangePairs
            [
              [( 4, 1); ( 1, 1)]   @ board ;
              [( 6, 1); ( 5, 0)]   @ board ;
              [( 2, 3); ( 3, 3)]   @ board ;
            ]
