(* texas holdem poker game emulator
    input params: number of hands
                  num of gamers
    ouput : win-poket(rank/suit)
 *)

open Shuffle ;; open Arbitrage ;; open Rangir ;; open Batteries ;;

let cardSort =
  fun (rank1, suit1) (rank2, suit2) ->
  if rank1 < rank2 then 1 else -1 ;;
  
let prepare numgamers deck =
     let player1 = List.drop  5 deck |> List.take 2 |> List.sort cardSort 
     and player2 = List.drop  7 deck |> List.take 2 |> List.sort cardSort 
     and player3 = List.drop  9 deck |> List.take 2 |> List.sort cardSort 
     and player4 = List.drop 11 deck |> List.take 2 |> List.sort cardSort 
     and player5 = List.drop 13 deck |> List.take 2 |> List.sort cardSort 
     and player6 = List.drop 15 deck |> List.take 2 |> List.sort cardSort 
     and player7 = List.drop 17 deck |> List.take 2 |> List.sort cardSort 
     and player8 = List.drop 19 deck |> List.take 2 |> List.sort cardSort 
     and player9 = List.drop 21 deck |> List.take 2 |> List.sort cardSort 
     and playerA = List.drop 23 deck |> List.take 2 |> List.sort cardSort 
     in ( List.take numgamers [player1
                              ; player2
                              ; player3
                              ; player4
                              ; player5
                              ; player6
                              ; player7
                              ; player8
                              ; player9
                              ; playerA
                              ]
        , List.take 5 deck
        )
;;

let nhands  = int_of_string (Sys.argv.(1)) ;; (* number of hands   *)
let ngamers = int_of_string (Sys.argv.(2)) ;; (* number of players *)
  
(1 -- nhands) |> Enum.iter (fun _ -> shuffle () |> prepare ngamers |> arbitIt ngamers) ;;
