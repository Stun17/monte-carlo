(* texas holdem poker game emulator
    input params : number of hands
                   num of gamers
    ouput        : win-poket(rank/suit)
 *)

open Shuffle ;; open Arbitrage ;; open Batteries ;;

let cardSort = fun (rank1, suit1) (rank2, suit2) -> if rank1 < rank2 then 1 else -1 ;;
  
let prepare numGamers deck =
  let board   =                      List.take 5 deck
  and player1 = List.drop  5 deck |> List.take 2 |> List.sort cardSort 
  and player2 = List.drop  7 deck |> List.take 2 |> List.sort cardSort 
  and player3 = List.drop  9 deck |> List.take 2 |> List.sort cardSort 
  and player4 = List.drop 11 deck |> List.take 2 |> List.sort cardSort 
  and player5 = List.drop 13 deck |> List.take 2 |> List.sort cardSort 
  and player6 = List.drop 15 deck |> List.take 2 |> List.sort cardSort 
  and player7 = List.drop 17 deck |> List.take 2 |> List.sort cardSort 
  and player8 = List.drop 19 deck |> List.take 2 |> List.sort cardSort 
  and player9 = List.drop 21 deck |> List.take 2 |> List.sort cardSort 
  and player0 = List.drop 23 deck |> List.take 2 |> List.sort cardSort 
  in ( List.take numGamers [ player1 @ board
                           ; player2 @ board
                           ; player3 @ board
                           ; player4 @ board
                           ; player5 @ board
                           ; player6 @ board
                           ; player7 @ board
                           ; player8 @ board
                           ; player9 @ board
                           ; player0 @ board
                           ]
        )
;;

let numOfHands  = int_of_string (Sys.argv.(1)) ;; (* number of hands   *)
let numOfGamers = int_of_string (Sys.argv.(2)) ;; (* number of players *)
  
(1 -- numOfHands) |> Enum.iter (fun _ -> shuffle () |> prepare numOfGamers |> start) ;;
