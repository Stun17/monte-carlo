(* texas holdem poker game emulator
    input params :      number of hands
                        number of gamers
    ouput        :      win-poket(rank/suit)
 *)

open Bat ;; open Shuffle ;; open Arbitrage ;; open Treatment ;;

let cardSort = fun (rank1, suit1) (rank2, suit2) -> if rank1 < rank2 then 1 else -1 ;;
  
let prepare numGamers deck =
  let board   =                     Bat.take 5 deck
  and player1 = Bat.drop  5 deck |> Bat.take 2 |> List.sort cardSort 
  and player2 = Bat.drop  7 deck |> Bat.take 2 |> List.sort cardSort 
  and player3 = Bat.drop  9 deck |> Bat.take 2 |> List.sort cardSort 
  and player4 = Bat.drop 11 deck |> Bat.take 2 |> List.sort cardSort 
  and player5 = Bat.drop 13 deck |> Bat.take 2 |> List.sort cardSort 
  and player6 = Bat.drop 15 deck |> Bat.take 2 |> List.sort cardSort 
  and player7 = Bat.drop 17 deck |> Bat.take 2 |> List.sort cardSort 
  and player8 = Bat.drop 19 deck |> Bat.take 2 |> List.sort cardSort 
  and player9 = Bat.drop 21 deck |> Bat.take 2 |> List.sort cardSort 
  and player0 = Bat.drop 23 deck |> Bat.take 2 |> List.sort cardSort 
  in
  List.iter (fun xs ->
      let [(r1, s1) ; (r2, s2)] = xs
      in Treatment.insert_deal (r1, s1, r2, s2))
                           [ player1 
                           ; player2 
                           ; player3 
                           ; player4 
                           ; player5 
                           ; player6 
                           ; player7 
                           ; player8 
                           ; player9 
                           ; player0 
                           ] |>
    fun _ ->  Bat.take numGamers   [ player1 @ board
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
;;

let numOfHands  = int_of_string (Sys.argv.(1)) ;; (* get number of hands fm command line params   *)
let numOfGamers = int_of_string (Sys.argv.(2)) ;; (* get number of players fm command line params *)

Treatment.inithash () ;;
    
let f = fun x -> if x < numOfHands then Some x else None ;; (* generate sequence on Nats *)

  (*  we pass to Arbitrage module sorted-by-rank lists of players hands    *)
Stream.from f |>
    Stream.iter
      ( fun _ ->
        Shuffle.shuffle ()  |> 
        prepare numOfGamers |>
        Arbitrage.start
      ) ;;

Treatment.extract () ;;
