(* texas holdem poker game statistics calculator
    input params :      number of hands
                        number of gamers
    ouput        :      win-poket(rank/suit)
 *)

open Shuffle ;;  open Treatment ;; open Dealing ;; open Arbitrage ;; open Bat ;;

let numOfHands =
  int_of_string (Sys.argv.(1)) ;; (* get number of hands fm command line params   *)

if numOfHands < 0 
then (print_endline "usage: a.out #numOfHands (> 0) #numOfPlayers (2 .. 10)" ; exit 1)
else () ;;

let numOfGamers = 
  int_of_string (Sys.argv.(2)) ;; (* get number of players fm command line params *)
  
if numOfGamers < 2 || numOfGamers > 10  
then (print_endline "usage: a.out #numOfHands (> 0) #numOfPlayers (2 .. 10)" ; exit 2 )
else () ;;

Treatment.inithash () ;; (* prepare hash-table for data *)
    
(1 -- numOfHands) |> (* to generate Stream of Nats from 1 to numOfHands *)
    Stream.iter
      ( fun _ ->
        Shuffle.shuffle ()  |> 
        Dealing.dealing numOfGamers |> (*  we pass lists of hands as list *)
        Arbitrage.start   
      )
;;

Treatment.extract () ;;   (* output data on console *)
