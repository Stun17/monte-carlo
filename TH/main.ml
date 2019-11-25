(* 
    texas holdem poker game pocket cards value statistics calculator
    it estimates how often the particular pocket win 

    input params :      number of hands
                        number of gamers
    ouput        :      win-poket(rank/suit)
*)

open Shuffle ;;
open Treatment ;;
open Dealing ;;
open Arbitrage ;;
open Bat ;;

exception BadParams ;;

if 3 != Array.length Sys.argv
then
  (
    print_endline "usage: a.out #numOfHands (> 0) #numOfPlayers (2 .. 10)" ;
    raise BadParams
  )
else () ;;
    
let numOfHands =
  int_of_string (Sys.argv.(1)) ;; (* get number of hands fm script command line params   *)

if numOfHands < 0 
then
  (
    print_endline "usage: a.out #numOfHands (> 0) #numOfPlayers (2 .. 10)" ;
    raise BadParams
  )
else () ;;

let numOfGamers = 
  int_of_string (Sys.argv.(2)) ;; (* get number of players fm script command line params *)
  
if numOfGamers < 2 || numOfGamers > 10  
then
  (
    print_endline "usage: a.out #numOfHands (> 0) #numOfPlayers (2 .. 10)" ;
    raise BadParams
  )
else () ;;

Treatment.inithash () ;;                      (* prepare hash-table for data *)
    
(1 -- numOfHands) |>                          (* to generate Stream of Nats from 1 to numOfHands *)
    Stream.iter
      ( fun _ ->
        Shuffle.shuffle () |> 
          Dealing.dealing numOfGamers |>      (*  we pass lists of hands as list *)
          Arbitrage.start
      )
;;

Treatment.extract () ;;                       (* output data on console *)
