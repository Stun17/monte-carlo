(* 
    texas holdem poker game pocket cards value statistics calculator
    it estimates how often the particular pocket win 

    input params :      number of hands
    ouput        :      plain text [table-size] [card1 card2 suit] [rate]
*)

open Shuffle ;;
open Treatment ;;
open Dealing ;;
open Arbitrage ;;
open Bat ;;

exception BadParams ;;

if 2 != Array.length Sys.argv
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

Treatment.init_hash () ;;               (* prepare hash-table for data *)
    
(1 -- numOfHands) |>                    (* to generate Stream of Nats from 1 to numOfHands *)
    Stream.iter
      ( fun _ ->
        (2 -- 10) |>                    (* to generate tables from HU to MX10 *)
          Stream.iter
            ( fun n ->
              Shuffle.shuffle () |> 
                Dealing.dealing n |>    (*  we pass lists of hands as list *)
                  Arbitrage.start
          )
      )
;;

Treatment.extract () ;;                 (* to put results out *)
