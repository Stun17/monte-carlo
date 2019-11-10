(* texas holdem poker game emulator
    input params :      number of hands
                        number of gamers
    ouput        :      win-poket(rank/suit)
 *)

open Shuffle ;;  open Treatment ;; open Dealing ;; open Arbitrage ;;

let numOfHands =
  int_of_string (Sys.argv.(1)) ;; (* get number of hands fm command line params   *)

if numOfHands < 0 
then  print_endline "usage: a.out #numOgGamers (> 0) #numOfHands (2 .. 10)"
else () ;;

let numOfGamers = 
  int_of_string (Sys.argv.(2)) ;; (* get number of players fm command line params *)
  
if numOfGamers < 2 || numOfGamers > 10  
then  print_endline "usage: a.out #numOgGamers #numOfHands (2-10)"
else () ;;

Treatment.inithash () ;; (* prepare hash-table for data *)
    
let f = fun x ->  (* to generate sequence of Nats from 1 to numOfHands *)
  if x < numOfHands
  then Some x
  else None
;; 

  (*  we pass to Arbitrage module sorted-by-rank lists of players hands    *)
Stream.from f |>
    Stream.iter
      ( fun _ ->
        Shuffle.shuffle ()  |> 
        Dealing.dealing numOfGamers |>
        Arbitrage.start
      )
;;

Treatment.extract () ;;   (* output data on console *)
