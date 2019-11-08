open Shuffle ;; open Printf ;;

let is_eq k (r , _) = r == k
let is_nl k (r , _) = r <= k

let c0 = ref 0  
let c1 = ref 0 

let count = fun k ->
  let deck = shuffle () 
  in let poket = Bat.take 2 deck
     and opp_and_board = Bat.drop 2 deck |> Bat.take 7  
     in 
       if List.exists (is_eq k) poket 
       then 
         if List.for_all (is_nl k) opp_and_board  
         then  (incr c0 ; incr c1) 
         else incr c0 
       else ()
;;

let r = print_endline "rank 0-12 : " ; read_line () ;;

let f = fun x -> if x < 10000 then Some x else None ;;

Stream.from f |> Stream.iter (fun _ -> count (int_of_string r)) ;;

let p0 = float_of_int (! c0)
and p1 = float_of_int (! c1)
in printf "in favor %5.2f\n" (p1 /. p0) 
;;
