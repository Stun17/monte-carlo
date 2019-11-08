open Shuffle ;; open Printf ;;

let schet = ref 0 ;;

let count = fun k ->
  let deck = shuffle () 
  in let poket = Bat.take 2 deck
     and table = Bat.drop 2 deck |> Bat.take 5
     and is_in_poket   n = fun (r , _) -> r =  n
     and more_on_table n = fun (r , _) -> r >= n
         in let in_hands = List.filter (is_in_poket   k) poket |> List.length 
            and on_table = List.filter (more_on_table k) table |> List.length
            in if in_hands < on_table then incr schet else () 
;;


let rank = print_endline "input rank of the card in your pocket: " ; read_line ()
let f = fun x -> if x < 10000 then Some x else None ;;

let g = 
  match rank with
  | "A" -> 12 | "K" -> 11 | "Q" -> 10 | "J" -> 9 | "T" -> 8
  | x -> (int_of_string x) - 2
in Stream.from f |> Stream.iter (fun _ -> count g) |> 
         fun _ ->
         printf "with %s in your poket, board will over you in %5.2f cases\n"
                rank (float_of_int (! schet))
;;
