open Shuffle ;; open Batteries ;; open Printf ;;

let count = fun k ->
  let deck = shuffle () 
  in let poket = List.take 2 deck
     and table = List.drop 2 deck |> List.take 5
     and is_in_poket   n = fun (r , _) -> r =  n
     and more_on_table n = fun (r , _) -> r >= n
         in let in_hands = List.filter (is_in_poket   k) poket |> List.length 
            and on_table = List.filter (more_on_table k) table |> List.length
            in (in_hands, on_table)
;;

let rank = print_endline "input rank of the card in your pocket: " ; read_line ()
and nums = print_endline "input num of trials: " ;                   read_line () 
in         print_endline "... wait ... " ;
    let funreduce = fun (x , y) (xa , ya) -> (x + xa , y + ya) 
    and r = match rank with
             | "A" -> 12 | "K" -> 11 | "Q" -> 10 | "J" -> 9 | "T" -> 8
             | x -> (int_of_string x) - 2
    in (1 -- (int_of_string nums)) |> Enum.map (fun _ -> count r)
       |> Enum.filter (fun (x, _) -> x > 0) |> Enum.reduce funreduce |>
         fun (x, y) ->
         printf "with %s in your poket, board will over you in %5.2f cases\n"
                rank ((float_of_int y) /. (float_of_int x))
;;
                                           
  

