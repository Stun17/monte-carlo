open Shuffle ;; open Batteries ;; open Printf ;;

let count = fun () ->
  let deck = shuffle () 
  in let poket = List.take 2 deck
     and table = List.drop 2 deck |> List.take 5
     and is_in = fun (r, _) -> r = 12 
         in let in_hands = List.filter is_in poket |> List.length 
            and on_table = List.filter is_in table |> List.length
            in (in_hands, on_table)
;;

let funr = fun (x , y) (xa , ya) -> (x + xa , y + ya) ;;
  
  (1 -- 10000) |> Enum.map (fun _ -> count ()) |> Enum.filter (fun (x,_) -> x > 0)
  |> Enum.reduce funr |> fun (x,y) -> printf "%5.2f \n" ((float_of_int y) /. (float_of_int x))

                                           
  

