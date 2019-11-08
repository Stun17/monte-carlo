open Shuffle ;; open Printf ;;

let is_eq k (r , _) = r = k
let is_nl k (r , _) = r < k

let c0 = ref 0  
let c1 = ref 0 

let count = fun k ->
  let deck = shuffle () 
  in let poket = Bat.take 2 deck
     and board = Bat.drop 2 deck |> Bat.take 5  
     and opp_and_board = Bat.drop 2 deck |> Bat.take 11  
     and opp = Bat.drop 7 deck |> Bat.take 6  
     in 
       if List.exists (is_eq k) poket 
       then 
         if List.for_all (is_nl k) opp_and_board  
         then     (incr c0 ;                     incr c1) 
         else 
           if (List.exists (is_eq k) board) && (not (List.exists (is_eq k) opp))
           then 
             if List.for_all (is_eq k) poket  
             then (incr c0 ; incr c0 ; incr c0 ; incr c1) 
             else (incr c0 ; incr c0 ;           incr c1) 
           else    incr c0
       else ()
;;

let f = fun x -> if x < 3000 then Some x else None ;;

List.map 
  ( fun r -> 
      Stream.from f |> Stream.iter (fun _ -> count r) |> fun _ -> 
      printf "%2i in favor %5.2f\n" r (float_of_int (! c1) /. float_of_int (! c0)) |>
      fun _ -> c0 := 0 ; c1 := 0 
  )
  [0;1;2;3;4;5;6;7;8;9;10;11;12]
;;


;;
