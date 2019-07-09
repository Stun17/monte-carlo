open Shuffle ;; open List ;;

exception Empty ;;
exception Negative ;;
  
let rec take' (n : int) (acc : 'a list) (xs : 'a list) : 'a list =
  match n with 
  | 0 -> rev acc 
  | k -> if xs == []
         then raise Empty
         else take' (k - 1) ((hd xs) :: acc) (tl xs) 
;;

let take : int -> 'a list -> 'a list =
  fun n xs ->
    if n < 0
    then raise Negative
    else take' n [] xs ;;
  
let rec drop (n : int) (xs : 'a list) : 'a list =
  if n < 0
  then raise Negative
  else match n with
       | 0 -> xs
       | k -> if xs == []
              then raise Empty
              else drop (k - 1) (tl xs) 
;;
  
let prepare xs = 
  let deal  = take 2 xs  
  and pla1  = drop 2 xs |> take 2  
  and pla2  = drop 4 xs |> take 2  
  and pla3  = drop 6 xs |> take 2 
  and deck  = drop 8 xs
  in ( [pla1; pla2; pla3], deal, deck )
;;

let print_card (x : int) : unit * unit = print_int x , print_char ' ' ;;

Shuffle.shuffle () |> prepare |> fun (players, dealer, deck) ->
  print_newline () ,
  map print_card deck   , print_newline () ,
  map print_card dealer , print_newline () ,
  map (fun xs -> map print_card xs , print_newline ()) players ;;
