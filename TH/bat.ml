module Bat =
struct 

exception EmptyList ;;
exception NegIndex ;;
  
let rec take' (n : int) (acc : 'a list) (xs : 'a list) : 'a list =
  match n with 
  | 0 -> List.rev acc 
  | k -> if xs == []
         then raise EmptyList
         else take' (k - 1) ((List.hd xs) :: acc) (List.tl xs) 
  ;;

let take =
  fun n xs ->
    if n < 0
    then raise NegIndex
    else take' n [] xs ;;
  
let rec drop =
  fun n xs ->
    if n < 0
    then raise NegIndex
    else match n with
         | 0 -> xs
         | k -> if xs == []
                then raise EmptyList
                else drop (k - 1) (List.tl xs) 
  ;;
  
end ;;