module Bat =
struct 

    exception EmptyList ;;
    exception NegIndex ;;
    exception ShortList ;;

    let rec take' (n : int) (acc : 'a list) (xs : 'a list) : 'a list =
      match n with 
      | 0 -> List.rev acc 
      | k -> take' (k - 1) ((List.hd xs) :: acc) (List.tl xs) 
    ;;

    let take =
      fun n xs ->
        if n < 0 then raise NegIndex else 
          if n > List.length xs then raise ShortList else
            take' n [] xs 
    ;;

    let rec drop =
      fun n xs ->
        if n < 0 then raise NegIndex else 
          if n > List.length xs then raise ShortList else
            match n with
            | 0 -> xs
            | k -> drop (k - 1) (List.tl xs) 
    ;;

    let delete =
        fun x xs ->
          if 0 == List.length xs then raise EmptyList else
            List.fold_right (fun y acc -> if y = x then acc else y :: acc) xs []
    ;;

    let (--) =
        fun n m ->
          if m < n then raise NegIndex else
            Stream.from (fun i -> if (i + n) <= m then Some (i + n) else None)
    ;; 
        
end 
