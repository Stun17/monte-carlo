module Decisions =
  struct
    type cards = (int * int) list

    let isSmth k cs =
      let ranks = Array.make 13 0
      in let _ = List.map fst cs |>
                   List.iter (fun x -> Array.set ranks x ((Array.get ranks x) + 1) ; ())
         in Array.exists (fun x -> x == k) ranks 
    ;;

    let isPair = isSmth 2
    let isSet  = isSmth 3
    let isCare = isSmth 4
      
    let isFlush cs =
        List.map snd cs |>
        List.fold_left
            (fun (s,c,d,h) x ->
              match x with
              | 0 -> (s+1,c,d,h)
              | 1 -> (s,c+1,d,h)
              | 2 -> (s,c,d+1,h)
              | _ -> (s,c,d,h+1)
            ) (0,0,0,0) |>
       fun (s,c,d,h) -> s>4 || c>4 || d>4 || h>4 
    ;;
                   
    let isCare cs = true
    let isSet  cs = true

    let isFull cs = true
    let isStraight cs = true
end 
