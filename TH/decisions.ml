module Decisions =
  struct

    type cards = (int * int) list

    let isTimes k cs =
      let ranks = Array.make 13 0
      in let _ = List.map fst cs |>
                   List.iter (fun x -> Array.set ranks x ((Array.get ranks x) + 1) ; ())
         in Array.exists (fun x -> x == k) ranks
    ;;

    let isDry cs =
      let ranks = Array.make 13 0
      in let _ = List.map fst cs |>
                   List.iter (fun x -> Array.set ranks x ((Array.get ranks x) + 1) ; ())
         in Array.for_all (fun x -> x < 2) ranks
    ;;

    let isColored cs =
      let suits = Array.make 4 0
      in let _ = List.map snd cs |>
                   List.iter (fun x -> Array.set suits x ((Array.get suits x) + 1) ; ())
         in Array.exists (fun x -> x > 2) suits
    ;;

    let isCare cs = isTimes 4 cs
    let isPair cs = isTimes 2 cs
    let isSet  cs = isTimes 3 cs
    let isFull cs = isPair cs && isSet cs

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
