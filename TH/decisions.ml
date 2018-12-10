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
      List.map snd cs |> List.fold_left
          (fun (s,c,d,h) x ->
            match x with
            | 0 -> (s+1,c,d,h) | 1 -> (s,c+1,d,h) | 2 -> (s,c,d+1,h) | _ -> (s,c,d,h+1)
          ) (0,0,0,0) |> fun (s,c,d,h) -> s>4 || c>4 || d>4 || h>4
    ;;

    let isStraight cs =
      let ranks = Array.make 13 0
      in let _ = List.map fst cs |>
                   List.iter (fun x -> Array.set ranks x ((Array.get ranks x) + 1) ; ())
         in let str2 = Array.sub ranks 0 5 |> Array.for_all (fun x -> x > 0)
            and str3 = Array.sub ranks 1 5 |> Array.for_all (fun x -> x > 0)
            and str4 = Array.sub ranks 2 5 |> Array.for_all (fun x -> x > 0)
            and str5 = Array.sub ranks 3 5 |> Array.for_all (fun x -> x > 0)
            and str6 = Array.sub ranks 4 5 |> Array.for_all (fun x -> x > 0)
            and str7 = Array.sub ranks 5 5 |> Array.for_all (fun x -> x > 0)
            and str8 = Array.sub ranks 6 5 |> Array.for_all (fun x -> x > 0)
            and str9 = Array.sub ranks 7 5 |> Array.for_all (fun x -> x > 0)
            and strT = Array.sub ranks 8 5 |> Array.for_all (fun x -> x > 0)
            and strW = Array.append
                         (Array.sub ranks 12 1)
                         (Array.sub ranks 0 4) |> Array.for_all (fun x -> x > 0) 
            in str2 || str3 || str4 || str5 || str6 || str7 || str8 || str9 || strT || strW
    ;;

    let isDupal cs =
      let ranks = Array.make 13 0
      in let _ = List.map fst cs |>
                   List.iter (fun x -> Array.set ranks x ((Array.get ranks x) + 1) ; ())
         and ds = Array.map (fun x -> if x == 2 then 1 else 0) ranks |>
                    Array.fold_left (fun acc x -> acc + x) 0
         in ds > 1
    ;;
end
