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

    let isDupal cs =
      let ranks = Array.make 13 0
      in let _ = List.map fst cs |>
                   List.iter (fun x -> Array.set ranks x ((Array.get ranks x) + 1) ; ())
         and ds = Array.map (fun x -> if x == 2 then 1 else 0) ranks |>
                    Array.fold_left (fun acc x -> acc + x) 0
         in ds > 1
    ;;

    let isHight _cs = true
                  
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
            (* wheel stright *)
            and strW = Array.append
                         (Array.sub ranks 12 1)
                         (Array.sub ranks 0 4) |> Array.for_all (fun x -> x > 0) 
            in str2 || str3 || str4 || str5 || str6 || str7 || str8 || str9 || strT || strW
    ;;

    let isFluStr8 cs =
      if isFlush cs && isStraight cs then
        let cs2 = List.sort compare cs
        in let ra = (List.hd cs2 |> fst) - 1
           and sa = List.hd cs2 |> snd
           in let (rez, _, _, _) = List.fold_left
            (fun (flag, count, ra, sa) (ru, su) ->
              if flag then (flag, 0, 0, 0)
              else if count >= 5 then (true, 0, 0, 0)
              else if sa == su && ra == 3 && ru == 12 && count == 4 then (true, 0, 0, 0)
              else if sa == su && (1 + ra) == ru then (flag, count + 1, ru, su)
              else if sa == su && (1 + ra) != ru then (false, 0, ru, su)              
              else if sa != su && (1 + ra) == ru then (flag, count, ra, sa)
              else if sa != su && (ra == 3) && count == 4 then (flag, count, ra, sa)
              else if sa != su && (1 + ra) != ru then (flag, 0, ra, sa)
              else (flag, ru, ru, su)
            ) (false, 0, ra, sa) cs2
        in rez
      else false
    ;;

end ;;

(* (\* test suite  *\) *)
  
(* print_newline () ;; *)
  
(*   (\* good one *\) *)
(* let te1 = Decisions.isFluStr8 [(1,1);(2,1);(0,1);(3,1);(4,1);(5,1);(9,1)] *)
(* let te2 = Decisions.isFluStr8 [(5,1);(4,1);(0,1);(2,1);(1,1);(3,1);(7,1)] *)
(* let te3 = Decisions.isFluStr8 [(7,1);(6,1);(5,0);(5,1);(4,1);(3,1);(2,1)] *)
(* let te4 = Decisions.isFluStr8 [(6,1);(5,1);(4,1);(3,1);(2,1);(7,1);(5,0)] *)
(* let teW = Decisions.isFluStr8 [(12,1);(1,1);(10,3);(5,0);(3,1);(2,1);(0,1)]  *)
(* let tu1 = Decisions.isFluStr8 [(12,3);(1,3);(4,3);(2,3);(7,1);(3,3);(0,3)] *)
(* let tu2 = Decisions.isFluStr8 [(4,1);(1,1);(3,1);(7,3);(7,1);(5,1);(2,1)] *)
(* let tu3 = Decisions.isFluStr8 [(3,0);(2,0);(12,0);(12,1);(6,3);(1,0);(0,0)] *)
(* let tu4 = Decisions.isFluStr8 [(10,0);(3,0);(8,2);(7,0);(5,0);(6,0);(4,0)] *)
(* let tu5 = Decisions.isFluStr8 [(4,1);(2,1);(1,1);(2,0);(0,1);(3,1);(8,2)] *)
(* let tu6 = Decisions.isFluStr8 [(3,2);(2,2);(4,2);(8,3);(10,0);(0,2);(1,2)] *)
(* let tu7 = Decisions.isFluStr8 [(3,0);(2,0);(5,0);(6,0);(8,2);(4,0);(12,0)] *)
(* let tu8 = Decisions.isFluStr8 [(7,3);(4,3);(5,3);(6,3);(8,3);(11,0);(3,3)] *)
(* let tu9 = Decisions.isFluStr8 [(7,3);(4,3);(12,0);(5,3);(8,3);(7,2);(6,3)] *)
(* let tua = Decisions.isFluStr8 [(1,2);(0,2);(4,2);(8,0);(3,2);(2,2);(11,3)] *)
(* let tub = Decisions.isFluStr8 [(4,1);(3,1);(1,1);(2,1);(5,1);(3,0);(5,3)] ;; *)
  
(* print_newline () ;; *)
  
(* (\*   (\\* bad one *\\) *\) *)
(* let tb1 = Decisions.isFluStr8 [(1,1);(2,1);(7,1);(3,1);(4,1);(6,1);(9,1)] *)
(* let tb2 = Decisions.isFluStr8 [(6,1);(4,1);(11,1);(2,1);(1,1);(3,1);(7,1)] *)
(* let tb3 = Decisions.isFluStr8 [(10,3);(6,4);(5,0);(7,4);(10,4);(3,4);(8,4)] *)
(* let tb4 = Decisions.isFluStr8 [(10,3);(7,1);(5,1);(1,1);(3,1);(11,1);(9,1)] *)
(* let tb5 = Decisions.isFluStr8 [(11,1);(1,1);(10,3);(5,1);(3,1);(2,1);(0,1)] *)
(* let tb6 = Decisions.isFluStr8 [(0,1);(1,1);(2,1);(3,1);(4,2);(5,1);(11,1)] *)
(* let tb7 = Decisions.isFluStr8 [(0,1);(4,1);(5,1);(6,1);(7,1);(8,2);(9,3)]  *)
(* let tb8 = Decisions.isFluStr8 [(3,0);(4,2);(5,3);(6,3);(7,3);(8,3);(11,3)]  *)
(* let tb9 = Decisions.isFluStr8 [(4,1);(7,1);(7,2);(8,1);(9,2);(10,1);(11,1)] ;;  *)

(* print_newline () ;; *)
