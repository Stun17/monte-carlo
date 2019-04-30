open Shuffle ;; open Batteries ;; open Printf ;;

let r1 = int_of_string (Sys.argv.(1)) ;;
let r2 = int_of_string (Sys.argv.(2)) ;;
           
let mysort (x1,_) (x2,_) = if x1 > x2 then 1 else if x2 > x1 then -1 else 0 ;;

let myfilter (x, y) = (if x = r1 then y != 0 else true) && (if x = r2 then y != 1 else true)  
  
let func = fun _ -> 
  let cs = shuffle () |> List.filter myfilter 
  in let [ (p1,_) ; (p2,_) ] = List.take 2 cs |> List.sort mysort
     and [ (q1,_) ; (q2,_) ] = List.drop 2 cs |> List.take 2 |> List.sort mysort
     in
     if (r1 > p1 && r1 > q1 && r1 = r2) then true
     else
          (p1 != p2 && q1 != q2 && r1 > p1 && r1 > q1 && r2 >= p2 && r2 >= q2)
       || ((r1 = p1 && r2 > p2 && p1 > q1) || (q1 > p1 && r1 = q1 && r2 > q2))
       || (r2 > 8 && p1 < 10 && p2 < r2 && q1 < p1 && q2 < r2)
;;

(1 -- 10000) |> Enum.map func |> Enum.map (fun x -> if x then 1 else 0) |> Enum.reduce (+) |>
  fun x -> printf "you win preflop against 2 ops in %5.2f cases\n"
                  (0.8 *. (float_of_int x) /. 10000.0) 
;;
                                           
  

