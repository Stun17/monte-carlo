open Shuffle ;; open Batteries ;; open Printf ;;

let mysort (x1,_) (x2,_) = if x1 > x2 then 1 else if x2 > x1 then -1 else 0 ;;
           
let [(r1, s1) ; (r2, s2)] = [ (read_int (), read_int ()) ; (read_int (), read_int ()) ] ;;

let myfilter (r,s) (x, y) = x != r && y != s
  
let func = fun () -> 
  let [(p1, q1) ; (p2, q2)] =
    shuffle () |> List.filter (myfilter (r1,s1)) |> List.filter (myfilter (r2,s2)) |>
    List.take 2 |> List.sort mysort
  in if q1 != q2 then (r1 > p1 || (r1 = p1 && r2 > p2)) else
       if s1 = s2 then (r1 > p1 || (r1 = p1 && r2 > p2)) else (r1 > p1 + 1 && r2 > p2 + 1)
;;

(1 -- 1000) |> Enum.map (fun _ -> func ()) |> Enum.map (fun x -> if x then 1 else 0)
|> Enum.reduce (+) |> fun x -> printf "you win in %5.2f cases\n" ((float_of_int x) /. 1000.0) 
;;
                                           
  

