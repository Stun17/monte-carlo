open Shuffle ;; open Batteries ;; open Printf ;;

let mysort (x1,_) (x2,_) = if x1 > x2 then 1 else if x2 > x1 then -1 else 0 ;;
           
let [(r1, s1) ; (r2, s2)] = [ (read_int (), read_int ()) ; (read_int (), read_int ()) ] ;;

let myfilter (r, s) (x, y) = x != r && y != s
  
let func = fun () -> 
  let [(p3, q3) ; (p4, q4)] =
    shuffle () |> List.filter (myfilter (r1,s1)) |> List.filter (myfilter (r2,s2)) |>
    List.take 2 |> List.sort mysort
  in if (r1 > p3 && r1 = r2)
     then true
     else
       if (q3 != q4 && s1 != s2) || (q3 = q4 && s1 = s2)
       then (r1 > p3 && p3 != p4) 
       else
         if s1 = s2 
         then r1 >= p3
         else r1 > p3 + 1
;;

(1 -- 10000) |> Enum.map (fun _ -> func ()) |> Enum.map (fun x -> if x then 1 else 0)
|> Enum.reduce (+) |> fun x -> printf "you win in %5.2f cases\n" ((float_of_int x) /. 10000.0) 
;;
                                           
  

