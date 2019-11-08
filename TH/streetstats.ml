open Shuffle ;; open Printf ;;

let m1 = int_of_string (Sys.argv.(1)) ;;
let m2 = int_of_string (Sys.argv.(2)) ;;
let n = int_of_string (Sys.argv.(3)) ;;
           
let mysort (x1,_) (x2,_) = if x1 >= x2 then -1 else if x2 > x1 then 1 else 0 ;;

  (* to exclude our own hand from dealing cards  *)
let hands (x, y) =
  if x = m1
  then y != 0
  else
    if x = m2
    then y != 1
    else true
;; 
 
let c = ref 0 ;;

let g =
  fun _ ->  
  let (r, _) =
    shuffle () |>
      List.filter hands |>
      Bat.take (2 * n + 5) |>
      List.sort mysort |> List.hd 
  in if r < m1 || r < m2 then incr c else ()
;;
    
let f = fun x -> if x < 2000 then Some x else None ;;
    
Stream.from f |> Stream.iter g ;;
      
printf "win High against %i opp(s) : %5.2f\n" n ((float_of_int (! c)) /. 2000.0) ;;
