open Shuffle ;; open Batteries ;; open Printf ;;

  
let (r1, r2)  = (read_int (), read_int ()) ;;
              
let func =
  fun () ->
  let [(op1, _) ; (op2, _)] = shuffle () |> List.take 2
  in r1 >= op1 && r2 >= op2 ;;

(1 -- 1000) |> Enum.map (fun _ -> func ())
|> Enum.filter (fun x -> x = true) |> Enum.map (fun x -> if x then 1 else 0)
|> Enum.reduce (+)
|> (* Int.print stdout *)
  fun x -> printf "you win opp in %5.2f cases\n" ((float_of_int x) /. 1000.0) 
;;
                                           
  

