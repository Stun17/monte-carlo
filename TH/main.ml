open Shuffle ;; open Arbitrage ;;

let mySort = fun (r1, s1) (r2, s2) -> if r1 < r2 then 1 else -1 ;;
  
let prepare m cs =
     let pl1 = List.drop  5 cs |> List.take 2 |> List.sort mySort 
     and pl2 = List.drop  7 cs |> List.take 2 |> List.sort mySort 
     and pl3 = List.drop  9 cs |> List.take 2 |> List.sort mySort 
     and pl4 = List.drop 11 cs |> List.take 2 |> List.sort mySort 
     and pl5 = List.drop 13 cs |> List.take 2 |> List.sort mySort 
     and pl6 = List.drop 15 cs |> List.take 2 |> List.sort mySort 
     and pl7 = List.drop 17 cs |> List.take 2 |> List.sort mySort 
     and pl8 = List.drop 19 cs |> List.take 2 |> List.sort mySort 
     and pl9 = List.drop 21 cs |> List.take 2 |> List.sort mySort 
     and plA = List.drop 23 cs |> List.take 2 |> List.sort mySort 
     in (List.take m [pl1; pl2; pl3; pl4; pl5; pl6; pl7; pl8; pl9; plA] , List.take 5 cs)
;;

let n = int_of_string (Sys.argv.(1)) ;; (* num of hands   *)
let m = int_of_string (Sys.argv.(2)) ;; (* num of players *)
  
(1 -- n) |> Enum.iter (fun _ -> shuffle () |> prepare m |> arbitIt m) ;;
