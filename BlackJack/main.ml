open Shuffle ;; open Batteries ;;

let prepare m cs =
     let pl1 = List.drop  2 cs |> List.take 2 
     and pl2 = List.drop  4 cs |> List.take 2  
     and pl3 = List.drop  6 cs |> List.take 2  
     and pl4 = List.drop  8 cs |> List.take 2  
     and pl5 = List.drop 10 cs |> List.take 2  
     and pl6 = List.drop 12 cs |> List.take 2  
     and pl7 = List.drop 14 cs |> List.take 2  
     in ( List.take m [pl1; pl2; pl3; pl4; pl5; pl6; pl7]
        , List.take 2 cs
        , List.drop (m * 2 + 2) cs
        )
;;

let m = int_of_string (Sys.argv.(1)) ;; (* num of players *)
let print_card = fun x -> print_int x, print_char ' ' ;;

(* let testIt = Shuffle.shuffle () |> List.sort compare |> List.map (fun x -> print_int x, print_char ' ') *)
  
Shuffle.shuffle () |> prepare m |> fun (players, dealer, deck) ->
     List.map print_card deck, print_newline () ,
     List.map (fun xs -> List.map print_card xs, print_newline ()) players ,
     List.map print_card dealer, print_newline () ;;
