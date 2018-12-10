type cards = (int * int) list ;;

open Shuffle ;;
open Decisions ;;
open Batteries ;;
open Printf ;;
  
let prepare cs = 
     let pl1 = List.drop  5 cs |> List.take 2 
     and pl2 = List.drop  7 cs |> List.take 2 
     and pl3 = List.drop  9 cs |> List.take 2 
     and pl4 = List.drop 11 cs |> List.take 2 
     and pl5 = List.drop 13 cs |> List.take 2 
     and pl6 = List.drop 15 cs |> List.take 2 
     and pl7 = List.drop 17 cs |> List.take 2 
     and pl8 = List.drop 19 cs |> List.take 2 
     and pl9 = List.drop 21 cs |> List.take 2 
     and plA = List.drop 23 cs |> List.take 2 
     in ([pl1; pl2; pl3; pl4; pl5; pl6; pl7; pl8; pl9; plA], List.take 5 cs)
;;

let myfinding predicate cs =
  List.map predicate cs |>
    List.exists (fun x -> x == true)
;;
  
let myindexing predicat cs =
  List.map predicat cs |>
  List.findi (fun i x -> x == true) |>
  fun (k,_) -> List.nth cs k
;;

let myprintin rez name =
  printf "%-10s " name ;
  List.iter (fun (r, s) -> printf "%i-%i\t" r s) rez ;
  printf "\n"
;;
  
let isSomeHaveHigh =
  fun _cs ->
  print_string "high\n"
;;

let isSomeHavePair =
  fun cs ->
  if myfinding isPair cs
  then myprintin (myindexing isPair cs) "pair"
  else isSomeHaveHigh cs
;;
  
let isSomeHaveDupal =
  fun cs ->
  if myfinding isDupal cs
  then myprintin (myindexing isDupal cs) "dupal"
  else isSomeHavePair cs
;;
  
let isSomeHaveSet =
  fun cs ->
  if myfinding isSet cs 
  then myprintin (myindexing isSet cs)"set"
  else isSomeHaveDupal cs
;;
  
let isSomeHaveStr =
  fun cs ->
  if myfinding isStraight cs
  then myprintin (myindexing isStraight cs) "str8"
  else isSomeHaveSet cs
;;
  
let isSomeHaveFlush =
  fun cs ->
  if myfinding isFlush cs
  then myprintin (myindexing isFlush cs) "flush"
  else isSomeHaveStr cs
;;
  
let isSomeHaveFull =
  fun cs ->
  if myfinding isFull cs 
  then myprintin (myindexing isFull cs) "full"
  else isSomeHaveStr cs
;;
  
let isSomeHaveCare =
  fun ps bs ->
  let cs = List.map (fun c -> c @ bs) ps
  in if isPair bs && not (isColored bs)
     then if myfinding isCare cs
          then myprintin (myindexing isCare cs) "care" 
          else isSomeHaveFull cs
     else if isColored bs
          then isSomeHaveFlush cs
          else isSomeHaveStr cs
;;

let analys (ps, bs) = isSomeHaveCare ps bs ;;
(1 -- 100) |> Enum.iter (fun _ -> shuffle () |> prepare |> analys) ;;
