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

let mfind predicat cs =
  List.map predicat cs |> List.exists (fun x -> x == true) ;;

let mindex predicat cs =
  List.map predicat cs |> List.findi (fun i x -> x == true) |> fun (k,_) -> List.nth cs k ;;

let mprn rez name =
  printf "%-10s " name ;
  List.iter (fun (r, s) -> printf "%i-%i\t" r s) rez ;
  printf "\n"
;;

let myWorkFun cs predicat title nextFun =
  if mfind predicat cs
  then mprn (mindex predicat cs) title
  else nextFun cs
;;
  
let isSomeHaveHigh  = fun _  -> print_string            "high\n"                ;;
let isSomeHavePair  = fun cs -> myWorkFun cs isPair     "pair"  isSomeHaveHigh  ;;
let isSomeHaveDupal = fun cs -> myWorkFun cs isDupal    "dupal" isSomeHavePair  ;;
let isSomeHaveSet   = fun cs -> myWorkFun cs isSet      "set"   isSomeHaveDupal ;;
let isSomeHaveStr   = fun cs -> myWorkFun cs isStraight "str8"  isSomeHaveSet   ;;
let isSomeHaveFlush = fun cs -> myWorkFun cs isFlush    "flush" isSomeHaveStr   ;;
let isSomeHaveFull  = fun cs -> myWorkFun cs isFull     "full"  isSomeHaveStr   ;;
let isSomeHaveCare  = fun cs -> myWorkFun cs isCare     "care"  isSomeHaveFull  ;;

(1 -- 100) |>
    Enum.iter (fun _ ->
        shuffle () |>
          prepare |>
             fun (ps, bs) ->
             let cs = (List.map (fun c -> c @ bs) ps)
             in if isPair bs && not (isColored bs) then isSomeHaveCare cs else
                  if isColored bs then isSomeHaveFlush cs else isSomeHaveStr cs)
               
