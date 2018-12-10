type cards = (int * int) list ;;

open Shuffle ;;
open Decisions ;;
open Batteries ;;
open Printf ;;

let convert (r, s) =
  let suit = match s with | 0 -> "s" | 1 -> "c" | 2 -> "d" | _ -> "h"
  and rank = match r with | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 -> string_of_int (r + 2)
                          | 8 -> "T" | 9 -> "J" | 10 -> "Q" | 11 -> "K" | _ -> "A"
  in rank ^ suit ;;

let mprn rez name =
  printf "%-10s" name ; List.iter (fun z -> printf "%s " (convert z)) rez ; printf "\n" ;;
  
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
  try
    List.map predicat cs |>
    List.findi (fun i x -> x == true) |>
    fun (k,_) -> (k, List.nth cs k)
  with _ -> (12, [])

let rec myWorkFun cs predicat title nextFun =
  let (k, rez1) = mfind predicat cs
  in if 0 < List.length rez1
     then (mprn rez1 title ; myWorkFun (List.drop (k + 1) cs) predicat title nextFun)
     else print_newline () (* nextFun cs *)
;;
  
let isSomeHaveHigh  = fun _  -> print_string            "high\n"                ;;
let isSomeHavePair  = fun cs -> myWorkFun cs isPair     "pair"  isSomeHaveHigh  ;;
let isSomeHaveDupal = fun cs -> myWorkFun cs isDupal    "dupal" isSomeHavePair  ;;
let isSomeHaveSet   = fun cs -> myWorkFun cs isSet      "set"   isSomeHaveDupal ;;
let isSomeHaveStr   = fun cs -> myWorkFun cs isStraight "str8"  isSomeHaveSet   ;;
let isSomeHaveFlush = fun cs -> myWorkFun cs isFlush    "flush" isSomeHaveStr   ;;
let isSomeHaveFull  = fun cs -> myWorkFun cs isFull     "full"  isSomeHaveStr   ;;
let isSomeHaveCare  = fun cs -> myWorkFun cs isCare     "care"  isSomeHaveFull  ;;

let n = int_of_string (Sys.argv.(1)) ;;
  
(1 -- n) |>
    Enum.iter (fun _ ->
        shuffle () |>
          prepare |>
             fun (ps, bs) ->
             let cs = (List.map (fun c -> c @ bs) ps)
             in if isPair bs && not (isColored bs) then isSomeHaveCare cs else
                  if isColored bs then isSomeHaveFlush cs else isSomeHaveStr cs)
