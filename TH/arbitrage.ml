module Arbitrage =
struct
    
type cards = (int * int) list ;;

open Shuffle ;;
open Decisions ;;
open Batteries ;;
open Printf ;;
open Convert ;;

let myPrn rez name =
  printf "%-10s" name ;
  List.iter (fun z -> printf "%s " (myConvi z)) rez ;
  printf "\n"
;;  
  
let myFind predicat cs =
  try
    List.map predicat cs |>
    List.findi (fun i x -> x == true) |>
    fun (k,_) -> (k, List.nth cs k)
  with _ -> (12, [])
          
let rec myWorkFun cs predicat title nextFun =
  let (k, rez1) = myFind predicat cs
  in if 0 < List.length rez1
     then (myPrn rez1 title ; myWorkFun (List.drop (k + 1) cs) predicat title nextFun)
     else if (10 == List.length cs) then nextFun cs else print_newline () 
;;

let isSomeHaveHight = fun cs -> myWorkFun cs isHight     "high"  (fun cs -> ())  ;;
let isSomeHavePair  = fun cs -> myWorkFun cs isPair      "pair"  isSomeHaveHight ;;
let isSomeHaveDupal = fun cs -> myWorkFun cs isDupal     "dupal" isSomeHavePair  ;;
let isSomeHaveSet   = fun cs -> myWorkFun cs isSet       "set"   isSomeHaveDupal ;;
let isSomeHaveStr   = fun cs -> myWorkFun cs isStraight  "str8"  isSomeHaveSet   ;;
let isSomeHaveFlush = fun cs -> myWorkFun cs isFlush     "flush" isSomeHaveStr   ;;
let isSomeHaveFlSt  = fun cs -> myWorkFun cs isFlushStr8 "fl-st" isSomeHaveFlush ;;  
let isSomeHaveFull  = fun cs -> myWorkFun cs isFull      "full"  isSomeHaveFlSt  ;;
let isSomeHaveCare  = fun cs -> myWorkFun cs isCare      "care"  isSomeHaveFull  ;;

let arbitit =
  fun (ps, bs) ->
  let cs = List.map (fun c -> c @ bs) ps
  in if isPair bs && not (isColored bs) then isSomeHaveCare cs else
       if isColored bs then isSomeHaveFlSt cs else isSomeHaveStr cs
;;
  
end 
