module Arbitrage =
struct
     
type cards = (int * int) list ;;

open Shuffle ;; open Decisions ;; open Batteries ;; open Printf ;; open Convert ;;

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
          
let rec myWorkFun m cs predicat title nextFun =
  let (k, rez1) = myFind predicat cs
  in if 0 < List.length rez1
     then (myPrn rez1 title ; myWorkFun m (List.drop (k + 1) cs) predicat title nextFun)
     else if (m == List.length cs) then nextFun m cs else print_newline () 
;;

let isSomeHaveHight = fun m cs -> myWorkFun m cs isHight     "high"  (fun m cs -> ());;
let isSomeHavePair  = fun m cs -> myWorkFun m cs isPair      "pair"  isSomeHaveHight ;;
let isSomeHaveDupal = fun m cs -> myWorkFun m cs isDupal     "dupal" isSomeHavePair  ;;
let isSomeHaveSet   = fun m cs -> myWorkFun m cs isSet       "set"   isSomeHaveDupal ;;
let isSomeHaveStr   = fun m cs -> myWorkFun m cs isStraight  "str8"  isSomeHaveSet   ;;
let isSomeHaveFlush = fun m cs -> myWorkFun m cs isFlush     "flush" isSomeHaveStr   ;;
let isSomeHaveFlSt  = fun m cs -> myWorkFun m cs isFlushStr8 "fl-st" isSomeHaveFlush ;;  
let isSomeHaveFull  = fun m cs -> myWorkFun m cs isFull      "full"  isSomeHaveFlSt  ;;
let isSomeHaveCare  = fun m cs -> myWorkFun m cs isCare      "care"  isSomeHaveFull  ;;

let arbitIt =
  fun m (ps, bs) ->
  let cs = List.map (fun c -> c @ bs) ps
  in if isPair bs && not (isColored bs) then isSomeHaveCare m cs else
       if isColored bs then isSomeHaveFlSt m cs else isSomeHaveStr m cs
;;
  
end 
