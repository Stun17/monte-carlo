module Arbitrage =
struct
     
open Shuffle ;; open Decisions ;; open Batteries ;; open Printf ;; open Convert ;;

type cards = (int * int) list ;;

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

let isAnyHaveHight = fun m cs -> myWorkFun m cs isHight     "high"  (fun m cs -> ())  ;;
let isAnyHavePair  = fun m cs -> myWorkFun m cs isPair      "pair"  isAnyHaveHight    ;;
let isAnyHaveDupal = fun m cs -> myWorkFun m cs isDupal     "dupal" isAnyHavePair     ;;
let isAnyHaveSet   = fun m cs -> myWorkFun m cs isSet       "set"   isAnyHaveDupal    ;;
let isAnyHaveStr   = fun m cs -> myWorkFun m cs isStraight  "str8"  isAnyHaveSet      ;;
let isAnyHaveFlush = fun m cs -> myWorkFun m cs isFlush     "flush" isAnyHaveStr      ;;
let isAnyHaveFlSt  = fun m cs -> myWorkFun m cs isFluStr8   "fl-st" isAnyHaveFlush    ;;  
let isAnyHaveFull  = fun m cs -> myWorkFun m cs isFull      "full"  isAnyHaveStr      ;;
let isAnyHaveCare  = fun m cs -> myWorkFun m cs isCare      "care"  isAnyHaveFull     ;;

let arbitIt =
  fun m (ps, bs) ->
  let cs = List.map (fun p -> p @ bs) ps
  in if isPair bs && not (isColored bs) then isAnyHaveCare m cs else
       if isColored bs then isAnyHaveFlSt m cs else isAnyHaveStr m cs
;;
  
end 
