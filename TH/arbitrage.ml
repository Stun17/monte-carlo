module Arbitrage =
struct
     
open Shuffle ;; open Decisions ;; open Batteries ;; open Printf ;; open Convert ;;

type cards = (int * int) list ;;

let myPrn rez name =
  printf "%-10s" name ;
  List.iter (fun zs ->
      List.iter (fun z -> printf "%s " (myConvi z)) zs ;
      printf "\t" ) rez ;
  printf "\n"
;;  
  
let myFind predicat cs =
  let (_, rez) =
    List.map predicat cs |>
      List.fold_left (
          fun (count, acc) c ->
          if c then (count + 1, (List.nth cs count) :: acc)
          else      (count + 1,                        acc)
        ) (0, [])
  in rez
;;
          
let rec myWorkFun m cs predicat title nextFun =
  let rez = myFind predicat cs
  in if List.is_empty rez then nextFun m cs else myPrn rez title
;;

let isAnyHaveHigh  = fun m cs -> myWorkFun m cs isHigh      "high"  (fun m cs -> ())  ;;
let isAnyHavePair  = fun m cs -> myWorkFun m cs isPair      "pair"  isAnyHaveHigh     ;;
let isAnyHaveDupal = fun m cs -> myWorkFun m cs isDupal     "dupal" isAnyHavePair     ;;
let isAnyHaveSet   = fun m cs -> myWorkFun m cs isSet       "set"   isAnyHaveDupal    ;;
let isAnyHaveStr   = fun m cs -> myWorkFun m cs isStraight  "str8"  isAnyHaveSet      ;;
let isAnyHaveFlush = fun m cs -> myWorkFun m cs isFlush     "flush" isAnyHaveStr      ;;
let isAnyHaveFull  = fun m cs -> myWorkFun m cs isFull      "full"  isAnyHaveFlush    ;;
let isAnyHaveCare  = fun m cs -> myWorkFun m cs isCare      "care"  isAnyHaveFull     ;;
let isAnyHaveFlSt  = fun m cs -> myWorkFun m cs isFluStr8   "fl-st" isAnyHaveFull     ;;
  
let arbitIt =
  fun m (ps, bs) ->
  let cs = List.map (fun p -> p @ bs) ps
  in if isColored bs then isAnyHaveFlSt m cs else 
       if isPair bs then isAnyHaveCare m cs else isAnyHaveStr m cs
;;
  
end 
