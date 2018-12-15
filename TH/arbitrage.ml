module Arbitrage =
struct
     
open Shuffle ;; open Decisions ;; open Convert ;; open Rangir ;;
open Batteries ;; open Printf ;;

type cards = (int * int) list ;;

let myPrn rez name =
  printf "%-10s" name ;
  let rez2 = (*    (int * int) list list      *)
    match name with  
    | "high"   -> rangeHigh   rez
    | "pair"   -> rangePair   rez
    | "dupal"  -> rangeDupal  rez
    | "set"    -> rangeSet    rez
    | "str8"   -> rangeStr    rez
    | "flush"  -> rangeFlush  rez
    | "full"   -> rangeFull   rez
    | "care"   -> rangeCare   rez
    | "fl-st"  -> rangeFuSt   rez
 in List.iter (fun p -> List.iter (fun (r, s) -> printf " %i %i " r s) p) rez2 ;
  printf "\n"
;;  
  
let myFoundOut predicat cs =
  let (_, rezultList) =
    List.map predicat cs |> List.fold_left
        ( fun (count, acc) c ->
          if c then (count + 1, (List.nth cs count) :: acc)
          else      (count + 1,                        acc)
        ) (0, [])
  in rezultList
;;
          
let rec myWorkFun m cs predicat title nextFun =
  let rez = myFoundOut predicat cs
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
