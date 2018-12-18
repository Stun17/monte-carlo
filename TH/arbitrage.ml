module Arbitrage =
struct
     
open Decisions ;; open Rangir ;;
open Batteries ;; open Printf ;;

type cards = (int * int) list ;;

let myPrn rez name =
  printf "%-10s" name ;
  let rez2 = 
    match name with  
    | "high"   -> Rangir.rangeHigh   rez
    | "pair"   -> Rangir.rangePair   rez
    | "dupal"  -> Rangir.rangeDupal  rez
    | "set"    -> Rangir.rangeSet    rez
    | "str8"   -> Rangir.rangeStr    rez
    | "flush"  -> Rangir.rangeFlush  rez
    | "full"   -> Rangir.rangeFull   rez
    | "care"   -> Rangir.rangeCare   rez
    | "fl-st"  -> Rangir.rangeFuSt   rez
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

let isAnyHaveHigh  = fun m cs -> myWorkFun m cs Decisions.isHigh      "high"  (fun m cs -> ())  ;;
let isAnyHavePair  = fun m cs -> myWorkFun m cs Decisions.isPair      "pair"  isAnyHaveHigh     ;;
let isAnyHaveDupal = fun m cs -> myWorkFun m cs Decisions.isDupal     "dupal" isAnyHavePair     ;;
let isAnyHaveSet   = fun m cs -> myWorkFun m cs Decisions.isSet       "set"   isAnyHaveDupal    ;;
let isAnyHaveStr   = fun m cs -> myWorkFun m cs Decisions.isStraight  "str8"  isAnyHaveSet      ;;
let isAnyHaveFlush = fun m cs -> myWorkFun m cs Decisions.isFlush     "flush" isAnyHaveStr      ;;
let isAnyHaveFull  = fun m cs -> myWorkFun m cs Decisions.isFull      "full"  isAnyHaveFlush    ;;
let isAnyHaveCare  = fun m cs -> myWorkFun m cs Decisions.isCare      "care"  isAnyHaveFull     ;;
let isAnyHaveFlSt  = fun m cs -> myWorkFun m cs Decisions.isFluStr8   "fl-st" isAnyHaveFull     ;;
  
let arbitIt = fun num_of_players (ps, bs) ->
  let cs = List.map (fun p -> p @ bs) ps
  in if Decisions.isColored bs then isAnyHaveFlSt num_of_players cs else 
       if Decisions.isPair bs then isAnyHaveCare num_of_players cs else
         isAnyHaveStr num_of_players cs
;;
  
end 
