module Arbitrage =
struct
     
open Decisions ;; open Rangir ;;
open Batteries ;; open Printf ;;

type cards = (int * int) list ;;

let print_rezult rezult combi =
  printf "%-10s" combi ;
  let rezult2 = 
    match combi with  
    | "high"   -> rangeHigh   rezult
    | "pair"   -> rangePair   rezult
    | "dupal"  -> rangeDupal  rezult
    | "set"    -> rangeSet    rezult
    | "str8"   -> rangeStr    rezult
    | "flush"  -> rangeFlush  rezult
    | "full"   -> rangeFull   rezult
    | "caree"  -> rangeCaree  rezult
    | "fl-st"  -> rangeFlSt   rezult
  in List.iter ( fun p ->
                 List.iter ( fun (r, s) -> printf " %i %i " r s ) p
               )
               rezult2 ;
     printf "\n"
;;  
  
let myFoundOut predicat cs =
  let (_, rezult) = List.map predicat cs |> List.fold_left
        ( fun (count, acc) x ->
          if x then (count + 1, (List.nth cs count) :: acc)
          else      (count + 1,                        acc)
        ) (0, [])
  in rezult
;;
          
let myWorkFun m cs predicat title nextFun =
  let rezult = myFoundOut predicat cs
  in if List.is_empty rezult
     then nextFun m cs
     else print_rezult rezult title
;;

let isAnyHaveHigh  = fun m cs -> myWorkFun m cs isHigh      "high"  (fun m cs -> ())  ;;
let isAnyHavePair  = fun m cs -> myWorkFun m cs isPair      "pair"  isAnyHaveHigh     ;;
let isAnyHaveDupal = fun m cs -> myWorkFun m cs isDupal     "dupal" isAnyHavePair     ;;
let isAnyHaveSet   = fun m cs -> myWorkFun m cs isSet       "set"   isAnyHaveDupal    ;;
let isAnyHaveStr   = fun m cs -> myWorkFun m cs isStraight  "str8"  isAnyHaveSet      ;;
let isAnyHaveFlush = fun m cs -> myWorkFun m cs isFlush     "flush" isAnyHaveStr      ;;
let isAnyHaveFull  = fun m cs -> myWorkFun m cs isFull      "full"  isAnyHaveFlush    ;;
let isAnyHaveCare  = fun m cs -> myWorkFun m cs isCare      "caree" isAnyHaveFull     ;;
let isAnyHaveFlStr = fun m cs -> myWorkFun m cs isFluStr8   "fl-st" isAnyHaveFull     ;;
  
let arbitIt =
  fun num_of_players (ps, bs) ->           (* ps - player hand cards, bs - board cards *)
  let cs = List.map (fun p -> p @ bs) ps   (* cs - comulative cards *)
  in if isColored bs then isAnyHaveFlStr num_of_players cs else 
       if isPair bs then isAnyHaveCare num_of_players cs else
         isAnyHaveStr num_of_players cs
;;
  
end 
