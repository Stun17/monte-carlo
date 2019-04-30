module Arbitrage =
struct
     
open Decisions ;; open Batteries ;; open Printf ;;

type hand = (int * int) list  
type cards = hand list ;;

let myWorkFun cs predicat title continuation =
  match (List.map predicat cs |> List.flatten) with
  | [] ->
     continuation cs
  | xs ->
     printf "%s\t" title ;
     if (List.length xs = 7)
     then (List.iter (fun (x,y) -> printf "%2i %i\t" x y) xs ; printf "\n")
     else print_endline "several players" 
;;

let isAnyHaveHigh  = fun cs -> myWorkFun cs isHigh      "high"  (fun cs -> ())    ;;
let isAnyHavePair  = fun cs -> myWorkFun cs isPair      "pair"  isAnyHaveHigh     ;;
let isAnyHaveDupal = fun cs -> myWorkFun cs isDupal     "dupal" isAnyHavePair     ;;
let isAnyHaveSet   = fun cs -> myWorkFun cs isSet       "set"   isAnyHaveDupal    ;;
let isAnyHaveStr   = fun cs -> myWorkFun cs isStraight  "str8"  isAnyHaveSet      ;;
let isAnyHaveFlush = fun cs -> myWorkFun cs isFlush     "flush" isAnyHaveStr      ;;
let isAnyHaveFull  = fun cs -> myWorkFun cs isFull      "full"  isAnyHaveFlush    ;;
let isAnyHaveCaree = fun cs -> myWorkFun cs isCaree     "caree" isAnyHaveFull     ;;
let isAnyHaveFlStr = fun cs -> myWorkFun cs isFlushStr8 "fl-st" isAnyHaveCaree    ;;
  
end ;;
