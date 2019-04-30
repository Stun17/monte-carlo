module Arbitrage =
struct
     
open Decisions ;; open Batteries ;; open Printf ;;

type hand = (int * int) list  
type cards = hand list ;;

let myWorkFun cs predicat title continuation =
  let r = List.map predicat cs
  in  if List.exists ( fun x -> x = true) r
      then List.combine r cs |> List.iter (fun (flag, xs) ->
                 if flag then
                   ( printf "%5s\t" title ;
                     List.iter (fun (x1, x2) -> printf "%2i %1i\t\t" x1 x2) (List.take 2 xs) ;
                     printf "\n"
                   )
                 else ()
               )
      else continuation cs
;;

let isAnyHaveHigh  = fun cs -> myWorkFun cs isHigh      "high"  (fun cs -> ())    ;;
let isAnyHavePair  = fun cs -> myWorkFun cs isPair      "pair"  isAnyHaveHigh     ;;
let isAnyHaveDupal = fun cs -> myWorkFun cs isDupal     "dupal" isAnyHavePair     ;;
let isAnyHaveSet   = fun cs -> myWorkFun cs isSet       "set"   isAnyHaveDupal    ;;
let isAnyHaveStr   = fun cs -> myWorkFun cs isStraight  "str8"  isAnyHaveSet      ;;
let isAnyHaveFlush = fun cs -> myWorkFun cs isFlush     "flush" isAnyHaveStr      ;;
let isAnyHaveFull  = fun cs -> myWorkFun cs isFull      "full"  isAnyHaveFlush    ;;
let isAnyHaveCare  = fun cs -> myWorkFun cs isCaree     "caree" isAnyHaveFull     ;;
let isAnyHaveFlStr = fun cs -> myWorkFun cs isFlushStr8 "fl-st" isAnyHaveFull     ;;
  
end 
