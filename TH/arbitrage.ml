open Bat ;; open Decisions ;; open Printf ;;

module Arbitrage =
struct

  type hand = (int * int) list 
  type cards = hand list

  let print_hand =
    fun xs css title ->
      printf "\n%-12s" title ;
      List.iter2 (
          fun x cs ->
             if x
             then (List.iter (fun (a,b) -> printf "%i,%i " a b) (Bat.take 2 cs) ; printf "\t")
             else ()) xs css
  ;;

  let treatRes =
    fun xs css title ->
    let numRes = List.filter (fun x -> x == true) xs |> List.length
    in if numRes == 1
       then print_hand xs css title
       else List.iter print_string ["\n" ; title ; "\t\tmultiple results"]
  ;;
    
    
  let myWorkFun css predicat title continuation =
    let xs = List.map predicat css
    in if  List.exists (fun x -> x == true) xs
       then treatRes xs css title
       else continuation css 
  ;;
  
  let isAnyHaveHigh  = fun cs -> myWorkFun cs isHigh      "high"  (fun cs -> ()) ;;
  let isAnyHavePair  = fun cs -> myWorkFun cs isPair      "pair"  isAnyHaveHigh  ;;
  let isAnyHaveDupal = fun cs -> myWorkFun cs isDupal     "dupal" isAnyHavePair  ;;
  let isAnyHaveSet   = fun cs -> myWorkFun cs isSet       "set"   isAnyHaveDupal ;;
  let isAnyHaveStr   = fun cs -> myWorkFun cs isStraight  "str8"  isAnyHaveSet   ;;
  let isAnyHaveFlush = fun cs -> myWorkFun cs isFlush     "flush" isAnyHaveStr   ;;
  let isAnyHaveFull  = fun cs -> myWorkFun cs isFull      "full"  isAnyHaveFlush ;;
  let isAnyHaveCaree = fun cs -> myWorkFun cs isCaree     "caree" isAnyHaveFull  ;;
  let isAnyHaveFlStr = fun cs -> myWorkFun cs isFlushStr8 "fl-st" isAnyHaveCaree ;;
    
  let start cs =
    let ts = List.hd cs |> Bat.drop 2  (* we took the board cards only                      *)
    in if isColor ts                   (* and chek if they have three in suit               *)
       then isAnyHaveFlStr cs          (* and in this case we start check from the begining *)
       else                            (* else we skip the first step in checking           *)
         if isDry ts                   (* and check if board have pair                      *)
         then isAnyHaveCaree cs        (* in which case we check caree and full             *)
         else isAnyHaveStr cs ;;       (* else we start from stright                        *)
         
end ;;                                    
  
