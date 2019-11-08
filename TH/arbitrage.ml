open Bat ;; open Decisions ;; open Printf ;;

module Arbitrage =
struct

  type hand = (int * int) list 
  type cards = hand list

  let print_hand =
    fun xs css title ->
      printf "\n%i\t" title ;
      List.iter2 (
          fun x cs ->
             if x
             then (List.iter (fun (a,b) -> printf "%2i%2i   " a b) (Bat.take 2 cs) ; printf "\t")
             else ()) xs css
  ;;

  let treatRes =
    fun xs css title ->
    let numRes = List.filter (fun x -> x == true) xs |> List.length
    in if numRes == 1
       then print_hand xs css title
       else List.iter print_string ["\n" ; (string_of_int title) ; "\tmultiple results"]
  ;;
    
    
  let myWorkFun css predicat title continuation =
    let xs = List.map predicat css
    in if  List.exists (fun x -> x == true) xs
       then treatRes xs css title
       else continuation css 
  ;;
  
  let isAnyHaveHigh  = fun cs -> myWorkFun cs isHigh       900  (fun cs -> ()) ;;
  let isAnyHavePair  = fun cs -> myWorkFun cs isPair       800  isAnyHaveHigh  ;;
  let isAnyHaveDupal = fun cs -> myWorkFun cs isDupal      700  isAnyHavePair  ;;
  let isAnyHaveSet   = fun cs -> myWorkFun cs isSet        600  isAnyHaveDupal ;;
  let isAnyHaveStr   = fun cs -> myWorkFun cs isStraight   500  isAnyHaveSet   ;;
  let isAnyHaveFlush = fun cs -> myWorkFun cs isFlush      400  isAnyHaveStr   ;;
  let isAnyHaveFull  = fun cs -> myWorkFun cs isFull       300  isAnyHaveFlush ;;
  let isAnyHaveCaree = fun cs -> myWorkFun cs isCaree      200  isAnyHaveFull  ;;
  let isAnyHaveFlStr = fun cs -> myWorkFun cs isFlushStr8  100  isAnyHaveCaree ;;
    
  let start cs =
    let ts = List.hd cs |> Bat.drop 2  (* we took the board cards only                      *)
    in if isColor ts                   (* and chek if they have three in suit               *)
       then isAnyHaveFlStr cs          (* and in this case we start check from the begining *)
       else                            (* else we skip the first step in checking           *)
         if isDry ts                   (* and check if board have pair                      *)
         then isAnyHaveCaree cs        (* in which case we check caree and full             *)
         else isAnyHaveStr cs ;;       (* else we start from stright                        *)
         
end ;;                                    
  
