open Bat ;; open Decisions ;; open Printf ;;

module Arbitrage =
struct

  type hand = (int * int) list 
  type cards = hand list

  let print_hand =
    fun cs t ->
      printf "\n%i\t" t ;
      List.iter (fun (r, s) -> printf "%2i %2i\t" r s) (Bat.take 2 cs)
  ;;

  let evaluate_hand =
    fun cs title -> 
    match title with
    | 100 -> print_hand cs title
    | 200 -> print_hand cs title
    | 300 -> print_hand cs title
    | 400 -> print_hand cs title
    | 500 -> print_hand cs title
    | 600 -> print_hand cs title
    | 700 -> print_hand cs title
    | 800 -> print_hand cs title
    | 900 -> print_hand cs title
    | _   -> print_hand cs title
  ;;

  let myWorkFun css predicat title continuation =
    let xs = List.map predicat css
    in if (List.exists (fun x -> x == true) xs)
       then List.iter2 (fun x cs -> if x then evaluate_hand cs title else ()) xs css
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

  let start css =
    let ts = List.hd css |> Bat.drop 2 (* we took the board cards only                      *)
    in if isColor ts                   (* and chek if they have three in suit               *)
       then isAnyHaveFlStr css         (* and in this case we start check from the begining *)
       else                            (* else we skip the first step in checking           *)
         if isDry ts                   (* and check if board have pair                      *)
         then isAnyHaveCaree css       (* in which case we check caree and full             *)
         else isAnyHaveStr css ;;      (* else we start from stright                        *)

end ;;

