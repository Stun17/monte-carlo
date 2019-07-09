module Arbitrage =
struct
     
open Decisions ;; open Batteries ;; open Printf ;;

type hand = (int * int) list  
type cards = hand list ;;

(* just output procedure - nothing special here *)
let print_hand xs title =
  printf "%s\t" title ;
  List.iter (fun (x,y) -> printf "%2i %i\t" x y) xs ;
  printf "\n"
;;

(*  obtain the highest rank of the hand *)
let getHigh xs =
  List.sort compare xs |> List.rev |> List.split |> fst |> List.hd
;;
  
let getPair xs =
  List.sort compare xs |> List.rev |> List.split |> fst |>
    fun ys ->
    List.fold_right (
        fun x (a, b) ->
        if x = b then (x,x) else (a,x))
      (List.tl ys) (List.hd ys, 0) 
;;

let getDupal xs =
  List.sort compare xs |> List.rev |> List.split |> fst |> fun ys ->
    let ys1 = ys @ [0]
    and ys2 = 0 :: ys
    in List.map2 (fun s1 s2 -> if s1 = s2 then s1 else 0) ys1 ys2
         |> List.sort compare |> List.rev |> fun zs -> (List.hd zs, List.tl zs |> List.hd)
;;

let getSet xs =
  List.sort compare xs |> List.rev |> List.split |> fst |> fun ys ->
    let ys1 = ys @ [0]
    and ys2 = 0 :: ys
    in List.map2 (fun s1 s2 -> if s1 = s2 then s1 else 0) ys1 ys2 |>
         List.sort compare |> List.rev |> List.hd
;;

let getStr8 xs =
  List.sort compare xs |> List.rev |> List.split |> fst |> fun ys ->
    let ys1 = ys @ [0]
    and ys2 = 0 :: ys
    in List.map2 (fun s1 s2 -> if s2 - s1 = 1 then s2 else 0) ys1 ys2 |>
         List.sort compare |> List.rev |> List.hd  

let kicker xs ys title =
  let one = List.sort compare xs |> List.rev and two = List.sort compare ys |> List.rev in
  match compare (getHigh one) (getHigh two) with
  | -1 -> print_hand ys title | 1 -> print_hand xs title | 0 ->
   (match compare (List.tl one |> getHigh) (List.tl two |> getHigh) with
    | -1 -> print_hand ys title | 1 -> print_hand xs title | 0 ->
    (match compare (List.drop 2 one |> getHigh) (List.drop 2 two |> getHigh) with
     | -1 -> print_hand ys title | 1 -> print_hand xs title | 0  ->
     (match compare (List.drop 3 one |> getHigh) (List.drop 3 two |> getHigh) with
      | -1 -> print_hand ys title | 1 -> print_hand xs title | 0 ->
      (match compare (List.drop 4 one |> getHigh) (List.drop 4 two |> getHigh) with
       | -1 -> print_hand ys title | 1 -> print_hand xs title | 0 ->
       (match compare (List.drop 5 one |> getHigh) (List.drop 5 two |> getHigh) with
        | -1 -> print_hand ys title | 1 -> print_hand xs title | 0 ->
        (match compare (List.drop 6 one |> getHigh) (List.drop 6 two |> getHigh) with
         | -1 -> print_hand ys title | 1 -> print_hand xs title | 0 ->
            print_hand xs title ; print_hand ys title ))))))
;;

let arbitThem xs title =
  let one = List.take 7 xs and two = List.drop 7 xs |> List.take 7 in
  match title with
  | "high" ->
     kicker one two title
  | "pair" ->
     (match compare (getPair one) (getPair two) with
      | -1 -> print_hand two title | 1 -> print_hand one title 
      | 0 -> kicker one two title )
  | "dupal" ->
     (match compare (getDupal one) (getDupal two) with
      | -1 -> print_hand two title | 1 -> print_hand one title
      | 0 -> kicker one two title )
  | "set" ->
     (match compare (getSet one) (getSet two) with
      | -1 -> print_hand two title | 1 -> print_hand one title
      | 0 -> kicker one two title )           
  | "str8" ->
     (match compare (getStr8 one) (getStr8 two) with
      | -1 -> print_hand two title | 1 -> print_hand one title
      | 0 -> kicker one two title )              
  | "flush"  -> () 
  | "full"   -> () 
  | "caree"  -> ()   
  | "fl-st"  -> ()
  | _        -> ()
;;
  
let myWorkFun cs predicat title continuation =
  match (List.map predicat cs |> List.flatten) with
  | [] ->                       (* we dont have any coincidence on this step *)
     continuation cs            (* so we continue in next low level *)
  | xs ->                       (* we have some result and treat them  *)
     if (List.length xs = 7)    (* if we have single result - we print it *)
     then print_hand xs title
     else                       (* if we have multiple results we treat them differently *)
       if (List.length xs = 14)
       then arbitThem xs title
       else print_endline (title ^ " three or more")
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
  let ts = List.hd cs |> List.drop 2 (*  we took the board cards only  *)
  in if isColor ts                   (* and chek if they have three in suit *)
     then isAnyHaveFlStr cs          (* and in this case we start check from the begining *)
     else                            (* else we skip the first step in checking *)
       if isDry ts                   (* and check if board have set *)
       then isAnyHaveCaree cs        (* in which case we check caree and full *)
       else isAnyHaveStr cs ;;       (* else we start from stright  *)
       
end ;;                                    


(* ------------------------  test suite --------------------------  *)
  
open Arbitrage ;;
  
let xs = [(11,1) ; (9,2) ; (7,0) ; (5,3) ; (6,3) ; (4,1) ; (3,0) ] ;;
  
(* let teHigh = getHigh  xs ;; *)
(* let tePair = getPair xs ;; *)
(* let teDupal = getDupal xs ;; *)
(* let teSet = getSet xs *)
(* let teStr = getStr8 xs *)
