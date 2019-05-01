module Arbitrage =
struct
     
open Decisions ;; open Batteries ;; open Printf ;;

type hand = (int * int) list  
type cards = hand list ;;

let print_hand xs title =
  printf "%s\t" title ;
  List.iter (fun (x,y) -> printf "%2i %i\t" x y) xs ;
  printf "\n"
;;

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
  List.split xs |> fst |> List.fold_left (fun a x -> if x != a then a else x) 0
;;
  
let arbitThem xs title =
  let one = List.take 7 xs 
  and two = List.drop 7 xs |> List.take 7 
  in match title with
     | "high"   ->
        (match compare (getHigh one)
                       (getHigh two)
         with
         | -1 -> print_hand two title | 1 -> print_hand one title 
         | 0 ->
            (match compare (List.tl one |> getHigh)
                           (List.tl two |> getHigh)
             with
             | -1 -> print_hand two title | 1 -> print_hand one title
             | 0 ->
                (match compare (List.drop 2 one |> getHigh)
                               (List.drop 2 two |> getHigh)
                 with
                 | -1 -> print_hand two title | 1 -> print_hand one title 
                 | 0  ->
                    (match compare (List.drop 3 one |> getHigh)
                                   (List.drop 3 two |> getHigh)
                     with
                     | -1 -> print_hand two title | 1 -> print_hand one title
                     | 0 ->
                        (match compare (List.drop 4 one |> getHigh)
                                       (List.drop 4 two |> getHigh)
                         with
                         | -1 -> print_hand two title | 1 -> print_hand one title
                         | 0 ->
                            (match compare (List.drop 5 one |> getHigh)
                                           (List.drop 5 two |> getHigh)
                             with
                             | -1 -> print_hand two title | 1 -> print_hand one title
                             | 0 ->
                                (match compare (List.drop 6 one |> getHigh)
                                               (List.drop 6 two |> getHigh)
                                 with
                                 | -1 -> print_hand two title | 1 -> print_hand one title
                                 | 0 -> print_hand one title ; print_hand two title
        )))))))
     | "pair" -> 
        (match compare (getPair one)
                       (getPair two)
         with
         | -1 -> print_hand two title | 1 -> print_hand one title 
         | 0 ->
            (match compare (getHigh one)
                           (getHigh two)
             with
             | -1 -> print_hand two title | 1 -> print_hand one title
             | 0 -> print_hand one title ; print_hand two title
        ))
     | "dupal"  ->
        (match compare (getDupal one)
                       (getDupal two)
         with
         | -1 -> print_hand two title | 1 -> print_hand one title
         | 0 ->
            (match compare (getHigh one)
                           (getHigh two)
             with
             | -1 -> print_hand two title | 1 -> print_hand one title
             | 0 -> print_hand one title ; print_hand two title
        ))
     | "set"    -> () 
     | "str8"   -> () 
     | "flush"  -> () 
     | "full"   -> () 
     | "caree"  -> ()   
     | "fl-st"  -> ()
     | _        -> ()
;;
  
  
let myWorkFun cs predicat title continuation =
  match (List.map predicat cs |> List.flatten) with
  | [] ->
     continuation cs
  | xs ->
     if (List.length xs = 7)
     then print_hand xs title
     else arbitThem xs title
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
  let ts = List.hd cs |> List.drop 2 
  in if isColor ts
     then isAnyHaveFlStr cs
     else
       if isDry ts
       then isAnyHaveCaree cs
       else isAnyHaveStr cs ;;
       
end ;;                                    


(* test suite  *)
let xs = [(12,1) ; (11,2) ; (11,0) ; (3,3) ; (3,2) ; (1,1) ] ;;
let teHigh = Arbitrage.getHigh  xs ;;
let tePair = Arbitrage.getPair xs ;;
let teDupal = Arbitrage.getDupal xs ;;
