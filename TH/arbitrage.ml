open Bat ;; open Decisions ;; open Evaluations ;; open Printf ;;

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
    | 100 -> Evaluations.getFlushStr8 cs 
    | 200 -> Evaluations.getCaree     cs
    | 300 -> Evaluations.getFull      cs 
    | 400 -> Evaluations.getFlush     cs 
    | 500 -> Evaluations.getStr8      cs 
    | 600 -> Evaluations.getSet       cs 
    | 700 -> Evaluations.getDupal     cs 
    | 800 -> Evaluations.getPair      cs 
    | 900 -> Evaluations.getHigh      cs 
    | _   -> 0 
  ;;

  let myWorkFun css predicat title continuation =
    let xs = List.map predicat css
    in if (List.exists (fun x -> x == true) xs)
       then List.map2 (
                fun x cs ->
                if x
                then evaluate_hand cs title 
                else 0) xs css |>
              List.filter (fun x -> x > 0) |>
              List.sort compare |> List.rev |> List.hd |> fun x -> printf "%3i\n" x 
       else continuation css
  ;;

  let isAnyHaveHigh  = fun css -> myWorkFun css isHigh       900  (fun cs -> ()) ;;
  let isAnyHavePair  = fun css -> myWorkFun css isPair       800  isAnyHaveHigh  ;;
  let isAnyHaveDupal = fun css -> myWorkFun css isDupal      700  isAnyHavePair  ;;
  let isAnyHaveSet   = fun css -> myWorkFun css isSet        600  isAnyHaveDupal ;;
  let isAnyHaveStr   = fun css -> myWorkFun css isStraight   500  isAnyHaveSet   ;;
  let isAnyHaveFlush = fun css -> myWorkFun css isFlush      400  isAnyHaveStr   ;;
  let isAnyHaveFull  = fun css -> myWorkFun css isFull       300  isAnyHaveFlush ;;
  let isAnyHaveCaree = fun css -> myWorkFun css isCaree      200  isAnyHaveFull  ;;
  let isAnyHaveFlStr = fun css -> myWorkFun css isFlushStr8  100  isAnyHaveCaree ;;

  (*  css is the list of lists and each list is poket @ board *)
  let start css =
    let ts = List.hd css |> Bat.drop 2 (* we took the board cards only                      *)
    in if isColor ts                   (* and chek if they have three in suit               *)
       then isAnyHaveFlStr css         (* and in this case we start check from the begining *)
       else                            (* else we skip the first step in checking           *)
         if isDry ts                   (* and check if board have pair                      *)
         then isAnyHaveCaree css       (* in which case we check caree and full             *)
         else isAnyHaveStr css ;;      (* else we start from stright                        *)

end ;;

    (* ----------------------- maybe these funcs for Arbitrage Module ------------------ *)
      
    let rec kicker xs ys title =
      let one = List.sort compare xs |> List.rev 
      and two = List.sort compare ys |> List.rev 
      in
        match compare (getHigh one) (getHigh two) with
        | -1 -> ()
        |  1 -> ()
        |  _ -> kicker (List.tl one) (List.tl two) title
    ;;

   let arbitThem xs title =
     let one = Bat.take 7 xs
     and two = Bat.drop 7 xs |> Bat.take 7
     in
     match title with
     | 900 -> kicker one two title
     | 800 -> (match compare (getPair one) (getPair two) with
               | -1 -> ()
               |  1 -> ()
               |  _ -> kicker one two title )
     | 700 -> (match compare (getDupal one) (getDupal two) with
               | -1 -> ()
               |  1 -> ()
               |  _ -> kicker one two title )
     | 600 -> (match compare (getSet one) (getSet two) with
               | -1 -> ()
               |  1 -> ()
               |  _ -> kicker one two title )           
     | 500 -> (match compare (getStr8 one) (getStr8 two) with
               | -1 -> ()
               |  1 -> ()
               |  _ -> kicker one two title )              
     | 400  -> (match compare (getFlush one) (getFlush two) with
                | -1 -> ()
                |  1 -> ()
                |  _ -> kicker one two title ) 
     | 300  -> (match compare (getFull one) (getFull two) with
                | -1 -> ()
                |  1 -> ()
                |  _ -> kicker one two title ) 
     | 200  -> (match compare (getCaree one) (getCaree two) with
                | -1 -> ()
                |  1 -> ()
                |  _ -> kicker one two title ) 
     | 100  -> (match compare (getFlushStr8 one) (getFlushStr8 two) with
                | -1 -> ()
                |  1 -> ()
                |  _ -> kicker one two title ) 
     | _    -> ()
   ;;
