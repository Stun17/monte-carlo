open Bat ;; open Decisions ;; open Evaluations ;; open Printf ;; open Treatment ;;

module Arbitrage =
struct

  type hand = (int * int) list
  type cards = hand list

  let evaluate_hand =
    fun cs combi ->
    match combi with
    | 8 -> Evaluations.priceFlushStr8 cs
    | 7 -> Evaluations.priceCaree     cs
    | 6 -> Evaluations.priceFull      cs
    | 5 -> Evaluations.priceFlush     cs
    | 4 -> Evaluations.priceStr8      cs
    | 3 -> Evaluations.priceSet       cs
    | 2 -> Evaluations.priceDupal     cs
    | 1 -> Evaluations.pricePair      cs
    | 0 -> Evaluations.priceHigh      cs
    | _ -> 0
  ;;

  let rwinners =
    fun n ws ->
    let (w1, _, _) = List.hd ws
    in let winlist = List.filter (fun (t, _, _) -> t = w1) ws
       in let winprice = n / (List.length winlist)
          in List.iter (fun (_, (r1, s1), (r2, s2)) ->
             Treatment.insert_win (r1, s1, r2, s2, winprice)
           ) winlist
  ;;

  let work css predicat combi continuation =
    let xs = List.map predicat css
    in if (List.exists (fun x -> x == true) xs)
       then List.map2 (
                fun x cs ->
                let c1 = List.hd cs
                and c2 = List.hd (List.tl cs)
                in if x
                   then (evaluate_hand cs combi, c1, c2)
                   else (0                     , c1, c2)
              ) xs css |> List.sort compare |> List.rev |> rwinners (List.length css)
       else continuation css
  ;;

  let tryWithHigh  = fun css -> work css isHigh       0  (fun cs -> ()) ;;
  let tryWithPair  = fun css -> work css isPair       1  tryWithHigh  ;;
  let tryWithDupal = fun css -> work css isDupal      2  tryWithPair  ;;
  let tryWithSet   = fun css -> work css isSet        3  tryWithDupal ;;
  let tryWithStr   = fun css -> work css isStraight   4  tryWithSet   ;;
  let tryWithFlush = fun css -> work css isFlush      5  tryWithStr   ;;
  let tryWithFull  = fun css -> work css isFull       6  tryWithFlush ;;
  let tryWithCaree = fun css -> work css isCaree      7  tryWithFull  ;;
  let tryWithFlStr = fun css -> work css isFlushStr8  8  tryWithCaree ;;

  (*  css is the list of lists and each list is poket @ board *)
  let start css =
    let ts = List.hd css |> Bat.drop 2 (* we took the board cards only                      *)
    in if   isColor ts                 (* and chek if they have three-in-suit               *)
       then tryWithFlStr css           (* and in this case we start check from the begining *)
       else                            (* else we skip the first step in checking           *)
         if   isWet ts                 (* and check if board have pair                      *)
         then tryWithCaree css         (* in which case we check Caree and Full and so on   *)
         else tryWithStr css ;;        (* else we start from Stright                        *)

end ;;
