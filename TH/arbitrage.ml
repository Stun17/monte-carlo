open Bat ;; open Decisions ;; open Evaluations ;; open Printf ;;

module Arbitrage =
struct

  type hand = (int * int) list 
  type cards = hand list

  let evaluate_hand =
    fun cs combi -> 
    match combi with
    | 8000 -> Evaluations.priceFlushStr8 cs 
    | 7000 -> Evaluations.priceCaree     cs
    | 6000 -> Evaluations.priceFull      cs 
    | 5000 -> Evaluations.priceFlush     cs 
    | 4000 -> Evaluations.priceStr8      cs 
    | 3000 -> Evaluations.priceSet       cs 
    | 2000 -> Evaluations.priceDupal     cs 
    | 1000 -> Evaluations.pricePair      cs 
    |    0 -> Evaluations.priceHigh      cs 
    | _   -> 0 
  ;;

  let print_poket =
    fun (_, (r1, s1), (r2, s2)) ->
    printf "%2i %2i %2i %2i\n" r1 s1 r2 s2
  ;;

  let take_winners =
    fun ws ->
    let (w1, _, _) = List.hd ws
    in List.filter (fun (t, _, _) -> t = w1) ws
  ;;
    
  let work css predicat combi continuation =
    let xs = List.map predicat css
    in if (List.exists (fun x -> x == true) xs)
       then List.map2 (
                fun x cs ->
                let c1 = List.hd cs
                and c2 = List.hd (List.tl cs)
                in if x
                   then (combi + (evaluate_hand cs combi), c1, c2)
                   else (0                               , c1, c2)
              ) xs css |> List.sort compare |> List.rev |>
              take_winners |> List.iter print_poket 
       else continuation css
  ;;

  let tryWithHigh  = fun css -> work css isHigh          0  (fun cs -> ()) ;;
  let tryWithPair  = fun css -> work css isPair       1000  tryWithHigh  ;;
  let tryWithDupal = fun css -> work css isDupal      2000  tryWithPair  ;;
  let tryWithSet   = fun css -> work css isSet        3000  tryWithDupal ;;
  let tryWithStr   = fun css -> work css isStraight   4000  tryWithSet   ;;
  let tryWithFlush = fun css -> work css isFlush      5000  tryWithStr   ;;
  let tryWithFull  = fun css -> work css isFull       6000  tryWithFlush ;;
  let tryWithCaree = fun css -> work css isCaree      7000  tryWithFull  ;;
  let tryWithFlStr = fun css -> work css isFlushStr8  8000  tryWithCaree ;;

  (*  css is the list of lists and each list is poket @ board *)
  let start css =
    let ts = List.hd css |> Bat.drop 2 (* we took the board cards only                      *)
    in if isColor ts                   (* and chek if they have three-in-suit               *)
       then tryWithFlStr css           (* and in this case we start check from the begining *)
       else                            (* else we skip the first step in checking           *)
         if   isWet ts                 (* and check if board have pair                      *)
         then tryWithCaree css         (* in which case we check Caree and Full and so on   *)
         else tryWithStr css ;;        (* else we start from Stright                        *)

end ;;

      
