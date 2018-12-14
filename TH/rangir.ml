open Batteries ;;

exception Bad_list_of_cards ;;
  
module Rangir =
  struct
    
    type cards = int * int list

    let ranksort =
      fun [(r1,_);(r2,_)] [(k1,_);(k2,_)] ->
                     if r1 > k1 && r1 > k2 then 1 else
                       if r2 > k1 && r2 > k2 then 1 else
                         if r1 == k1 && r2 == k2 then 0 else
                           if r1 == k2 && r2 == k1 then 0 else -1
                      
    let rangeHigh =
      fun css ->
      let pockets = List.map (fun cs -> List.take 2 cs) css
      and (maxRank,_) = List.map (fun cs -> List.max cs) css |> List.max
                      (* pockets list never will be empty so ignore warning  *)
      in let rez = List.filter (fun [(r1,_); (r2,_)] -> r1 == maxRank || r2 == maxRank) pockets
         in if List.length rez > 0
            then List.sort ranksort rez 
            else List.sort ranksort pockets 
    ;; 
                      
    let rangePairs css = css
                       
  end ;;


  (* test suite *)
let board = [(3,1);(7,3);(0,1);(5,1);(2,1)] ;;

(* Rangir.ranksort  *)
let tu2 = Rangir.rangeHigh
            [
              [( 4, 1); ( 1, 1)]   @ board ;
              [( 6, 3); ( 2, 2)]   @ board ;
              [( 1, 0); ( 6, 1)]   @ board ;
            ]
