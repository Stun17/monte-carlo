open Batteries ;;

exception Bad_list_of_cards ;;
  
module Rangir =
  struct
    
    type cards = int * int list

    let rangeHigh =
      fun css ->
      let pockets = List.map (fun cs -> List.take 2 cs) css
      and maxRank = List.map (fun cs -> List.max cs) css |> List.max
      in let rez = List.filter
                     (fun xs -> not (List.is_empty (List.filter (fun x -> x == maxRank) xs)))
                     pockets
            in if List.length rez > 0 then rez else pockets 
    ;; 
                      
    let rangePairs css = css
                       
  end ;;

let tu2 = Rangir.rangeHigh 
            [
              [(4,1);(6,1);   (3,1);(7,3);(8,1);(5,1);(2,1)] ;
              [(11,1);(2,2);   (3,1);(7,3);(8,1);(5,1);(2,1)] ;
              [(11,0);(11,3);  (3,1);(7,3);(8,1);(5,1);(2,1)] ;              
            ]
