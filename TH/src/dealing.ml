open Bat ;; 
open Treatment ;;

(* module to deal shuffled deck *)
module Dealing =
  struct

    type hands = (int * int) list
    type cards = hands list

    exception BadDeck ;;

    (* to form list of lists of hands . each card is coded as (rank:int, suit:int) 
       params :   number of Players :int 
                  shuffled deck : plain list of pairs (rank:int, suit:int)
    *)  
    let dealing (n : int)  (deck : hands) : cards =
      (* test to quaranty succesfull pattern matching *)
      if 28 > List.length deck then raise BadDeck else
        (* to form pockets *)
        (* because ocaml PM works only with just few items you have to treat by 5 elems *)
      let [ p11 ; p21 ; p31 ; p41 ; p51 ]     = Bat.take  5 deck 
      and [ p61 ; p71 ; p81 ; p91 ; p01 ]     = Bat.drop  5 deck |> Bat.take 5
      and [ p12 ; p22 ; p32 ; p42 ; p52 ]     = Bat.drop 10 deck |> Bat.take 5
      and [ p62 ; p72 ; p82 ; p92 ; p02 ]     = Bat.drop 15 deck |> Bat.take 5
      and [ _ ; b1 ; b2; b3 ; _ ; b4; _ ; b5] = Bat.drop 20 deck |> Bat.take 8
      in
      (* to put pockets into hash table *)
      List.iter (fun [ (r1, s1) ; (r2, s2) ] -> Treatment.insert_deal (r1, s1, r2, s2))
                   ( Bat.take n [ [ p11 ; p12 ]
                                ; [ p21 ; p22 ]
                                ; [ p31 ; p32 ]
                                ; [ p41 ; p42 ]
                                ; [ p51 ; p52 ]
                                ; [ p61 ; p62 ]
                                ; [ p71 ; p72 ]
                                ; [ p81 ; p82 ]
                                ; [ p91 ; p92 ]
                                ; [ p01 ; p02 ]
                                ]
                   ) |>
        (* to form the hands *)
        fun _ ->
                        Bat.take n [ [ p11 ; p12   ; b1 ; b2 ; b3 ; b4 ; b5 ]
                                   ; [ p21 ; p22   ; b1 ; b2 ; b3 ; b4 ; b5 ]
                                   ; [ p31 ; p32   ; b1 ; b2 ; b3 ; b4 ; b5 ]
                                   ; [ p41 ; p42   ; b1 ; b2 ; b3 ; b4 ; b5 ]
                                   ; [ p51 ; p52   ; b1 ; b2 ; b3 ; b4 ; b5 ]
                                   ; [ p61 ; p62   ; b1 ; b2 ; b3 ; b4 ; b5 ]
                                   ; [ p71 ; p72   ; b1 ; b2 ; b3 ; b4 ; b5 ]
                                   ; [ p81 ; p82   ; b1 ; b2 ; b3 ; b4 ; b5 ]
                                   ; [ p91 ; p92   ; b1 ; b2 ; b3 ; b4 ; b5 ]
                                   ; [ p01 ; p02   ; b1 ; b2 ; b3 ; b4 ; b5 ]
                               ]
    ;;

  end
  
