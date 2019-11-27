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
      List.iter (fun [ (r1, s1) ; (r2, s2) ] -> Treatment.insert_deal (n, r1, s1, r2, s2))
                   ( Bat.take n [ List.sort compare [ p11 ; p12 ] |> List.rev
                                ; List.sort compare [ p21 ; p22 ] |> List.rev
                                ; List.sort compare [ p31 ; p32 ] |> List.rev
                                ; List.sort compare [ p41 ; p42 ] |> List.rev
                                ; List.sort compare [ p51 ; p52 ] |> List.rev
                                ; List.sort compare [ p61 ; p62 ] |> List.rev
                                ; List.sort compare [ p71 ; p72 ] |> List.rev
                                ; List.sort compare [ p81 ; p82 ] |> List.rev
                                ; List.sort compare [ p91 ; p92 ] |> List.rev
                                ; List.sort compare [ p01 ; p02 ] |> List.rev
                                ]
                   ) |>
        (* to form the hands *)
        fun _ ->
        Bat.take n [ (List.sort compare [ p11 ; p12 ] |> List.rev) @ [ b1 ; b2 ; b3 ; b4 ; b5 ]
                   ; (List.sort compare [ p21 ; p22 ] |> List.rev) @ [ b1 ; b2 ; b3 ; b4 ; b5 ]
                   ; (List.sort compare [ p31 ; p32 ] |> List.rev) @ [ b1 ; b2 ; b3 ; b4 ; b5 ]
                   ; (List.sort compare [ p41 ; p42 ] |> List.rev) @ [ b1 ; b2 ; b3 ; b4 ; b5 ]
                   ; (List.sort compare [ p51 ; p52 ] |> List.rev) @ [ b1 ; b2 ; b3 ; b4 ; b5 ]
                   ; (List.sort compare [ p61 ; p62 ] |> List.rev) @ [ b1 ; b2 ; b3 ; b4 ; b5 ]
                   ; (List.sort compare [ p71 ; p72 ] |> List.rev) @ [ b1 ; b2 ; b3 ; b4 ; b5 ]
                   ; (List.sort compare [ p81 ; p82 ] |> List.rev) @ [ b1 ; b2 ; b3 ; b4 ; b5 ]
                   ; (List.sort compare [ p91 ; p92 ] |> List.rev) @ [ b1 ; b2 ; b3 ; b4 ; b5 ]
                   ; (List.sort compare [ p01 ; p02 ] |> List.rev) @ [ b1 ; b2 ; b3 ; b4 ; b5 ]
                   ]
    ;;

  end
  
