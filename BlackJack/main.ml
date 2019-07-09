open Shuffle ;; open List ;;

let prepare [c1; c2; c3; c4; c5; c6; c7; c8; c9; c10; c11; c12; c13; c14; c15; c16; c17; c18; c19; c20; c21; c22; c23; c24; c25; c26; c27; c28; c29; c30; c31; c32; c33; c34; c35; c36; c37; c38; c39; c40; c41; c42; c43; c44; c45; c46; c47; c48; c49; c50; c51; c52] =
  let deal = [c1; c2]
  and pl1  = [c3; c4] 
  and pl2  = [c5; c6]
  and pl3  = [c7; c8]
  and deck = [c9; c10; c11; c12; c13; c14; c15; c16; c17; c18; c19; c20; c21; c22; c23; c24; c25; c26; c27; c28; c29; c30; c31; c32; c33; c34; c35; c36; c37; c38; c39; c40; c41; c42; c43; c44; c45; c46; c47; c48; c49; c50; c51; c52] 
  in ( [pl1; pl2; pl3], deal, deck )
;;

let print_card = fun x -> print_int x, print_char ' ' ;;

(* let testIt = Shuffle.shuffle () |> List.sort compare |> List.map (fun x -> print_int x, print_char ' ') *)
  
shuffle () |> prepare |> fun (players, dealer, deck) ->
     map print_card deck, print_newline () ,
     map (fun xs -> map print_card xs, print_newline ()) players ,
     map print_card dealer, print_newline () ;;

print_newline () ;;
