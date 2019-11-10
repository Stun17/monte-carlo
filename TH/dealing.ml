open Bat ;; open Treatment ;;
  
module Dealing =
  struct
    type hands = (int * int) list
    type cards = hands list

    exception WrongNumOfGamers ;;
      
    let poketSort = fun (r1, s1) (r2, s2) -> if r1 < r2 then 1 else -1 ;;
  
    let dealing (n : int)  (deck : hands) : cards =
      if n < 2 || n > 10 then raise WrongNumOfGamers else 
      let board   = Bat.take  5 deck
      and player1 = Bat.drop  5 deck |> Bat.take 2 |> List.sort poketSort 
      and player2 = Bat.drop  7 deck |> Bat.take 2 |> List.sort poketSort 
      and player3 = Bat.drop  9 deck |> Bat.take 2 |> List.sort poketSort 
      and player4 = Bat.drop 11 deck |> Bat.take 2 |> List.sort poketSort 
      and player5 = Bat.drop 13 deck |> Bat.take 2 |> List.sort poketSort 
      and player6 = Bat.drop 15 deck |> Bat.take 2 |> List.sort poketSort 
      and player7 = Bat.drop 17 deck |> Bat.take 2 |> List.sort poketSort 
      and player8 = Bat.drop 19 deck |> Bat.take 2 |> List.sort poketSort 
      and player9 = Bat.drop 21 deck |> Bat.take 2 |> List.sort poketSort 
      and player0 = Bat.drop 23 deck |> Bat.take 2 |> List.sort poketSort 
      in let ingame = Bat.take n
                               [ player1 @ board
                               ; player2 @ board
                               ; player3 @ board
                               ; player4 @ board
                               ; player5 @ board
                               ; player6 @ board
                               ; player7 @ board
                               ; player8 @ board
                               ; player9 @ board
                               ; player0 @ board
                               ]
         in List.iter
              (
                fun xs ->
                (* length of xs has been tested so all good with warning *)
                let (r1, s1) :: (r2, s2) :: _ = xs
                in Treatment.insert_deal (r1, s1, r2, s2)
              )
              ingame |> fun _ -> ingame
    ;;

  end
  
