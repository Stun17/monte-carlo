module Shuffle =
struct

  (* input: num of cards,  accum ;   output: shuffled deck *)
    let rec genDeck ((num : int), (acc : int list)) : int list = 
      match num with
      | 0 -> acc 
      | t -> let newval = Random.int 52
             in if List.mem newval acc
                then genDeck (num, acc)
                else let acc2 = newval :: acc
                     in genDeck (t - 1, acc2)
    ;;

    let rangeIt (deck : int list) : (int * int) list =
      List.map (fun x -> (x mod 13, x / 13)) deck ;;
      
    let shuffle =
      fun () -> Random.self_init () ; genDeck (28, []) |> rangeIt

end ;;




