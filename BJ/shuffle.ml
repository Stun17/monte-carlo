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

    let shuffle =
      fun () -> Random.self_init () ; genDeck (52, []) |>
        List.map (
          fun x ->
            let y = x mod 13 
            in match y with
               | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 -> y + 2
               | 8 | 9 | 10 | 11  -> 10
               | _ -> 11)

end ;;

(* open Printf ;; *)
(* let testIt = *)
(*   fun () ->   *)
(*   Shuffle.shuffle () |> List.sort (fun (r1, s1) (r2, s2) -> *)
(*                             if s1 < s2 then -1 else *)
(*                               if s1 == s2 then if r1 < r2 then -1 else 1 *)
(*                                 else 1 ) *)
(*   |> List.iter (fun (r, s) -> printf "%i, " r)  *)


