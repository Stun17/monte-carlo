module Shuffle =
struct

  (* input: 
   *   #card : int 
   *   accum : int list 
   * output:
   *   shuffled deck : int list
   *)
    let rec gendeck (num, acc) : int list = 
      match num with
      | 0 -> acc 
      | t -> let newval = Random.int 52 
             in if List.mem newval acc
                then gendeck (num, acc)
                else let acc2 = newval :: acc
                     in gendeck (t - 1, acc2) 

    let shuffle = fun () -> gendeck (52, []) 
  
end ;;

Random.self_init () ;;
List.map (fun x -> print_int x ; print_string " ") (Shuffle.shuffle ()) 
