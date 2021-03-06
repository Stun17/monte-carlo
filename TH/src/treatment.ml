open Printf ;;

(*
    to create, insert and extract results from/to hash-table
    key of this hash is (table_size, rank1, rank2, suited 1 / unsuited 0) tuple 
 *)
  
module Treatment =
  struct

    let my_hash = Hashtbl.create 1521 ;; (*    13 x 13 x 2 x 9 / 2   *)

    let init_hash () =
      List.iter (fun k ->
          List.iter (fun s ->
              List.iter (fun r2 -> 
                  List.iter (fun r1 ->
                      if r1 >= r2
                      then Hashtbl.add my_hash (k, r1, r2, s) (0, 0)
                      else ()
                    ) [12;11;10;9;8;7;6;5;4;3;2;1;0] (* the first card *)
                ) [12;11;10;9;8;7;6;5;4;3;2;1;0]     (* the second card *)
            ) [0;1]             (* suit / unsuit *)
        ) [2;3;4;5;6;7;8;9;10]  (* talbe sizes *)
    ;;

    let insert_deal (k, r1, s1, r2, s2) =
      if s1 == s2               (* suited *)
      then 
        let (m, n) = Hashtbl.find my_hash (k, r1, r2, 1)
        in Hashtbl.replace my_hash (k, r1, r2, 1) (m - 1, n + 1)
      else                      (* unsuited *)
        let (m, n) = Hashtbl.find my_hash (k, r1, r2, 0)
        in Hashtbl.replace my_hash (k, r1, r2, 0) (m - 1, n + 1)
    ;;
      
    let insert_win (k, r1, s1, r2, s2, q) =
    if s1 = s2                  (* suited *)
      then
        let (m, n) = Hashtbl.find my_hash (k, r1, r2, 1)
        in Hashtbl.replace my_hash (k, r1, r2, 1) (m + q, n)
      else                      (* unsuited *)
        let (m, n) = Hashtbl.find my_hash (k, r1, r2, 0)
        in Hashtbl.replace my_hash (k, r1, r2, 0) (m + q, n) 
    ;;
      
    let extract_all =
      fun _ ->
      List.iter (fun k ->
          List.iter (fun s ->
              List.iter (fun r2 -> 
                  List.iter (fun r1 ->
                      if (r1 > r2) || ((r1 = r2) && (s = 0))
                      then
                        let c1 =
                          match r1 with
                          | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 -> string_of_int (r1 + 2)
                          | 8  -> "t" | 9  -> "j" | 10 -> "q" | 11 -> "k" | _  -> "a"
                        and  c2 =
                          match r2 with
                          | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 -> string_of_int (r2 + 2)
                          | 8  -> "t" | 9  -> "j" | 10 -> "q" | 11 -> "k" | _  -> "a"
                        and c3 =
                          match s with
                          | 0 -> "u" | _ -> "s"
                        and (m, n) = Hashtbl.find my_hash (k, r1, r2, s)
                        in let rez = (float_of_int m) /. (float_of_int n)
                           in  printf "%2i %s%s%s %3.1f\n" k c1 c2 c3 rez
                      else ()
                    ) [12;11;10;9;8;7;6;5;4;3;2;1;0] (* the first card *)
                ) [12;11;10;9;8;7;6;5;4;3;2;1;0]     (* the second card *)
            ) [0;1]             (* suited / unsuited *)
        ) [2;3;4;5;6;7;8;9;10]  (*  table sizes *)
    ;;
                    
  end
;;
