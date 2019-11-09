open Printf ;;
  
module Treatment =
  struct

    let my_hash = Hashtbl.create 169 ;;

    let inithash () = 
      List.iter (
          fun s ->
          List.iter (
              fun r2 -> 
              List.iter (
                  fun r1 ->
                  if r1 > r2 || (r1 = r2 && s = 0)
                  then Hashtbl.add my_hash (r1, r2, s) (0, 0)
                  else ()
                ) [12;11;10;9;8;7;6;5;4;3;2;1;0]
            ) [12;11;10;9;8;7;6;5;4;3;2;1;0]
        ) [0; 1]
    ;;

    let insert_deal (r1, s1, r2, s2) =
      if s1 = s2
      then
        let (n, m) = Hashtbl.find my_hash (r1, r2, 1)
        in Hashtbl.replace my_hash (r1, r2, 1) (1 + n, m)
      else
        let (n, m) = Hashtbl.find my_hash (r1, r2, 0)
        in Hashtbl.replace my_hash (r1, r2, 0) (1 + n, m) 
    ;;
      
    let insert_win (r1, s1, r2, s2) =
    if s1 = s2
      then
        let (n, m) = Hashtbl.find my_hash (r1, r2, 1)
        in Hashtbl.replace my_hash (r1, r2, 1) (n, m + 1)
      else
        let (n, m) = Hashtbl.find my_hash (r1, r2, 0)
        in Hashtbl.replace my_hash (r1, r2, 0) (n, m + 1) 
      ;;
      
    let extract () =
      List.iter (
          fun s ->
          List.iter (
              fun r2 -> 
              List.iter (
                  fun r1 ->
                  if r1 > r2 || (r1 = r2 && s = 0)
                  then
                    let (n, m) = Hashtbl.find my_hash (r1, r2, s)
                    in printf "%2i %2i %i %5.2f\n" r1 r2 s ((float_of_int m) /. (float_of_int n)) 
                  else ()
                ) [12;11;10;9;8;7;6;5;4;3;2;1;0]
            ) [12;11;10;9;8;7;6;5;4;3;2;1;0]
        ) [0; 1]
    ;;
                    
  end
