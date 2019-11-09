open Bat ;;
  
module Evaluations =
  struct

    type hand = (int * int) list
    
    let getHigh xs =
      List.sort compare xs |> List.rev |> List.split |> fst |> List.hd
    ;;

    let getPair xs =
      List.sort compare xs |> List.rev |> List.split |> fst |>
        fun ys ->
        List.fold_right (
            fun x (a, b) ->
            if x = b then (x, x) else (a, x))
                        (List.tl ys) (List.hd ys, 0) 
    ;;

    let getDupal xs =
      List.sort compare xs |> List.rev |> List.split |> fst |>
        fun ys ->
        let ys1 = ys @ [0]
        and ys2 = 0 :: ys
        in List.map2 (fun s1 s2 -> if s1 = s2 then s1 else 0) ys1 ys2 |>
             List.sort compare |> List.rev |>
             fun zs -> (List.hd zs, List.tl zs |> List.hd)
    ;;

    let getSet xs =
      List.sort compare xs |> List.rev |> List.split |> fst |>
        fun ys ->
        let ys1 = ys @ [0]
        and ys2 = 0 :: ys
        in List.map2 (fun s1 s2 -> if s1 = s2 then s1 else 0) ys1 ys2 |>
             List.sort compare |> List.rev |> List.hd
    ;;

    let getStr8 xs =
      List.sort compare xs |> List.rev |> List.split |> fst |>
        fun ys ->
        let ys1 = ys @ [0]
        and ys2 = 0 :: ys
        in List.map2 (fun s1 s2 -> if s2 - s1 = 1 then s2 else 0) ys1 ys2 |>
             List.sort compare |> List.rev |> List.hd  

    let kicker xs ys title =
      let one = List.sort compare xs |> List.rev 
      and two = List.sort compare ys |> List.rev 
      in
      match
        compare (getHigh one) (getHigh two)
      with
        | -1 -> ()
        |  1 -> ()
        | _  ->
           (match
              compare (List.tl one |> getHigh) (List.tl two |> getHigh)
            with
            | -1 -> ()
            |  1 -> ()
            | _  ->
               (match
                  compare (Bat.drop 2 one |> getHigh) (Bat.drop 2 two |> getHigh)
                with
                | -1 -> ()
                |  1 -> ()
                | _  ->
                   (match
                      compare (Bat.drop 3 one |> getHigh) (Bat.drop 3 two |> getHigh)
                    with
                    | -1 -> ()
                    |  1 -> ()
                    | _  ->
                       (match
                          compare (Bat.drop 4 one |> getHigh) (Bat.drop 4 two |> getHigh)
                        with
                        | -1 -> ()
                        |  1 -> ()
                        | _  ->
                           (match
                              compare (Bat.drop 5 one |> getHigh) (Bat.drop 5 two |> getHigh)
                            with
                            | -1 -> ()
                            |  1 -> ()
                            | _  ->
                               (match
                                  compare (Bat.drop 6 one |> getHigh) (Bat.drop 6 two |> getHigh)
                                with
                                | -1 -> ()
                                |  1 -> ()
                                | _  -> () ; () )
                           )
                       )
                   )
               )
           )
   ;;

   let arbitThem xs title =
     let one = Bat.take 7 xs and two = Bat.drop 7 xs |> Bat.take 7 in
     match title with
     | 900 ->
        kicker one two title
     | 800 ->
        (match
           compare (getPair one) (getPair two)
         with
         | -1 -> ()
         | 1 -> ()
         | _ -> kicker one two title )
     | 700 ->
        (match
           compare (getDupal one) (getDupal two)
         with
         | -1 -> ()
         |  1 -> ()
         | _ -> kicker one two title )
     | 600 ->
        (match
           compare (getSet one) (getSet two)
         with
         | -1 -> ()
         |  1 -> ()
         | _ -> kicker one two title )           
     | 500 ->
        (match
           compare (getStr8 one) (getStr8 two)
         with
         | -1 -> () | 1 -> ()
         | _  -> kicker one two title )              
     | 400  -> () 
     | 300  -> () 
     | 200  -> ()   
     | 100  -> ()
     | _    -> ()
   ;;
     
  end ;;  
