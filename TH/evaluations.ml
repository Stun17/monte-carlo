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
        List.fold_right
          (fun x acc -> if x == acc then acc else x)
          (List.tl ys)
          (List.hd ys) 
    ;;

    let getDupal xs =
      List.sort compare xs |> List.rev |> List.split |> fst |>
        fun ys ->
        let ys1 = ys @ [0]
        and ys2 = 0 :: ys
        in List.map2 (fun s1 s2 -> if s1 = s2 then s1 else 0) ys1 ys2 |>
             List.sort compare |> List.rev |>
             fun zs -> 10 * (List.hd zs) + (List.hd (List.tl zs))
    ;;

    let getSet xs =
      List.sort compare xs |> List.rev |> List.split |> fst |>
        fun ys ->
        List.map2 (fun s1 s2 -> if s1 = s2 then s1 else 0) (ys @ [0]) (0 :: ys) |>
          List.sort compare |> List.rev |> List.hd
    ;;

    let getStr8 xs =
      List.sort compare xs |> List.rev |> List.split |> fst |>
        fun ys ->
        let ys1 = ys @ [0]
        and ys2 = 0 :: ys
        in List.map2 (fun s1 s2 -> if s2 - s1 = 1 then s2 else 0) ys1 ys2 |>
             List.sort compare |> List.rev |> List.hd  

    let getFlush xs     = 0 ;;
    let getFull xs      = 0 ;;
    let getCaree xs     = 0 ;;
    let getFlushStr8 xs = 0 ;;
      
    let rec kicker xs ys title =
      let one = List.sort compare xs |> List.rev 
      and two = List.sort compare ys |> List.rev 
      in
        match compare (getHigh one) (getHigh two) with
        | -1 -> ()
        |  1 -> ()
        |  _ -> kicker (List.tl one) (List.tl two) title
    ;;

   let arbitThem xs title =
     let one = Bat.take 7 xs and two = Bat.drop 7 xs |> Bat.take 7 in
     match title with
     | 900 -> kicker one two title
     | 800 -> (match compare (getPair one) (getPair two) with
               | -1 -> ()
               |  1 -> ()
               |  _ -> kicker one two title )
     | 700 -> (match compare (getDupal one) (getDupal two) with
               | -1 -> ()
               |  1 -> ()
               |  _ -> kicker one two title )
     | 600 -> (match compare (getSet one) (getSet two) with
               | -1 -> ()
               |  1 -> ()
               |  _ -> kicker one two title )           
     | 500 -> (match compare (getStr8 one) (getStr8 two) with
               | -1 -> ()
               |  1 -> ()
               |  _ -> kicker one two title )              
     | 400  -> (match compare (getFlush one) (getFlush two) with
                | -1 -> ()
                |  1 -> ()
                |  _ -> kicker one two title ) 
     | 300  -> (match compare (getFull one) (getFull two) with
                | -1 -> ()
                |  1 -> ()
                |  _ -> kicker one two title ) 
     | 200  -> (match compare (getCaree one) (getCaree two) with
                | -1 -> ()
                |  1 -> ()
                |  _ -> kicker one two title ) 
     | 100  -> (match compare (getFlushStr8 one) (getFlushStr8 two) with
                | -1 -> ()
                |  1 -> ()
                |  _ -> kicker one two title ) 
     | _    -> ()
   ;;
     
  end ;;  
