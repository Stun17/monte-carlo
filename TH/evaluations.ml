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

    let getFlush xs     = 999 ;;
    let getFull xs      = 999 ;;
    let getCaree xs     = 999 ;;
    let getFlushStr8 xs = 999 ;;
     
  end ;;  
