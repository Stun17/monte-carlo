
(*  obtain the highest rank of the hand *)
let getHigh xs =
  List.sort compare xs |> List.rev |> List.split |> fst |> List.hd
;;
  
let getPair xs =
  List.sort compare xs |> List.rev |> List.split |> fst |>
    fun ys ->
    List.fold_right (
        fun x (a, b) ->
        if x = b then (x,x) else (a,x))
      (List.tl ys) (List.hd ys, 0) 
;;

let getDupal xs =
  List.sort compare xs |> List.rev |> List.split |> fst |> fun ys ->
    let ys1 = ys @ [0]
    and ys2 = 0 :: ys
    in List.map2 (fun s1 s2 -> if s1 = s2 then s1 else 0) ys1 ys2
         |> List.sort compare |> List.rev |> fun zs -> (List.hd zs, List.tl zs |> List.hd)
;;

let getSet xs =
  List.sort compare xs |> List.rev |> List.split |> fst |> fun ys ->
    let ys1 = ys @ [0]
    and ys2 = 0 :: ys
    in List.map2 (fun s1 s2 -> if s1 = s2 then s1 else 0) ys1 ys2 |>
         List.sort compare |> List.rev |> List.hd
;;

let getStr8 xs =
  List.sort compare xs |> List.rev |> List.split |> fst |> fun ys ->
    let ys1 = ys @ [0]
    and ys2 = 0 :: ys
    in List.map2 (fun s1 s2 -> if s2 - s1 = 1 then s2 else 0) ys1 ys2 |>
         List.sort compare |> List.rev |> List.hd  

let kicker xs ys title =
  let one = List.sort compare xs |> List.rev 
  and two = List.sort compare ys |> List.rev 
  in match compare (getHigh one) (getHigh two) with
     | -1 -> print_hand ys title | 1 -> print_hand xs title | _ ->
      (match compare (List.tl one |> getHigh) (List.tl two |> getHigh) with
      | -1 -> print_hand ys title | 1 -> print_hand xs title | _ ->
      (match compare (Bat.drop 2 one |> getHigh) (Bat.drop 2 two |> getHigh) with
       | -1 -> print_hand ys title | 1 -> print_hand xs title | _  ->
       (match compare (Bat.drop 3 one |> getHigh) (Bat.drop 3 two |> getHigh) with
        | -1 -> print_hand ys title | 1 -> print_hand xs title | _ ->
        (match compare (Bat.drop 4 one |> getHigh) (Bat.drop 4 two |> getHigh) with
         | -1 -> print_hand ys title | 1 -> print_hand xs title | _ ->
         (match compare (Bat.drop 5 one |> getHigh) (Bat.drop 5 two |> getHigh) with
          | -1 -> print_hand ys title | 1 -> print_hand xs title | _ ->
          (match compare (Bat.drop 6 one |> getHigh) (Bat.drop 6 two |> getHigh) with
           | -1 -> print_hand ys title | 1 -> print_hand xs title | _ ->
              print_hand xs title ; print_hand ys title ))))))
;;

let arbitThem xs title =
  let one = Bat.take 7 xs and two = Bat.drop 7 xs |> Bat.take 7 in
  match title with
  | "high" ->
     kicker one two title
  | "pair" ->
     (match compare (getPair one) (getPair two) with
      | -1 -> print_hand two title | 1 -> print_hand one title 
      | _ -> kicker one two title )
  | "dupal" ->
     (match compare (getDupal one) (getDupal two) with
      | -1 -> print_hand two title | 1 -> print_hand one title
      | _ -> kicker one two title )
  | "set" ->
     (match compare (getSet one) (getSet two) with
      | -1 -> print_hand two title | 1 -> print_hand one title
      | _ -> kicker one two title )           
  | "str8" ->
     (match compare (getStr8 one) (getStr8 two) with
      | -1 -> print_hand two title | 1 -> print_hand one title
      | _ -> kicker one two title )              
  | "flush"  -> () 
  | "full"   -> () 
  | "caree"  -> ()   
  | "fl-st"  -> ()
  | _        -> ()
;;
  

(* ------------------------  test suite --------------------------  *)
  
let xs = [(11,1) ; (9,2) ; (7,0) ; (5,3) ; (6,3) ; (4,1) ; (3,0) ] ;;
  
(* let teHigh = getHigh  xs ;; *)
(* let tePair = getPair xs ;; *)
(* let teDupal = getDupal xs ;; *)
(* let teSet = getSet xs *)
(* let teStr = getStr8 xs *)
