module type Convert =
sig
    val myConvi : int * int -> string
end  ;;

module Convert =
  struct
    
  let myConvi (r, s) =
    let suit =
      match s with
      | 0 -> "♠"
      | 1 -> "♣"
      | 2 -> "♦"
      | _ -> "♥"
    and rank =
      match r with
      | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 -> string_of_int (r + 2)
      | 8 -> "T"
      | 9 -> "J"
      | 10 -> "Q"
      | 11 -> "K"
      | _ -> "A"
    in rank ^ suit
  ;;

  let myIvonc (r, s) =
    let suit =
      match s with
      | "♠" -> 0
      | "♣" -> 1
      | "♦" -> 2
      | _ -> 3
    and rank =
      match r with
      | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> (int_of_string r) - 2
      | "T" -> 8
      | "J" -> 9
      | "Q" -> 10
      | "K" -> 11
      | _ -> 12
    in (rank, suit)
    ;;
    
  end ;;

(* let te01 = List.map Convert.myIvonc [("A","♥");("3","♥");("6","♥");("4","♥");("9","♣");("5","♥");("2","♥") ]  *)
(* let te02 = List.map Convert.myIvonc [("6","♣");("3","♣");("5","♣");("9","♥");("9","♣");("7","♣");("4","♣") ]  *)
(* let te03 = List.map Convert.myIvonc [("5","♠");("4","♠");("A","♠");("A","♣");("8","♥");("3","♠");("2","♠") ]   *)
(* let te04 = List.map Convert.myIvonc [("Q","♠");("5","♠");("T","♦");("9","♠");("7","♠");("8","♠");("6","♠") ]   *)
(* let te05 = List.map Convert.myIvonc [("6","♣");("4","♣");("3","♣");("4","♠");("2","♣");("5","♣");("T","♦") ]   *)
(* let te06 = List.map Convert.myIvonc [("5","♦");("4","♦");("6","♦");("T","♥");("Q","♠");("2","♦");("3","♦") ]   *)
(* let te07 = List.map Convert.myIvonc [("5","♠");("4","♠");("7","♠");("8","♠");("T","♦");("6","♠");("A","♠") ]   *)
(* let te08 = List.map Convert.myIvonc [("9","♥");("6","♥");("7","♥");("8","♥");("T","♥");("K","♠");("5","♥") ]   *)
(* let te09 = List.map Convert.myIvonc [("9","♥");("6","♥");("A","♠");("7","♥");("T","♥");("9","♦");("8","♥") ]   *)
(* let te10 = List.map Convert.myIvonc [("3","♦");("2","♦");("6","♦");("T","♠");("5","♦");("4","♦");("K","♥") ]   *)
(* let te11 = List.map Convert.myIvonc [("6","♣");("5","♣");("3","♣");("4","♣");("7","♣");("5","♠");("7","♥") ]   *)
