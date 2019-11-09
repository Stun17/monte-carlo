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
