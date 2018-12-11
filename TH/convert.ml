module type Convert =
sig
    val myConvi : int * int -> string
end  ;;

module Convert =
struct
  let myConvi (r, s) =
    let suit =
      match s with | 0 -> "♠" | 1 -> "♣" | 2 -> "♦" | _ -> "♥"
    and rank =
      match r with | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 -> string_of_int (r + 2)
                   | 8 -> "T" | 9 -> "J" | 10 -> "Q" | 11 -> "K" | _ -> "A"
    in rank ^ suit
  ;;
end ;;
