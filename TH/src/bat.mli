module Bat : 
sig

  val take     : int -> 'a list -> 'a list 
  val drop     : int -> 'a list -> 'a list
  val delete   : 'a -> 'a list -> 'a list
  val ( -- )   : int -> int -> int Stream.t
    
end ;;
