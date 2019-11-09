module Evaluations :
  sig

    type hand = (int * int) list
              
    val getHigh   : hand -> int 
    val getPair   : hand -> int * int 
    val getDupal  : hand -> int * int
    val getSet    : hand -> int 
    val getStr8   : hand -> int 

    val kicker    : hand -> hand -> int -> unit 
    val arbitThem : hand -> int -> unit 
     
  end ;;  
