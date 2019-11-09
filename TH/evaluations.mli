module Evaluations :
  sig

    type hand = (int * int) list
              
    val getHigh      : hand -> int 
    val getPair      : hand -> int
    val getDupal     : hand -> int
    val getSet       : hand -> int 
    val getStr8      : hand -> int 
    val getFlush     : hand -> int
    val getFull      : hand -> int
    val getCaree     : hand -> int
    val getFlushStr8 : hand -> int

    val kicker    : hand -> hand -> int -> unit 
    val arbitThem : hand -> int -> unit 
     
  end ;;  
