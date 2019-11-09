module Evaluations :
  sig

    type hand = (int * int) list (* list of pairs rank/suit *)
              
    val priceHigh      : hand -> int 
    val pricePair      : hand -> int
    val priceDupal     : hand -> int
    val priceSet       : hand -> int 
    val priceStr8      : hand -> int 
    val priceFlush     : hand -> int
    val priceFull      : hand -> int
    val priceCaree     : hand -> int
    val priceFlushStr8 : hand -> int

  end ;;  
