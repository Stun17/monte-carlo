module Rangir :
  sig
    type cards = (int * int) list
               
    val rangeHigh  : cards list -> cards list 
    val rangePair  : cards list -> cards list 
    val rangeDupal : cards list -> cards list 
    val rangeSet   : cards list -> cards list 
    val rangeStr   : cards list -> cards list  
    val rangeFlush : cards list -> cards list  
    val rangeFull  : cards list -> cards list  
    val rangeCare  : cards list -> cards list  
    val rangeFuSt  : cards list -> cards list  

  end ;;
