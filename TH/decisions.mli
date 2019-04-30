module Decisions :
sig

  type hand = (int * int) list

    val isFlushStr8 : hand -> hand      
    val isCaree     : hand -> hand
    val isFull      : hand -> hand
    val isFlush     : hand -> hand
    val isStraight  : hand -> hand
    val isSet       : hand -> hand
    val isPair      : hand -> hand
    val isDupal     : hand -> hand
    val isHigh      : hand -> hand
      
end
