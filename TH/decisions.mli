module Decisions :
sig

  type hand = (int * int) list

    val isFlushStr8 : hand -> bool      
    val isCaree     : hand -> bool
    val isFull      : hand -> bool
    val isFlush     : hand -> bool
    val isStraight  : hand -> bool
    val isSet       : hand -> bool
    val isPair      : hand -> bool
    val isDupal     : hand -> bool
    val isHigh      : hand -> bool

    val isColor     : hand -> bool
    val isDry       : hand -> bool
end
