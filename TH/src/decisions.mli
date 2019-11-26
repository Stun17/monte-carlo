module Decisions :
sig

  type hand = (int * int) list  (* list of pairs rank/suit *)

    val isFlushStr8 : hand -> bool      
    val isCaree     : hand -> bool
    val isFull      : hand -> bool
    val isFlush     : hand -> bool
    val isStraight  : hand -> bool
    val isSet       : hand -> bool
    val isDupal     : hand -> bool
    val isPair      : hand -> bool
    val isHigh      : hand -> bool

    val isColor     : hand -> bool
    val isWet       : hand -> bool
end
