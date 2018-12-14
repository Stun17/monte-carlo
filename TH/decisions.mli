module Decisions :
sig
    type cards = (int * int) list

    val isDry       : cards -> bool               
    val isColored   : cards -> bool
      
    val isFluStr8   : cards -> bool      
    val isCare      : cards -> bool
    val isFull      : cards -> bool
    val isFlush     : cards -> bool
    val isStraight  : cards -> bool
    val isSet       : cards -> bool
    val isPair      : cards -> bool
    val isDupal     : cards -> bool
    val isHigh      : cards -> bool 
end
