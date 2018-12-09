module Decisions :
sig
  type cards = (int * int) list
            
    val isFlush    : cards -> bool
    val isCare     : cards -> bool
    val isSet      : cards -> bool
    val isPair     : cards -> bool
    val isFull     : cards -> bool
    val isStraight : cards -> bool
  end
