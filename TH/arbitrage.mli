module Arbitrage :
sig
  
  type hand = (int * int) list 
  type cards = hand list

  val isAnyHaveHigh  : cards -> unit 
  val isAnyHavePair  : cards -> unit 
  val isAnyHaveDupal : cards -> unit 
  val isAnyHaveSet   : cards -> unit 
  val isAnyHaveStr   : cards -> unit 
  val isAnyHaveFlush : cards -> unit 
  val isAnyHaveFull  : cards -> unit              
  val isAnyHaveCaree : cards -> unit
  val isAnyHaveFlStr : cards -> unit
    
end
