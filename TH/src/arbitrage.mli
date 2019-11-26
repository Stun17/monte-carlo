module Arbitrage :
sig
  
  type hand = (int * int) list  (* list of pairs rank/suit *)
  type cards = hand list

  val tryWithHigh  : cards -> unit 
  val tryWithPair  : cards -> unit 
  val tryWithDupal : cards -> unit 
  val tryWithSet   : cards -> unit 
  val tryWithStr   : cards -> unit 
  val tryWithFlush : cards -> unit 
  val tryWithFull  : cards -> unit              
  val tryWithCaree : cards -> unit
  val tryWithFlStr : cards -> unit

  val start          : cards -> unit
    
end
