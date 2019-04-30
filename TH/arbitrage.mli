module Arbitrage :
sig
  type hand = (int * int) list 
  type cards = hand list
             
  val isAnyHaveFlStr : cards -> unit 
end
