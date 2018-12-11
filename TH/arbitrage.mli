module Arbitrage :
sig
  type cards = (int * int) list 
             
  val arbitit :  cards list * cards -> unit 
end
