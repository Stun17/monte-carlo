module Arbitrage :
sig
  type cards = (int * int) list 
             
  val arbitIt : int -> cards list * cards -> unit 
end
