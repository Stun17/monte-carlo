module Dealing :
sig
  type hands = (int * int) list
  type cards = hands list
             
  val dealing : int -> hands -> cards
end
