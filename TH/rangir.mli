module Rangir :
  sig
    type cards = int * int list
               
    val rangePairs : cards list list -> cards list list
  end ;;
