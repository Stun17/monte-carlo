module Treatment :
sig
  val inithash      : unit -> unit
  val insert_deal   : int * int * int * int -> unit
  val insert_win    : int * int * int * int -> unit
  val extract       : unit -> unit 
end
