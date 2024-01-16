module Make : 
  functor (R: sig val reset : unit -> unit end) -> 
sig
  val wrap_reset : ('a -> 'b) -> 'a -> 'b
  val wrap_list : (string * (unit -> unit)) list -> (string * (unit -> unit)) list
end
