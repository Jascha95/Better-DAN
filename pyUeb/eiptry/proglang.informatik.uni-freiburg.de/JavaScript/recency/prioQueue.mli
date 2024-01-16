open ProglangUtils
open PrioQueues

module type DATA = sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
  val string_of : t -> string
end

module type S = sig
  type t 
  type key 
  type data
  exception Empty
  val create : unit -> t
  val clear : t -> unit
  val is_empty : t -> bool
  val top : t -> data
  val pop : t -> data
  val push : key -> data -> t -> unit
  val string_of : t -> string
  val remove : data -> t -> unit
end

module Make: functor (Ord: ORD) -> functor (Data: DATA) -> S 
  with type key = Ord.t
  and type data = Data.t
