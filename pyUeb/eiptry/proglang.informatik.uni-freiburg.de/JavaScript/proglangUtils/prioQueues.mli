(** This module provides a Queue that allows to add each element with
    a priority. The pop operation behaves as follows:

    It returns the element with the minimal priority that was
    added into the queue at first. 
*)

open ExtSQueue

module type ORD = sig
  include Map.OrderedType

  val string_of : t -> string
end

module type S = sig
  type 'a t
  type key 
  exception Empty

  val create : unit -> 'a t
  val clear : 'a t -> unit
  val is_empty : 'a t -> bool

  val top : 'a t -> 'a 
  val pop : 'a t -> 'a 

  val push : key -> 'a -> 'a t -> unit

  val string_of : ('a -> string) -> 'a t -> string
end

module Make : functor (Ord: ORD) -> S
  with type key = Ord.t
