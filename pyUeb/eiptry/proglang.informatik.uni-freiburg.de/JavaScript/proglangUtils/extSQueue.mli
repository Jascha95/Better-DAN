(** This module provides a little bit extended version of the original
    Queue module of the standard library. 
    See {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Queue.html}Queue}.
*)

(** This signature is the original queue signature from the
    ocaml library. 
*)
module type QUEUE = 
sig
  type 'a t
  exception Empty
  val create : unit -> 'a t
  val add : 'a -> 'a t -> unit
  val push : 'a -> 'a t -> unit
  val take : 'a t -> 'a
  val pop : 'a t -> 'a
  val peek : 'a t -> 'a
  val top : 'a t -> 'a
  val clear : 'a t -> unit
  val copy : 'a t -> 'a t
  val is_empty : 'a t -> bool
  val length : 'a t -> int
  val iter : ('a -> unit) -> 'a t -> unit
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val transfer : 'a t -> 'a t -> unit
end

(** This is the extended Queue module. *)
module Queue : sig
  include QUEUE

  (** returns the string representation of a queue. *)
  val string_of : ('a -> string) -> 'a t -> string
end
