(** Let you create in a simple way an
    hidden integer counter that garanties,
    that every created int is unique. 
    @author: Phillip Heidegger
*)

(** module type with the prefix for
    the string_of function. *)
module type S = sig
  val prefix : string
end

(** Output signature of the functor *)
module type HI = sig
  type t
  val create_new : unit -> t
  val compare : t -> t -> int
  val string_of : t -> string
  val reset : unit -> unit
  val from_int : int -> t
end

(** Create a new instance of an HiddenInteger
    module *)
module Make : functor ( X : S ) -> HI
