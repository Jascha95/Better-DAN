open ProglangUtils
open ExtSSet

(** This module deals with the locations of the objects. *)
module Loc : sig
  (** location *)
  type t

  (** create a new location *)
  val create : unit -> t

  (** Compares two locations *)
  val compare : t -> t -> int

  (** Returns a string represenation of a location *)
  val string_of : t -> string

  (** IMPORTANT: Use this only, if you lex code. *)
  val from_int : int -> t

  (** IMPORTANT: Use this only for testing. Otherwise
      the create_new function will not create unique
      locations. *)
  val reset : unit -> unit
end

module type LSET = ExtSSet.S 
  with type elt = Loc.t

(** Set of Locations *)
module LSet : LSET

module LSetRef : sig
  include ExtSSetImp.S
  val lset : t -> LSet.t
end 
  with type elt = Loc.t

(** This module represents pointers to locations. They
    could be exact or inexact. *)
module P : sig
  (** Program locations *)
  type t

  (** Create a new unique exact pointer *)
  val create_exact : unit -> t

  (** Create a new unique inexact pointer *)
  val create_inexact : unit -> t

  (** Returns a string representation of a pointer *)
  val string_of : t -> string

  (** [get_location p] returns the location of the pointer. *)
  val get_location : t -> Loc.t

  (** [is_precise p] returns [true] if the pointer is precise, 
      otherwise [false] is returned. *)
  val is_precise : t -> bool
  
  (** IMPORTANT: Use this only for testing. Otherwise
      the create_new function will not create unique
      locations. *)
  val reset : unit -> unit
end

(** IMPORTENT: Do use this only for testing! 
               Calls reset of P and Loc. 
    It destroys the invariant that new locations and
    pointers a unique!
*)
val reset_all : unit -> unit


(** Module that is the image of location set variables.
    You can collect positive, negative and upper bound
    locations. The module will merge then as needed using
    the union function. *)
module LSetInEx : LowerUpperNeg.S
  with type elm = LSet.t

