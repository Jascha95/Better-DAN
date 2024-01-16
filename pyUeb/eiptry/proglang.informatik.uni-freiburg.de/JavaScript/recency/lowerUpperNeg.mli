(** Module that is the image of location set variables.
    You can collect positive, negative and upper bound
    locations. The module will merge then as needed using
    the union function. *)
open ProglangUtils

module type SMALLSET = sig
  include LowerUpper.SS_WO_ME
  val diff : t -> t -> t
end


module type S = sig
  
  type elm

  (** These are the alternatives of a lower-upper value *)
  type t = [
    | `Lower of elm
    | `Upper of elm
    | `Neg of elm
    | `LowerNeg of elm * elm
    | `LowerUpper of elm * elm
  ]

  (** create a lower bound *)
  val create_lower : elm -> t

  (** creates an upper bound *)
  val create_upper : elm -> t

  (** creates a negative information *)
  val create_neg : elm -> t

  (** create a lower and an upper bound *)
  val create_lowerupper : elm -> elm -> t

  (** create a lower bound and a negative information *)
  val create_lowerneg : elm -> elm -> t

  (** compare to values *)
  val compare : t -> t -> int

  (** unions two LSetInEx.t values *)
  val merge : t -> t -> t

  (** If the upper and lower bound are equal, then 
      the exact value is returned. Otherwise, [None]
      indicates that there are more than one possibilities
      for this binding. 
  *)
  val get_exact : t -> elm option

  (** Returns a string representation of a LSetInEx.t value *)
  val string_of : t -> string

end


module Make : functor(Set: SMALLSET) ->
  S with type elm = Set.t
