(** Allows to create a module that represents lower and upper bounds
    of a solution. The functor takes a set that allows intersection
    ([inter]) and [union], supports the [is_empty] and [subset] operation
    and also allows to [compare] two sets and create a string
    ([string_of]) representation for the set.
*)
open ProglangUtils

module type SS_WO_ME = sig
  type t
  
  (** [is_empty t] should returns [true] if [t] is the empty set, 
      otherwise it returns [false]. *)
  val is_empty : t -> bool

  (** [subset t1 t2] should return [Some true] if [t1] is a subset of [t2].
      If [t1] is no subtype of [t2], then it should returns [Some false]. 
      If the result of the subtype relation is unsure (for example [t1]
      is a type variable that is not assigned to a type, and [t2] is
      a type), then the function should return [None].
  *)
  val subset : t -> t -> bool option

  (** If [inter t1 t2] is called please compute a value [t] such that 
      [subset t t1] and [subset t t2] holds. 
  *)
  val inter : t -> t -> t
  
  (** If [union t1 t2] is called please compute a value [t] such that 
      [subset t1 t] and [subset t2 t] holds. 
  *)
  val union : t -> t -> t

  val compare : t -> t -> int
  val string_of : t -> string

  val normalize : t -> t
end

(** The parameter of the functor. *)
module type SMALLSET = sig
  include SS_WO_ME
  val make_eq_lower : t -> t -> bool * t
  val make_eq_upper : t -> t -> bool * t
  val make_eq_eq : t -> t -> bool * t
end

(** Result of the functor *)
module type S = sig
  
  (** These are the alternatives of a lower-upper value *)
  type 'a d = 
      [ `Lower of 'a
      | `Upper of 'a
      | `LowerUpper of 'a * 'a 
      | `Eq of 'a ]
        
  type elm
  
  (** The type of a LSetInEx value *)
  type t = elm d 

  (** create a lower bound *)
  val create_lower : elm -> t

  (** creates an upper bound *)
  val create_upper : elm -> t

  (** create a lower and an upper bound *)
  val create_lowerupper : elm -> elm -> t

  (** compare to values *)
  val compare : t -> t -> int

  (** merge to sets *)
  val merge : t -> t -> t

  (** If the upper and lower bound are equal, then 
      the exact value is returned. Otherwise, [None]
      indicates that there are more than one possibilities
      for this binding. 
  *)
  val get_exact : t -> elm option

  (** Returns a string representation of a LSetInEx.t value *)
  val string_of : t -> string

  val normalize : t -> t
end

(** Takes the set and creates a module allowing to
    collect lower and upper bounds. *)
module Make : functor(Set: SMALLSET) ->
  S with type elm = Set.t
