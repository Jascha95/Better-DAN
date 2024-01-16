(** Module that provides a generic implemention for variables. *)

exception EUnion of string


(** Module type for the image of the map 
    assigning variables to values. It's nesseary to
    compare the values and to compute a union of
    two value. This is needed in the module, because
    while make to variables aliases we need to union
    the two value of these.
    If it's not possible to union all values, for example
    because there's no top type, you should include a 
    [NotPossible] Value into the img type and deal with 
    this value in your constraints solver. It will normaly 
    mean, that no solution could be found.
*)
module type IMG = 
sig
  (** type of the values *)
  type t

  (** compare to values *)
  val compare : t -> t -> int
  
  (** merge these two images, such that the new image
      fulfills all properties of both old ones. 
  *)
  val merge : t -> t -> t

  (** bring the image into a unique from *)
  val normalize : t -> t

  val string_of : t -> string
end

module type PREFIX = sig
  val prefix : string
end


(** Module type that defines the observer *)
module type OBSERVER =
sig
  (** type of the observer *)
  type t 

  (** comapres two observers *)
  val compare : t -> t -> int

  (** string representation of the observer *)
  val string_of : t -> string

  (** this method is called if the image of the variable
      changes *)
  val fire : t -> unit 
end

module type STATE = 
sig
  type obs 
  val add_wl : obs -> unit
end

(** This module type is the signature of the typevars without
    the image operations. The type is used by the {! MapVar } Module
    to modify the interface to the image. There the two functions
    allowing the modifications of the image are hidden.
*)
module type VAR_WITHOUT_IMG =
sig
  (** The image of the vars are the from type img. Solving the constraints 
      means maps variables to values in a way, that the constraints are
      satisfied. *)

  (** type of the observer *)
  type obs 

  (** Type variables are instances of this type *)
  type t

  (** create a new uniquie type variable *)
  val create : unit -> t

  (** compare type variables. It looks if you have added aliases, and
      if so, the two type variable are equal. *)
  val compare : t -> t -> int

  (** compare type variables. It do not look 
      if you have added aliases! *)
  val total_ord : t -> t -> int

  (** make two variables equal *)
  val add_alias : t -> t -> unit

  (** This function is called by observers every time something
      with the image of the variable changes. The variable should
      then call the normalize function of the image to ensure that
      the assignment is up to date. *)
  val do_normalize : t -> unit

  (** add a new observer *)
  val add_observer : obs -> t -> unit

  (** remove a observer *)
  val remove_observer : obs -> t -> unit

  (** converts a variable to a string *)
  val string_of : t -> string

  (** Resets the internal counter that allows to differ 
      variables from each other. Do not call this function
      exept for testing. If you use it, you will use the
      invariant, that two variables are different if you
      create two with the create function.
  *)
  val reset : unit -> unit    

  val get_all_ts : unit -> t list
end


(** This module type is the signature of the typevars. It is used
    every time where the jsanalse program make use of type variables. *)
module type VAR = sig
  include VAR_WITHOUT_IMG

  (** The image *)
  type img

  (** extends the image of the mapping from type variables to types *)
  val add_img : img -> t -> unit

  (** returns the image *)
  val get_img : t -> img option
end

module type CVAR =
  functor (Obs: OBSERVER) ->
    VAR
  with type obs = Obs.t

(** Signature of the functor *)
module type MAKE =
  functor (Pre: PREFIX) ->
    functor (Obs: OBSERVER) ->
      functor (Img: IMG) -> 
        functor (State: STATE with type obs = Obs.t) ->
          VAR
  with type obs = Obs.t
with type img = Img.t
  
(** Implemenation of the type variables. *)
module Make : MAKE
