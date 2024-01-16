(** The module provides a functor that allows creating a
    variable module with an observer. Throw the first
    functor parameter the type module is set. *)

open Location
open LowerUpper

(** Constrains variables needed for the inference *)
module type VARS =
sig
 module TVar : sig
   include LUVar.S
   val do_on_exact :
        ?nothing:(unit -> unit)
     -> ?something:(elm -> unit)
     -> t
     -> unit
   val do_on_lower :
        ?nothing:(unit -> unit)
     -> ?something:(elm -> unit)
     -> t
     -> unit
   val do_on_upper :
        ?nothing:(unit -> unit)
     -> ?something:(elm -> unit)
     -> t
     -> unit       
 end
 module LSVar : sig
   include LUNVar.S 
   val lower_one_element :
        ?nothing:(unit -> unit)
     -> ?exact:(Loc.t -> unit)
     -> ?tomuch:(unit -> unit)
     -> t
     -> unit
   val lower_iter : (Loc.t -> unit) -> t -> unit
   val upper_iter : (Loc.t -> unit) -> t -> unit
   val is_one_possible : t -> bool
   val equal_possible : t -> t -> bool
   val equal_possible_lset : t -> LSet.t -> bool
   val subset_possible : t -> t -> bool
   val subset_possible_lset : t -> LSet.t -> bool
   val choose_lower : t -> Loc.t option
   val set_upper_for_all : LSet.t -> unit
 end
   with type elm = LSet.t
  module PrVar : sig
   include LUVar.S
   val do_on_lower :
        ?nothing:(unit -> unit)
     -> ?exact:(unit -> unit)
     -> ?inexact:(unit -> unit)
     -> ?nv:(NoValue.nv -> unit)
     -> t
     -> unit
   val do_on_upper :
        ?nothing:(unit -> unit)
     -> ?exact:(unit -> unit)
     -> ?inexact:(unit -> unit)
     -> ?nv:(NoValue.nv -> unit)
     -> t
     -> unit
 end 
 module ObjectType : sig 
   type t 
   type key = Syntax.label
   type img = TVar.t 
 end
 module OTVar : MapVar.S 
   with type I.img = TVar.t
   and type I.key = Syntax.label
 module LEVar : sig
   include MapVar.S
   val synchronize : t -> unit
 end
   with type I.img = OTVar.t
   and type I.key = Loc.t
   and type domain_var = LSVar.t
 end

module type TYPE = sig
  include SMALLSET 
  type obs
end

(** Functor that creates the variables. Gets a type module
    and an observer module a functor parameters. *)
module Make :
  functor (Obs: GenVars.OBSERVER) -> 
    functor (Type: TYPE with type obs = Obs.t) ->
      functor (State: GenVars.STATE with type obs = Obs.t) -> VARS
  with type TVar.elm = Type.t
  and type PrVar.elm = Pre.t
  and type TVar.obs = Obs.t
  and type LSVar.obs = Obs.t
  and type PrVar.obs = Obs.t
  and type OTVar.obs = Obs.t
  and type LEVar.obs = Obs.t
