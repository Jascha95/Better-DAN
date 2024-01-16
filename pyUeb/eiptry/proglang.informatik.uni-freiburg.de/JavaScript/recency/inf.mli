(** Module that provides structurs for doing the type inference *)

open ProglangUtils 
open Location

(** {2 Types of Variables } *)

(** Type variables *)
type tvar

(** Local environment Variables *)
type levar

(** Object type variables *)
type otvar

(** Precise variables *)
type prevar

(** Location Set Variables *)
type lsvar

(** {2 Modules} *)

(** Module that containts the variables *)
module rec Vars : sig
  include Gvars.VARS
  val reset : unit -> unit
end

(** Module that conataints the types *)
and Type : sig
  (** Types and type variables *)
  type t

  (** Types without type variables *)
  type tau

  (** Location environment or local envrionment variable (LEVar) *)
  type lenv
    
  (** object type or object type variable (OTVar) *)
  type r
    
  (** location set or location set variable (LSVar) *)
  type phi
    
  (** Precise information of precise variable (PreVar) *)
  type q

  (** {3 Methods to create values} *)
      
  (** {4 Types } *)
    
  val t_top : t
  val t_undef : t
  val t_int : t
  val create_t_obj : q -> phi -> t
  val create_t_fun : Vars.LEVar.t -> t -> t -> phi -> Vars.LEVar.t -> t -> t
  val create_t_tv : Vars.TVar.t -> t
    
  (** {4 Local Environment } *)
    
  val empty_lenv : lenv
  val add_to_lenv : lenv -> Loc.t -> r -> lenv
  val union_lenv : lenv -> lenv -> lenv
  val create_lenv_levar : levar -> lenv

    
  (** {4 Object Types } *)
    
  val empty_obj : r
  val add_to_obj : r -> Syntax.label -> Vars.TVar.t -> r
  val create_r_otv : Vars.OTVar.t -> r
    
  (** {4 Locations } *)
    
  val create_phi_ls : LSet.t -> phi
  val create_phi_lsvar : Vars.LSVar.t -> phi
    
  (** {4 Precise Informations } *)
    
  val q_in : q
  val q_ex : q
  val create_q_pv : Vars.PrVar.t -> q

  (** {3 output } *)

  val string_of : t -> string
  val string_of_p : (q * phi ) -> string
  val string_of_lenv : lenv -> string
  val string_of_r : r -> string
  val string_of_phi : phi -> string
  val string_of_q : q -> string

end

open Type


(** Module that containts the constraints *)
module ConstBasic : sig

  (** constraint *)
  type t

  type obs
    
  (** {3 Methods to create values } *)
    
  val c_true : t
  val c_false : string option -> t

  (** [create_c_demonation_lenv lev1 mu lev2] creates a constraint that
      changes all precise pointers, for which the location is part of
      mu, into imprecise pointers. It is equivalant with
      [create_c_demonation_lenv_phi lev1 mu (create_phi_ls LSet.empty) lev2].
  *)
  val create_c_demotation_lenv : Vars.LEVar.t -> Type.phi -> Vars.LEVar.t -> t

  (** [create_c_demonation_lenv_ls lev1 mu ls lev2]
      creates a constraint that ensures that the intersection of
      the domain of variables lev1 and ls is empty. *)
  val create_c_demotation_lenv_phi : Vars.LEVar.t -> Type.phi -> Type.phi -> Vars.LEVar.t -> t

  val create_c_demotation_type : Type.t -> Type.phi -> Type.t -> t
  val create_c_include : Loc.t -> phi -> t
  val create_c_exclude : Loc.t -> phi -> t
  val create_c_subset : ?prio:int -> phi -> phi -> t
  val create_c_disjoint : ?prio:int -> phi -> phi -> t
    
  val create_c_subtype : ?prio:int -> Type.t -> Type.t -> t
  val create_c_eqtype : Type.t -> Type.t -> t
  val create_c_eqlenv : lenv -> lenv -> t
  val create_c_eqobj : r -> r -> t
  val create_c_eqset : phi -> phi -> t
  val create_c_eqpres : q -> q -> t

  val create_c_domain_eq : Type.phi -> Vars.LEVar.t -> t
  val create_c_lev_empty_obj : 
    Vars.LEVar.t -> Vars.LEVar.t -> Location.Loc.t -> t


  val create_c_flow : Vars.LEVar.t -> Type.t -> Type.t -> t
  val create_c_read : Vars.LEVar.t -> Vars.PrVar.t -> Vars.LSVar.t -> Syntax.label -> Vars.TVar.t -> t
  val create_c_write : Vars.LEVar.t -> Vars.TVar.t -> Syntax.label -> Vars.TVar.t -> Vars.LEVar.t -> t

  val create_c_locs : Type.phi -> Type.t -> t

  (** {3 Conversion into Strings } *)

  val string_of : t -> string
end

(** Module that contains the global set of basic constraints *)
module Const : sig

  (** Type of the observer *)
  type obs = ConstBasic.obs

  (** [add obs] adds the basic constraint [obs] to the global 
      constraint set.
  *)
  val add : ConstBasic.t -> unit
  

  (** [is_false str] adds the constraint [False (Some s)] to the
      global constraint set and removes all other constraints from
      the set. It also empties the work list.
  *)
  val is_false : string -> unit

  (** [remove obs] removes the basic constraint [obs] from the 
      global constraint set. 
  *)
  val remove : ConstBasic.t -> unit

  (** [add_wl obs] adds the observer [obs] to the working list. This 
      ensures that the [solve] function will visit the observer  
      [obs] if the solving procedure did not deduce an [False] before. 
      If the observer is already part of the work list it is not added a
      second time.
  *)
  val add_wl : obs -> unit

  (** [solve ()] starts the solving of the global constraint set. 
      This means going throw the work list and call for each basic
      constraint the function [fire]. Before the fire function is called
      the basic constraint is removed from the work list. So if a basic
      constraints needs more than one visit it can add them self to
      the list again using [add_wl].
  *)
  val solve : unit ->  unit 

  (** This returns a string representation of the global constraint. The
      string do not contain information about the work list. 
  *)
  val string_of : unit -> string
  
  (** This function resets the global constraint. This results in the
      constraint [true] and an empty work list. 
  *)
  val reset : unit -> unit

  (** Calling of [full_visit] adds all constraints that are part of
      the global constraint set to the work list. 
  *)
  val full_visit : unit -> unit

end


module GEnv : sig
  val string_of : unit -> string
  val reset : unit -> unit
end
