(** This module provides functions to generate constraints for a
    program. 
*)

open ProglangUtils
open Location
open Inf
open Type

(** Module of the type environment (Gamma) *)
module TEnv : OwnMap.S

(** Module that generates the constraints for 
    expressions, serious expressions and values *)
module Gen : sig
  exception NoLetAnnotation of string

  type result = {
    typevar : Vars.TVar.t;
  }
  type changed = {
    location: Type.phi;
    lenv: Vars.LEVar.t;
    tenv: TEnv.t;
  }

  (** create the constraints for an expression *) 
  val create_e : TEnv.t -> Vars.LEVar.t -> Syntax.e -> result * changed

  (** create the constraints for a serious expression *) 
  val create_s : TEnv.t -> Vars.LEVar.t -> Syntax.s 
    -> result * Type.phi * Vars.LEVar.t

  (** create the constraints for a value *)
  val create_v : TEnv.t -> Vars.LEVar.t -> Syntax.v -> result
end
