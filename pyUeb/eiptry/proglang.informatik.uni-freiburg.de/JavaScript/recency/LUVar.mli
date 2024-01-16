module type S = sig
  include GenVars.VAR_WITHOUT_IMG
  type img
  type elm
  val set_lower : elm -> t -> unit
  val set_upper : elm -> t -> unit
  val set_eq : elm -> t -> unit
  val subset : t -> t -> unit
  val get_lower : t -> elm option
  val get_upper : t -> elm option
  val string_of_img : t -> string
  val get_exact : t -> elm option
  val string_of_with_img : t -> string
end


module Make :
  functor (LU:  LowerUpper.S) ->
    functor (Var: GenVars.VAR with type img = LU.t) ->  
      S
  with type elm = LU.elm
  and type obs = Var.obs
  and type img = LU.t
