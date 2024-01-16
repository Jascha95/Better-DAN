open LUVar

module type S = sig
  include LUVar.S
  val set_neg : elm -> t -> unit
  val disjoint : t -> t -> unit
  val get_neg : t -> elm option    
end


module Make :
  functor (LU:  LowerUpperNeg.S) ->
    functor (Var: GenVars.VAR with type img = LU.t) ->  
      S 
  with type elm = LU.elm
  and type obs = Var.obs
