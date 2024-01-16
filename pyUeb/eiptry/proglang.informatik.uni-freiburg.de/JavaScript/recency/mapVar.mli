(** This module provides a functor that create a 
    mapping variable from a normal variable. 
*)

(** Signature of the functor *)
module type S_WITHOUT_STATE =
  sig
    include GenVars.VAR_WITHOUT_IMG
    type img

    module I : sig 
      type key 
      type img 
    end
      
    (** Returns the domain of the map *)
    val domain : t -> I.key list

    (** [find' k map] returns [Some b] if [map(k) == b].
        If the exists no binding for [k], then [None] 
        is returned.
    *)
    val find' : I.key -> t -> I.img option

    (** [find_or_add ~create:c ~key:k ~map:m] 
        returns [m(k)] if [k] in [domain(m)].
        If [k] is not in the domain of [m], then a new
        image is create with the function [c]. Then
        [m(k)] is set to this new image and the new 
        image is returned.
    *)
    val find_or_add : 
         create:(unit -> I.img) 
      -> key:I.key 
      -> map:t 
      -> I.img

    val iter : (I.key -> I.img -> unit) -> t -> unit

    (** Like a fold operator on a map *)
    val fold : (I.key -> I.img -> 'b -> 'b) -> t -> 'b -> 'b

    (** hm, what happens here? *)
    val make_write_equal : 
         (I.key -> I.key -> int) 
      -> (I.img -> I.img -> unit) 
      -> t -> t -> I.key -> unit

    (** convert the map into a string *)
    val string_of_map : t -> string

    val string_of_with_img : t -> string
  end

module type S = sig
  include S_WITHOUT_STATE
  type domain_var
  val get_domain_var : t -> domain_var
end

module Make :
  functor (DVar: sig type t val create : unit -> t val merge : t -> t -> t end) -> 
    functor (Map: sig include OwnMap.S_Str val alias : img -> img -> unit end) ->
      functor (P : GenVars.PREFIX) ->
        functor (O : GenVars.OBSERVER) ->
          functor (State : GenVars.STATE with type obs = O.t) ->
            S
  with type I.key = Map.key
  and type I.img = Map.img
  and type obs = O.t
  and type domain_var = DVar.t
