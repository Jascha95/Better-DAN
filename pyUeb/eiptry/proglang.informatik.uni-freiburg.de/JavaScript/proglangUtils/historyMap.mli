(** The module to create a map with a fixed image *)

(** The signature is an extension of the signature found in {!OwnMap}.
    That's the case because you can now appy the
    two functors {!AddS} and {!AddString} to the result of
    the {!HistoryMap.Make} functor, too. 
    Please notice that {!fold}, {!map}, {!mapi}, 
    {!iter}, {!to_list} 
    and all functions that deals only with the newest 
    binding. All hidden bindings are ignored. Use the 
    functions with the prefix [_all] to deal with all
    bindings. 
*)
module type S = sig
  include OwnMap.S

  (** [iter_all f m] applies [f] to all bindings in map [m]. [f] receives 
      the key as first argument, and the associated value as second argument. 
      The bindings are passed to [f] in increasing order with respect to the
      ordering over the type of the keys. All bindings are presented to f,
      also bindings hidden by more recent bindings. 

      The hidden bindings follows the binding that hide the bindings process.
      So if we add ["x"] with image ["xi1"], then later adds
      ["x"] with image ["xi2"] that hides the first binding, the 
      tuple [("x","xi2")] will passed to the function before [("x","xi1")].
      You can change this behavior with the optional parameter [rev]. Pass
      [false] here to process the hidden bindings first.
  *)
  val iter_all : ?rev:bool -> (key -> img -> unit) -> t -> unit


  (** [map_all f m] behaves like [map f m], except that also the hidden
      bindings are passed to [f]. 

      The hidden bindings follows the binding that hide the bindings process.
      So if we add ["x"] with image ["xi1"], then later adds
      ["x"] with image ["xi2"] that hides the first binding, the 
      tuple [("x","xi2")] will passed to the function before [("x","xi1")].
      You can change this behavior with the optional parameter [rev]. Pass
      [false] here to process the hidden bindings first.
  *)
  val map_all : ?rev:bool -> (img -> img) -> t -> t

  (** [mapi_all f m] behaves like [mapi f m], except that also the hidden
      bindings are passed to [f]. 
      
      The hidden bindings follows the binding that hide the bindings process.
      So if we add ["x"] with image ["xi1"], then later adds
      ["x"] with image ["xi2"] that hides the first binding, the 
      tuple [("x","xi2")] will passed to the function before [("x","xi1")].
      You can change this behavior with the optional parameter [rev]. Pass
      [false] here to process the hidden bindings first.
  *)  
  val mapi_all : ?rev:bool -> (key -> img -> img) -> t -> t

  (** [fold_all f m] behaves like [fold f m], except that also the hidden
      bindings are passed to [f]. 
      
      The hidden bindings follows the binding that hide the bindings process.
      So if we add ["x"] with image ["xi1"], then later adds
      ["x"] with image ["xi2"] that hides the first binding, the 
      tuple [("x","xi2")] will passed to the function before [("x","xi1")].
      You can change this behavior with the optional parameter [rev]. Pass
      [false] here to process the hidden bindings first.
  *)  
  val fold_all : ?rev:bool -> (key -> img -> 'b -> 'b) -> t -> 'b -> 'b

  (** [to_list_all m] behaves like [to_list m], except that also the
      hidden bindings are part of the result list. 

      The hidden bindings follows the binding that hide the bindings in
      the list. So if we add ["x"] with image ["xi1"], then later adds
      ["x"] with image ["xi2"] that hides the first binding, the 
      tuple [("x","xi2")] appears in front of [("x","xi1")] in the list.
      You can change this behavior with the optional parameter [rev]. Pass
      [false] here to get the hidden bindings first.
      
  *)  
  val to_list_all : ?rev:bool -> t -> (key * img) list

  (** [compare_all m1 m2] compares two maps and respects the 
      full history of the two maps. This function is a total 
      ordering over maps with histories.
  *)
  val compare_all : t -> t -> int

  (** [equal m1 m2] tests whether the maps [m1] and [m2] are equal.
      Here it resprects the full history of the two maps. So the
      hidden bindings of the maps are importend for the result.
  *)
  val equal_all : t -> t -> bool
end

module Make :
  functor (Key : OwnMap.OrderedType) ->
    functor (Img : OwnMap.OrderedType) -> S
  with type key = Key.t
  and type img = Img.t


(** The module signature for types that can 
    be converted into strings. *)
module type StringOf = sig 
  type t 
  val string_of : t -> string 
end

(** Custom string print informations for the map modules *)
module type Custom = sig 
  val sep : string 
  val mapto : string 
end

module type S_Str = sig
  include S
  val string_of : t -> string
end

(** This functor adds string_of function to a map. You
    can specify with string should be used as [separator]
    and [mapsto] string.
*)      
module AddString :
  functor (KeyS : StringOf) ->
    functor (ImgS : StringOf) ->
      functor (SCustom : Custom) ->
        functor (Map : S  with type img = ImgS.t and type key = KeyS.t) 
          -> S_Str
  with type key = Map.key
  and type img = Map.img

(** This functor adds [string_of] function to a map using
    the default [separator] "," and the [mapsto] default "->" 
*)
module AddS : 
    functor (KeyS: StringOf) -> 
      functor (ImgS: StringOf) ->
        functor (Map: S with type img = ImgS.t and type key = KeyS.t) ->
          S_Str
  with type key = Map.key
  and type img = Map.img
