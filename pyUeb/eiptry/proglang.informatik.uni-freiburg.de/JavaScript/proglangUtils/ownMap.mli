(** This module provides an not polymorphic map implementation.
    It is based on the Map module of the OCaml standard library.
    The main difference is, that the [Make] functor takes two
    arguments, one that represents the domain of the map, 
    and the other that represents the image of the map.
    As result you get an map that is not polymorphic in
    the image. This allows you to restrict the possible values
    that are allowed in the map and possibly as a result of
    this you might get better error reports from OCaml. Of cause
    you lost the flexibility to use one module at different places
    with different images.

    An other advantage is, that you can add [string_of] functions
    to the module, if you needs them. [AddString] allows to specify
    the [mapto] string and the [seperator] string, [AddS] is a simple
    version of the functor that uses [","] and ["->"] for these two
    strings. 

    @author: Phillip Heidegger
*)

(** The ordered type, like in the original Map module *)
module type OrderedType = sig 
  type t 
  val compare : t -> t -> int 
end

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

(** The signature for the map, very similar to the [Map.S] signature (from the
    OCaml standard library). *)
module type S = sig

  (** The domain of the map *)
  type key

  (** The image of the map *)
  type img 

  (** The map itself *)
  type t 

  (** The empty map *)
  val empty : t

  (** Tests if the map is empty or not. *)
  val is_empty : t -> bool

  (** [add x y m] returns a map containing the same bindings as 
      [m], plus a binding of [x] to [y]. If [x] was already 
      bound in [m], its previous binding disappears. *)
  val add : key -> img -> t -> t
 
  (** Returns the image of the given key. Raises [Not_found] if
      the key is not assigned to a value. *)
  val find : key -> t -> img

  (** [remove x m] returns a map containing the same bindings 
      as [m], except for [x] which is unbound in the returned map. *)
  val remove : key -> t -> t

  (** [mem x m] returns [true] if [m] contains a binding for [x], and [false] otherwise. *)
  val mem : key -> t -> bool

  (** [iter f m] applies [f] to all bindings in map [m]. [f] 
      receives the key as first argument, and the associated 
      value as second argument. The bindings are passed to [f]
      in increasing order with respect to the ordering over 
      the type of the keys. Only current bindings are presented 
      to [f]: bindings hidden by more recent bindings are not
      passed to f. *)
  val iter : (key -> img -> unit) -> t -> unit
 
  (** [map f m] returns a map with same domain as [m], where 
      the associated value a of all bindings of [m] has been
      replaced by the result of the application of [f] to [a]. 
      The bindings are passed to f in increasing order with
      respect to the ordering over the type of the keys. *)
  val map : (img -> img) -> t -> t

  (** Same as [S.map], but the function receives as arguments both 
      the key and the associated value for each binding of the map. *)
  val mapi : (key -> img -> img) -> t -> t

  (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)], where 
      [k1 ... kN] are the keys of all bindings in [m] 
      (in increasing order), and [d1 ... dN] are the 
      associated data. *)
  val fold : (key -> img -> 'b -> 'b) -> t -> 'b -> 'b

  (** Total ordering between maps. Here's no need to give a compare function for
      the images like in the standard library, because the image compare function
      is know throw the functor parameter [Img.compare].*)
  val compare : t -> t -> int

  (** [equal m1 m2] tests whether the maps [m1] and [m2] 
      are equal, that is, contain equal keys and associate them 
      with equal data. 
      Here's no need to give a compare function for
      the images like in the standard library, because the image compare function
      is know throw the functor parameter [Img.compare].*)
  val equal : t -> t -> bool

  (** {2 New Functions } *)

  (** [add' x y m] returns [m] if [y = None], and [add x (Some e) m] 
      if [y = Some e]. *)
  val add' : key -> img option -> t -> t

  (** Similar to find, but does not raise an error if no
      image is found. *)
  val find' : key -> t -> img option

  (** add a list of key,img pairs to the map *)
  val add_list : (key * img) list -> t -> t

  (** returns the hole map as list of key,img tuples *)
  val to_list : t -> (key * img) list

  (** creates a new map from a assignment list *)
  val from_list : (key * img) list -> t

  (** [map_to_list f m] returns the list of values that [f] return, if it is
      applied to the [key] and [image] values of the map [m].
  *)
  val map_to_list : (key -> img -> 'a) -> t -> 'a list


  (** [mapi_and_map_to_list f m] returns the list of values that [f] return, if it is
      applied to the [key] and [image] values of the map [m] and allows to change
      the image of the map at the same time.
  *)
  val mapi_and_map_to_list : (key -> img -> img * 'a) -> t -> t * 'a list

  (** Returns the domain of the map as a list of keys *)
  val domain : t -> key list

  (** [fold_two f a m1 m2] fold the two maps [m1] and [m2] together. 
      For all key \in \dom(m1) \cap \dom(m2)
      the function gets two [Some img] parameters, where img is the
      result of [find key m] of the corresponding map. If key is only in one
      of the domains, the image parameter of the other map is set to
      [None].
  *)
  val fold_two : (key -> img option -> img option -> 'a -> 'a) -> 'a -> t -> t -> 'a

  (** [restrict kl m] returns a new map, from which all bindings [x] from [m] are
      removed, if they are not part of the key list [kl]. *)
  val restrict : key list -> t -> t
end

(** The Signature of the map together with the
    string_of conversion function *)
module type S_Str =
sig
  include S
  val string_of : t -> string 
end

(** The module to create a map with a fixed image *)
module Make :
  functor (Key : OrderedType) ->
    functor (Img : OrderedType) -> S
  with type key = Key.t
  and type img = Img.t

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
