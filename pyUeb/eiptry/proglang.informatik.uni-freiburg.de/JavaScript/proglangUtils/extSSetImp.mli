open ExtSSet

module type S = sig
  type t 
  type elt
  val create : unit -> t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> unit
  val add_list : elt list -> t -> unit
  val singleton : elt -> t
  val remove : elt -> t -> unit
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> unit
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val max_elt : t -> elt
  val choose : t -> elt
  val from_list : elt list -> t
  val string_of : t -> string
  val string_of_complex : ?start_char:string -> ?end_char:string -> t -> string
end

module Make : functor(Ord: OrderedType) -> S
  with type elt = Ord.t
  and type t = ExtSSet.Make(Ord).t ref
