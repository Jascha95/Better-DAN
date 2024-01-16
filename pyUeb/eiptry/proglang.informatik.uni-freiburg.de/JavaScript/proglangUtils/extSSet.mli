(** Extends the OCaml Set with a string_of function.
    @author: Phillip Heidegger
*)

(** Module type that extends the Map.OrderedType with some 
    information needed to print the Map. *)
module type OrderedType = sig
  type t
  val compare : t -> t -> int
  val string_of : t -> string
  val sep : string
end


(** Output signature of the functor {!OwnSet.Make}. *)
module type S =
  sig
    type elt
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val add_list : elt list -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val choose' : t -> elt option
    val split : elt -> t -> t * bool * t
    val from_list : elt list -> t
    val string_of : t -> string
    val string_of_complex : ?start_char:string -> ?end_char:string -> t -> string
  end

(** Like Set.Make, but with string_of. *)
module Make : functor (Ord : OrderedType) -> S with type elt = Ord.t
