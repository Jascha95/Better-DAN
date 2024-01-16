module type OrderedType = sig
  type t
  val compare : t -> t -> int
  val string_of : t -> string
  val sep : string
end

module type S = sig
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
  val string_of_complex :
    ?start_char:string -> ?end_char:string -> t -> string
end

module Make : functor (Ord: OrderedType) -> S 
  with type elt = Ord.t = functor (Ord: OrderedType) -> 
struct

  include Set.Make(Ord)

  let rec add_list el set =
    match el with
      | [] -> set
      | x :: xs -> add_list xs (add x set)

  let from_list el = add_list el empty
  let string_of_complex ?(start_char = "") ?(end_char = "") set =
    start_char 
    ^ (String.concat Ord.sep (List.map Ord.string_of (elements set)))
    ^ end_char
  let string_of set = string_of_complex set

  let choose' t =
    try
      Some (choose t)
    with Not_found -> None

end 


