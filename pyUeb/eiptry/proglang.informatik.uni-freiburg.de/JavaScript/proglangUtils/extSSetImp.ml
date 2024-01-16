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
  = 
  functor (Ord: OrderedType) -> struct
    module Set = ExtSSet.Make(Ord)

    type t = Set.t ref
    type elt = Set.elt
    let create () = ref Set.empty
    let is_empty t = Set.is_empty !t
    let mem e t = Set.mem e !t
    let add e t = 
      t := Set.add e !t
    let add_list el t =
      t := Set.add_list el !t
    let singleton e =
      ref (Set.singleton e)
    let remove e t =
      t := Set.remove e !t
    let compare t1 t2 =
      Set.compare !t1 !t2
    let equal t1 t2 =
      Set.equal !t1 !t2
    let subset t1 t2 =
      Set.subset !t1 !t2
    let iter f t = Set.iter f !t
    let fold f t a = Set.fold f !t a
    let for_all f t = Set.for_all f !t
    let exists f t = Set.exists f !t
    let filter f t =
      t := Set.filter f !t
    let cardinal t = Set.cardinal !t
    let elements t = Set.elements !t
    let min_elt t = Set.min_elt !t
    let max_elt t = Set.max_elt !t
    let choose t = Set.choose !t
    let from_list el = ref (Set.from_list el)
    let string_of t = Set.string_of !t
    let string_of_complex ?start_char:sc ?end_char:ec t =
      Set.string_of_complex ?start_char:sc ?end_char:ec !t
end
