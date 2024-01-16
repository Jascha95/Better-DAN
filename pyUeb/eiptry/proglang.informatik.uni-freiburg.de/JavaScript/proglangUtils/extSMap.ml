module type OrderedType = sig
  type t 
  val compare : t -> t -> int
  val string_of : t -> string
  val sep : string
  val mapto : string
end



module type S = sig
  type key 
  type +'a t 
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val string_of : ('a -> string) -> 'a t -> string
end


module Make : functor (Ord: OrderedType) -> S 
  with type key = Ord.t = functor (Ord: OrderedType) -> 
struct

  include Map.Make(Ord)

  let rec add_list el map =
    match el with
        [] -> map
      | (k,v) :: xs -> add_list xs (add k v map)

  let from_list el = add_list el empty

  let to_list set =
    fold 
      (fun k v l -> (k,v) :: l)
      set
      []

  let string_of string_of_values set =
    String.concat Ord.sep 
      (List.map 
          (fun (k,v) -> (Ord.string_of k) ^ Ord.mapto ^ (string_of_values v) )
          (to_list set)
      )

end 


