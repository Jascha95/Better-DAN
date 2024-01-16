open ProglangUtils
open OwnMap

module ModAdd :
  functor (Add : sig type t val merge : t -> t -> t end) ->
    functor (Map : S  with type img = Add.t) 
      -> S
  with type key = Map.key
  and type img = Map.img
