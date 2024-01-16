open ProglangUtils
open OwnMap


module ModAdd :
  functor (Add : sig type t val merge : t -> t -> t end) ->
    functor (Map : S  with type img = Add.t) 
      -> S
  with type key = Map.key
  and type img = Map.img
= functor(Add: sig type t val merge : t -> t -> t end) ->
  functor(Map : S with type img = Add.t) -> 
struct
  type t = Map.t
  type key = Map.key
  type img = Map.img

  let empty = Map.empty
  let is_empty = Map.is_empty
  let find = Map.find
  let remove = Map.remove
  let mem = Map.mem
  let iter = Map.iter
  let map = Map.map
  let mapi = Map.mapi
  let fold = Map.fold
  let compare = Map.compare
  let equal = Map.equal
  let to_list = Map.to_list
  let find' = Map.find'
  let restrict = Map.restrict
  let fold_two = Map.fold_two
  let domain = Map.domain
  let mapi_and_map_to_list = Map.mapi_and_map_to_list
  let map_to_list = Map.map_to_list
  let add key img t =
    match Map.find' key t with
      | None -> Map.add key img t
      | Some img2 ->
          Map.add key (Add.merge img img2) t

  let add' key imgo map =
    match imgo with
      | None -> map
      | Some img -> add key img map
  let rec add_list el map =
    match el with
        [] -> map
      | (k,v) :: xs -> add_list xs (add k v map)
  let from_list el = add_list el empty

end
