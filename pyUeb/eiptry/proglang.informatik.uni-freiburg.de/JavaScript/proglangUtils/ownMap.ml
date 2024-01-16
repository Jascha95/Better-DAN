module type OrderedType = sig
  type t 
  val compare : t -> t -> int
end

module type StringOf = sig
  type t
  val string_of : t -> string
end

module type Custom = sig
  val sep : string
  val mapto : string
end

module type S = sig
  type key
  type img 
  type t 
  val empty : t
  val is_empty : t -> bool
  val add : key -> img -> t -> t
  val find : key -> t -> img
  val remove : key -> t -> t
  val mem : key -> t -> bool
  val iter : (key -> img -> unit) -> t -> unit
  val map : (img -> img) -> t -> t
  val mapi : (key -> img -> img) -> t -> t
  val fold : (key -> img -> 'b -> 'b) -> t -> 'b -> 'b
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val add' : key -> img option -> t -> t
  val find' : key -> t -> img option
  val add_list : (key * img) list -> t -> t
  val to_list : t -> (key * img) list
  val from_list : (key * img) list -> t
  val map_to_list : (key -> img -> 'a) -> t -> 'a list
  val mapi_and_map_to_list : (key -> img -> img * 'a) -> t -> t * 'a list
  val domain : t -> key list
  val fold_two : (key -> img option -> img option -> 'a -> 'a) -> 'a -> t -> t -> 'a
  val restrict : key list -> t -> t
end

module type S_Str = sig
  include S
  val string_of : t -> string
end

module Make : functor (Key: OrderedType) ->
  functor (Img: OrderedType) -> S 
  with type key = Key.t 
  and type img = Img.t
= functor (Key: OrderedType) ->
  functor (Img: OrderedType) ->
struct
  type img = Img.t
  module M = Map.Make(Key)
  type t = img M.t
  type key = M.key

  let empty = M.empty
  let is_empty = M.is_empty
  let add = M.add
  let find = M.find
  let remove = M.remove
  let mem = M.mem
  let iter = M.iter
  let map = M.map
  let mapi = M.mapi
  let fold = M.fold
  let compare = M.compare Img.compare
  let equal = M.equal (fun i1 i2 -> Img.compare i1 i2 == 0)
  let rec add_list el map =
    match el with
        [] -> map
      | (k,v) :: xs -> add_list xs (add k v map)
  let from_list el = add_list el empty
  let to_list m =
    fold 
      (fun k v l -> (k,v) :: l)
      m
      []
  let find' key map =
    try
      Some (find key map)
    with Not_found -> None
  let add' key imgo map =
    match imgo with
      | None -> map
      | Some img -> add key img map

  let domain m =
    fold 
      (fun k v l -> k :: l)
      m
      []

  open ExtList
  let fold_two f a m1 m2 =
    let d1 = domain m1 in
    let d2 = domain m2 in
    let d = List.union d1 d2 in
      List.fold_left 
        (fun a key -> 
           let img1o = find' key m1 in
           let img2o = find' key m2 in
             f key img1o img2o a
        )
        a
        d
      

  let map_to_list f m = 
    fold
      (fun k v l -> f k v :: l)
      m
      []

  let mapi_and_map_to_list f m =
    let l = ref [] in
    let new_map =
      M.mapi
        (fun key img -> 
           let img,a = f key img in
             l := a :: !l;
             img
        )
        m
    in
      new_map, !l

  open ExtUtils
  let restrict kl m =
    fold
      (fun k v new_m -> 
         if (List.mem_cmp 
               (Utils.compare_to_equal Key.compare)
               k
               kl)
         then
           add k v new_m
         else
           new_m
      )
      m
      M.empty
           

end


module AddString : 
    functor (KeyS: StringOf) -> 
      functor (ImgS: StringOf) ->
        functor (SCustom: Custom) -> 
          functor (Map: S with type img = ImgS.t and type key = KeyS.t) ->
            S_Str
  with type key = Map.key
  and type img = Map.img
  =
  functor (KeyS: StringOf) -> 
    functor (ImgS: StringOf) ->
      functor (SCustom: Custom) -> 
        functor (Map: S with type img = ImgS.t and type key = KeyS.t) ->
struct
  include Map
  let string_of map =
    String.concat SCustom.sep 
      (List.map 
          (fun (k,v) -> (KeyS.string_of k) ^ SCustom.mapto ^ (ImgS.string_of v) )
          (to_list map)
      )

end

module AddS : 
    functor (KeyS: StringOf) -> 
      functor (ImgS: StringOf) ->
        functor (Map: S with type img = ImgS.t and type key = KeyS.t) ->
          S_Str
  with type key = Map.key
  and type img = Map.img
  =
  functor (KeyS: StringOf) -> 
    functor (ImgS: StringOf) ->
      functor (Map: S with type img = ImgS.t and type key = KeyS.t) ->
struct
  include AddString(KeyS)(ImgS)(struct let sep = "," let mapto = "->" end)(Map)
end
