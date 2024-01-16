open OwnMap
open ExtList
open ExtUtils

module type S = sig
  include OwnMap.S
  val iter_all : ?rev:bool -> (key -> img -> unit) -> t -> unit
  val map_all : ?rev:bool -> (img -> img) -> t -> t
  val mapi_all : ?rev:bool -> (key -> img -> img) -> t -> t
  val fold_all : ?rev:bool -> (key -> img -> 'b -> 'b) -> t -> 'b -> 'b
  val to_list_all : ?rev:bool -> t -> (key * img) list
  val compare_all : t -> t -> int
  val equal_all : t -> t -> bool
end

module type StringOf = sig 
  type t 
  val string_of : t -> string 
end

module type Custom = sig 
  val sep : string 
  val mapto : string 
end

module type S_Str = sig
  include S
  val string_of : t -> string
end

module Make :
  functor (Key : OrderedType) ->
    functor (Img : OrderedType) -> S
  with type key = Key.t
  and type img = Img.t
= functor (Key : OrderedType) ->
    functor (Img : OrderedType) -> 
struct
  module ImgList = struct
    type t = Img.t list
    let compare = List.compare Img.compare 
  end
  module OM = OwnMap.Make(Key)(ImgList)

  type img = Img.t
  type key = OM.key
  type t = OM.t
  let empty = OM.empty
  let is_empty = OM.is_empty

  let add key img map =
    match OM.find' key map with
      | None -> OM.add key [img] map
      | Some el -> OM.add key (img :: el) map

  let add' key img map =
    match img with
      | None -> map
      | Some img -> add key img map

  let find key map =
    let imgl = OM.find key map in
      match imgl with
        | [] -> raise Not_found
        | i :: _ -> i
  let find' key map =
    try
      Some (find key map)
    with Not_found ->
      None
 
  let remove key map =
    match OM.find' key map with
      | None -> map
      | Some [] -> OM.remove key map
      | Some ([i]) -> OM.remove key map
      | Some (i :: imgl) -> OM.add key imgl map

  let mem key map =
    match find' key map with
      | None -> false
      | Some _ -> true
 
  let iter f map =
    OM.iter 
      (fun key -> function
         | [] -> ()
         | e :: _ -> f key e
      )
      map

  let map f map =
    OM.map
      (function
        | [] -> []
        | e :: el -> f e :: el 
      )
      map

  let mapi f map =
    OM.mapi
      (fun key -> function
         | [] -> []
         | e :: el -> f key e :: el
      )
      map

  let fold f m a =
    OM.fold
      (fun key imgl a ->
         match imgl with
           | [] -> a
           | e :: _ -> f key e a
      )
      m
      a

  let to_list map =
    fold 
      (fun k v l -> (k,v) :: l)
      map
      []
    
  let compare m1 m2 = 
    let l1 = to_list m1 in
    let l2 = to_list m2 in
      List.compare (Utils.compare_2_tup (Key.compare,Img.compare)) l1 l2

  let equal m1 m2 = compare m1 m2 == 0


  let iter_all ?(rev = true) f map =
    OM.iter 
      (fun key imgl -> 
         List.iter 
           (f key) 
           (if rev then (List.rev imgl) else imgl)) 
      map
               
  let map_all ?(rev = true) f map =
    OM.map 
      (fun imgl -> 
         List.map 
           f 
           (if rev then (List.rev imgl) else imgl)) 
      map

  let mapi_all ?(rev = true) f map =
    OM.mapi 
      (fun key imgl -> 
         List.map (f key) 
           (if rev then (List.rev imgl) else imgl)) 
      map

  let fold_all ?(rev = true) f m a =
    OM.fold 
      (fun key imgl a -> 
         List.fold_left 
           (fun a img -> f key img a)
           a
           (if rev then (List.rev imgl) else imgl))
      m
      a

  let compare_all = OM.compare
  let equal_all = OM.equal

  let rec add_list el map =
    match el with
        [] -> map
      | (k,v) :: xs -> add_list xs (add k v map)
  let from_list el = add_list el empty

  let to_list_all ?(rev = true) set =
    fold_all 
      ~rev: rev
      (fun k v l -> (k,v) :: l)
      set
      []

  let domain = OM.domain
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
      OM.mapi
        (fun key imgl -> 
           match imgl with
             | [] -> []
             | img :: imgl ->
                 let img,a = f key img in
                   l := a :: !l;
                   img :: imgl
        )
        m
    in
      new_map, !l

  let restrict = OM.restrict

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


module HMapTest = struct
  open Test

  module HMapSS = Make(String)(String)

  let m = HMapSS.empty
  let cmp_s = (fun s1 s2 -> String.compare s1 s2 == 0)


  let t1 () =
    let m = HMapSS.add "x" "i1" m in
    let i1 = HMapSS.find "x" m in
    let m = HMapSS.add "x" "i2" m in
    let i2 = HMapSS.find "x" m in
    let m = HMapSS.remove "x" m in
    let i1' = HMapSS.find "x" m in
      assert_equal 
        ~cmp: cmp_s
        ~printer: (fun s -> s)
        "i1"
        i1;
      assert_equal 
        ~cmp: cmp_s
        ~printer: (fun s -> s)
        "i1"
        i1';
      assert_equal 
        ~cmp: cmp_s
        ~printer: (fun s-> s)
        "i2"
        i2

  let t2 () =
    let m = HMapSS.add "x" "xi1" m in
    let m = HMapSS.add "y" "yi1" m in
    let m = HMapSS.add "x" "xi2" m in
    let m = HMapSS.add "z" "zi1" m in
    let l = HMapSS.to_list m in
    let l_all = HMapSS.to_list_all m in
    let l_all_r = HMapSS.to_list_all ~rev:false m in
    let s = 
      String_of.string_of_list_complex 
        ~sep:";"
        ~start_char: "["
        ~end_char: "]"
        (fun (s1,s2) -> "(" ^ s1 ^ "," ^ s2 ^ ")")
        l
    in
    let s_all =
      String_of.string_of_list_complex
        ~sep:";"
        ~start_char: "["
        ~end_char: "]"
        (fun (s1,s2) -> "(" ^ s1 ^ "," ^ s2 ^ ")")
        l_all
    in        
    let s_all_r =
      String_of.string_of_list_complex
        ~sep:";"
        ~start_char: "["
        ~end_char: "]"
        (fun (s1,s2) -> "(" ^ s1 ^ "," ^ s2 ^ ")")
        l_all_r
    in        
      assert_equal 
        ~cmp: cmp_s
        ~printer: (fun s -> s)
        "[(z,zi1);(y,yi1);(x,xi2)]"
        s;
      assert_equal 
        ~cmp: cmp_s
        ~printer: (fun s -> s)
        "[(z,zi1);(y,yi1);(x,xi2);(x,xi1)]"
        s_all;
      assert_equal 
        ~cmp: cmp_s
        ~printer: (fun s -> s)
        "[(z,zi1);(y,yi1);(x,xi1);(x,xi2)]"
        s_all_r

  (* TODO: TEST the functions: iter_all, map_all, mapi_all, 
     fold_all, compare_all, equal_all *)

  let _ =
    install_tests "HistoryMap"
      (fun _ -> [("remove test", t1);
                 ("to_list", t2)])

end
