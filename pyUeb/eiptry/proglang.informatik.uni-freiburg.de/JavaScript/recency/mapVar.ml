module type MAPMOD = sig 
  type t 
  type key  
  type img  
  type obs
  type state 
  val compare : t -> t -> int 
  val empty : unit -> t 
  val merge : t -> t -> t
  val find' : key -> t -> img option 
  val find_or_add : 
    create:(unit -> img) -> 
    key:key -> 
    map:t -> 
    img
  val domain : t -> key list 
  val iter : (key -> img -> unit) -> t -> unit
  val fold : (key -> img -> 'b -> 'b) -> t -> 'b -> 'b
  val string_of : t -> string
  val get_state : t -> state
  val set_state : state -> t -> unit
end 
module type OBS = sig
  type t
end

module type DSTATE = sig
  type t
  val create : unit -> t
  val merge : t -> t -> t
end

module MapMod : 
  functor (State: DSTATE) -> 
    functor(M: OwnMap.S_Str) -> 
      functor(O : OBS) -> 
        functor (I : sig 
                   type img 
                   val alias : img -> img -> unit 
                 end
                 with type img = M.img) ->
          MAPMOD 
  with type key = M.key 
  and type img = M.img 
  and type obs = O.t
  and type state = State.t
  = functor (State: DSTATE) 
    -> functor (M : OwnMap.S_Str) 
      -> functor (O : OBS)
        -> functor (I : sig 
                      type img 
                      val alias : img -> img -> unit 
                    end
                    with type img = M.img) ->
struct 
  type t = M.t ref * State.t ref
  type key = M.key 
  type img = M.img 
  type obs = O.t
  type state = State.t

  let maybe f o1 o2 =
    match o1,o2 with
      | None, _ | _,None -> None
      | Some o1, Some o2 -> Some (f o1 o2)

  let string_of (m,_) = M.string_of !m
  let compare (m1,_) (m2,_) = M.compare !m1 !m2
  let empty () = (ref M.empty,ref (State.create ()))
  let merge (m1,s1) (m2,s2) =  
    (* TODO: Union of maps has to work conform with the 
       interface of LEVar!  *)
    let m = ref 
      (M.fold 
         (fun key img m -> 
            match M.find' key m with
              | None -> M.add key img m
              | Some i ->
               I.alias i img;
                  m)
         !m1 
         !m2)
    in
    let s = ref (State.merge !s1 !s2) in
      m,s
      
  let add key img (m,s) = m := M.add key img !m
  let iter f (m,_) = M.iter f !m
  let fold f (m,s) a = M.fold f !m a
  let find' key (m,_) = M.find' key !m
  let find_or_add ~create:c ~key:k ~map:t =
    match find' k t with
      | None -> let img = c () in
          add k img t;
          img
      | Some img -> img
  let domain (m,_) = M.domain !m
  let get_state (_,s) = !s
  let set_state s (m,s1) = s1 := s
end 


module type S_WITHOUT_STATE = sig
  include GenVars.VAR_WITHOUT_IMG
  type img
  module I : sig
    type key 
    type img 
  end
  val domain : t -> I.key list
  val find' : I.key -> t -> I.img option

  val find_or_add : create:(unit -> I.img) 
    -> key:I.key -> map:t -> I.img
  val iter : (I.key -> I.img -> unit) -> t -> unit
  val fold : (I.key -> I.img -> 'b -> 'b) -> t -> 'b -> 'b
  val make_write_equal : (I.key -> I.key -> int) 
    -> (I.img -> I.img -> unit) 
    -> t -> t -> I.key -> unit
  val string_of_map : t -> string
  val string_of_with_img : t -> string
end

module type S = sig
  include S_WITHOUT_STATE
  type domain_var
  val get_domain_var : t -> domain_var
end

module Make
  (DState: DSTATE)
  (Map: sig include OwnMap.S_Str val alias : img -> img -> unit end)
  (P: GenVars.PREFIX)
  (O: GenVars.OBSERVER)
  (State : GenVars.STATE with type obs = O.t) =
struct
  module I = struct
    include MapMod(DState)(Map)(O)(
      struct 
        type img = Map.img
        let alias = Map.alias
      end)
    let normalize t = t
    let ask_for_obs _ = [] (* TODO *)
  end
  type domain_var = I.state
  module PrivGVar = GenVars.Make(P)(O)(I)(State)

  type t = PrivGVar.t
  type img = PrivGVar.img
  type obs = PrivGVar.obs
  let create = PrivGVar.create
  let compare = PrivGVar.compare
  let add_alias = PrivGVar.add_alias
  let do_normalize = PrivGVar.do_normalize
  let add_observer = PrivGVar.add_observer
  let remove_observer = PrivGVar.remove_observer
  let string_of = PrivGVar.string_of
  let reset = PrivGVar.reset
  let get_all_ts = PrivGVar.get_all_ts
  let total_ord = PrivGVar.total_ord

  let add_img = PrivGVar.add_img
  let get_img t = 
    match PrivGVar.get_img t with
      | None -> 
          let i = I.empty () in
            add_img i t;
            i
      | Some i -> i

  let get_domain_var t = I.get_state (get_img t)
  let domain t =  I.domain (get_img t)           

  let find' : I.key -> t -> I.img option =  fun l t -> I.find' l (get_img t)

  let find_or_add ~create:c ~key:k ~map:mv = I.find_or_add c k (get_img mv)
      
  let iter f mv = I.iter f (get_img mv)
  let fold f mv a = I.fold f (get_img mv) a
  let string_of_map t = I.string_of (get_img t)

  let make_write_equal key_cmp img_make_alias mv1 mv2 key =
    let meq mv l' img () =
      if (key_cmp l' key == 0) then begin
        ()
      end else begin
        match find' l' mv with
          | None -> 
              ignore 
                (find_or_add 
                   ~create:(fun () -> img) 
                   ~key:l' 
                   ~map:mv)
          | Some img2 -> 
              img_make_alias img img2              
      end
    in
    fold (meq mv1) mv2 ();
    fold (meq mv2) mv1 ()

  let string_of_with_img t =
    string_of t ^ ": " ^ string_of_map t
end
