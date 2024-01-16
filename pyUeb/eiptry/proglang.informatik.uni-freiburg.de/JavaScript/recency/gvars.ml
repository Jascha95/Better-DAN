open ProglangUtils
open Location
open LUNVar
open LUVar
open MapVar
open LowerUpper

module type VARS =
sig
 module TVar : sig
   include LUVar.S
   val do_on_exact :
        ?nothing:(unit -> unit)
     -> ?something:(elm -> unit)
     -> t
     -> unit
   val do_on_lower :
        ?nothing:(unit -> unit)
     -> ?something:(elm -> unit)
     -> t
     -> unit
   val do_on_upper :
        ?nothing:(unit -> unit)
     -> ?something:(elm -> unit)
     -> t
     -> unit
 end
 module LSVar : sig
   include LUNVar.S 
   val lower_one_element :
        ?nothing:(unit -> unit)
     -> ?exact:(Loc.t -> unit)
     -> ?tomuch:(unit -> unit)
     -> t
     -> unit
   val lower_iter : (Loc.t -> unit) -> t -> unit
   val upper_iter : (Loc.t -> unit) -> t -> unit
   val is_one_possible : t -> bool
   val equal_possible : t -> t -> bool
   val equal_possible_lset : t -> LSet.t -> bool
   val subset_possible : t -> t -> bool
   val subset_possible_lset : t -> LSet.t -> bool
   val choose_lower : t -> Loc.t option
   val set_upper_for_all : LSet.t -> unit
 end
   with type elm = LSet.t
 module PrVar : sig
   include LUVar.S
   val do_on_lower :
        ?nothing:(unit -> unit)
     -> ?exact:(unit -> unit)
     -> ?inexact:(unit -> unit)
     -> ?nv:(NoValue.nv -> unit)
     -> t
     -> unit
   val do_on_upper :
        ?nothing:(unit -> unit)
     -> ?exact:(unit -> unit)
     -> ?inexact:(unit -> unit)
     -> ?nv:(NoValue.nv -> unit)
     -> t
     -> unit
 end 
 module ObjectType : sig 
   type t 
   type key = Syntax.label
   type img = TVar.t 
 end
 module OTVar : MapVar.S 
   with type I.img = TVar.t
   and type I.key = Syntax.label
 module LEVar : sig
   include MapVar.S
   val synchronize : t -> unit
 end
   with type I.img = OTVar.t
   and type I.key = Loc.t
   and type domain_var = LSVar.t
end

module type TYPE = sig
  include SMALLSET 
  type obs
end

module Make :
  functor (Obs: GenVars.OBSERVER) -> 
    functor (Type: TYPE with type obs = Obs.t) ->
      functor (State: GenVars.STATE with type obs = Obs.t) -> VARS
  with type TVar.elm = Type.t
  and type PrVar.elm = Pre.t
  and type TVar.obs = Obs.t
  and type LSVar.obs = Obs.t
  and type PrVar.obs = Obs.t
  and type OTVar.obs = Obs.t
  and type LEVar.obs = Obs.t
= functor (Obs: GenVars.OBSERVER) -> 
    functor (Type: TYPE with type obs = Obs.t) ->
      functor (State: GenVars.STATE with type obs = Obs.t) ->
struct
  open ExtUtils
  open ExtList

  module LUTypes = struct
    include LowerUpper.Make(Type)
    type obs = Obs.t
(*     let normalize = function *)
(*       | `Lower t -> `Lower (Type.normalize t) *)
(*       | `Upper t -> `Upper (Type.normalize t) *)
(*       | `LowerUpper (t1,t2) -> *)
(*           create_lowerupper  *)
(*             (Type.normalize t1) *)
(*             (Type.normalize t2) *)
(*       | `Eq t -> `Eq (Type.normalize t) *)
  end

  module TVar = struct
    module PrivTVar = GenVars.Make
      (struct let prefix = "tv_" end)(Obs)(LUTypes)(State)
    include LUVar.Make(LUTypes)(PrivTVar)
    let do_on_f 
        ~f:f 
        ?(nothing = fun () -> ()) 
        ?(something = fun _ -> ()) 
        t =
      match f t with 
        | None -> nothing ()
        | Some img -> something img
      
    let do_on_exact = do_on_f ~f:get_exact
    let do_on_lower = do_on_f ~f:get_lower
    let do_on_upper = do_on_f ~f:get_upper

  end
  module LSetInEx = struct
    include LSetInEx
    let normalize t = t
    type obs = Obs.t
  end
  module LSVar = struct
    module PLSVar = LUNVar.Make
      (LSetInEx)
      (GenVars.Make(struct let prefix = "mu_" end)(Obs)(LSetInEx)(State))
    type t = PLSVar.t
    type obs = PLSVar.obs
    type img = PLSVar.img
    type elm = PLSVar.elm
    let ub = ref None
    let get_all_ts = PLSVar.get_all_ts

    let add_alias = PLSVar.add_alias
    let get_lower = PLSVar.get_lower
    let get_upper = PLSVar.get_upper
    let set_lower = PLSVar.set_lower
    let set_upper = PLSVar.set_upper

    let compare = PLSVar.compare
    let total_ord = PLSVar.total_ord
    let do_normalize = PLSVar.do_normalize
    let add_observer = PLSVar.add_observer
    let remove_observer = PLSVar.remove_observer
    let string_of = PLSVar.string_of
    let string_of_with_img = PLSVar.string_of_with_img
    let reset () = 
      ub := None;
      PLSVar.reset ()

    let create () = 
      let var = PLSVar.create () in
        match !ub with
          | None -> var
          | Some ub -> 
              PLSVar.set_upper ub var;
              var
    let get_neg = PLSVar.get_neg
    let disjoint = PLSVar.disjoint
    let set_neg = PLSVar.set_neg
    let get_exact = PLSVar.get_exact
    let string_of_img = PLSVar.string_of_img
    let subset = PLSVar.subset
    let set_eq = PLSVar.set_eq    

    let lower_one_element 
        ?(nothing = fun () -> ()) 
        ?(exact = fun _ -> ())
        ?(tomuch = fun () -> ()) 
        t =
      match get_lower t with
        | None -> nothing ()
        | Some ls ->
            if (LSet.cardinal ls > 1) then
              tomuch ()
            else
              exact (LSet.choose ls)
    let iter_on_option f = function
      | None -> ()
      | Some ls -> LSet.iter f ls

    let lower_iter f t = iter_on_option f (get_lower t)
    let upper_iter f t = iter_on_option f (get_upper t)
    let is_one_possible t =
      let check_lower = function
        | None -> true
        | Some ls -> (LSet.cardinal ls <= 1) 
      in
      let check_upper = function
        | None -> true
        | Some ls -> (LSet.cardinal ls >= 1)
      in
        (check_lower (get_lower t)) && (check_upper (get_upper t))

    let subset_option ls1o ls2o = 
      match ls1o, ls2o with
        | None, _ | _, None -> true
        | Some ls1, Some ls2 -> (LSet.subset ls1 ls2)
            
    let subset_possible t1 t2 = subset_option (get_lower t1) (get_upper t2)
    let subset_possible_lset t1 ls = subset_option (get_lower t1) (Some ls)
    let equal_possible t1 t2 = subset_possible t1 t2 && subset_possible t2 t1
    let equal_possible_lset t ls = 
      (subset_option (get_lower t) (Some ls) &&
         subset_option (Some ls) (get_upper t))

    let choose_lower t =
      match get_lower t with
        | None -> None
        | Some ls -> LSet.choose' ls 

    let set_upper_for_all ls = ub := Some ls;
  end
  module LUPre = struct
    include LowerUpper.Make(struct include Pre let normalize x = x end)
    let normalize t = t
    type obs = Obs.t
    let ask_for_obs _ = []
  end
  module PrVar = struct
    include LUVar.Make(LUPre)
      (GenVars.Make(struct let prefix = "pv_" end)(Obs)(LUPre)(State))

    let do_on_f
        ~get_image:f
        ?(nothing = fun () -> ())
        ?(exact = fun () -> ())
        ?(inexact = fun () -> ())
        ?(nv = fun _ -> ())
        t =
      match f t with
        | None -> nothing ()
        | Some `Ex -> exact ()
        | Some `In -> inexact ()
        | Some (`NVUp as n) | Some (`NVDown as n) ->
            nv n

    let do_on_lower = do_on_f ~get_image:get_lower
    let do_on_upper = do_on_f ~get_image:get_upper 

  end
  module ObjectType = struct
    include OwnMap.AddString
      (struct type t = Syntax.label let string_of = Syntax.string_of_label end)
      (struct type t = TVar.t let string_of = TVar.string_of end)
      (struct let sep = "; " let mapto = " : " end)
      (OwnMap.Make
         (struct
            type t = Syntax.label
            let compare = Syntax.compare_label
          end)
         (TVar))
    let alias tv1 tv2 = TVar.add_alias tv1 tv2
  end
  module OTVar = MapVar.Make
    (struct type t = int let merge _ _ = 0 let create _ = 0 end)
    (ObjectType)
    (struct let prefix = "ov_" end)
    (Obs)
    (State)
  module LocalEnv = OwnMap.AddString
    (struct type t = Loc.t let string_of = Loc.string_of end)
    (struct type t = OTVar.t let string_of = OTVar.string_of end)
    (struct let sep = "; " let mapto = " : " end)
    (OwnMap.Make(Loc)(OTVar))

  module LEVarPriv = MapVar.Make
    (struct 
       type t = LSVar.t 
       let merge lsv1 lsv2 = LSVar.add_alias lsv1 lsv2; lsv1 
       let create = LSVar.create
     end)
    (struct 
       include LocalEnv
       let alias = OTVar.add_alias
     end)
    (struct let prefix = "le_" end)
    (Obs)
    (State)

  module LEVar = struct
    type t = LEVarPriv.t
    type img = LEVarPriv.img
    type obs = LEVarPriv.obs
    type domain_var = LEVarPriv.domain_var
    module I = LEVarPriv.I

    let get_all_ts = LEVarPriv.get_all_ts
    let get_domain_var lev = LEVarPriv.get_domain_var lev
    let create = LEVarPriv.create
(*     let add key img lev =  *)
(*       let lsv = get_domain_var lev in *)
(*         LSVar.set_lower (LSet.singleton key) lsv; *)
(*         LEVarPriv.add key img lev *)

    let find' = LEVarPriv.find'
    let find_or_add ~create:c ~key:k ~map:t = 
      LEVarPriv.find_or_add 
        ~key:k 
        ~create:(
          fun () ->             
            let lsv = get_domain_var t in
              LSVar.set_lower 
                (LSet.singleton k) 
                lsv;            
              c ())
        ~map:t

    let iter = LEVarPriv.iter
    let fold = LEVarPriv.fold
    let compare = LEVarPriv.compare
    let total_ord = LEVarPriv.total_ord
    let add_alias = LEVarPriv.add_alias
    let do_normalize = LEVarPriv.do_normalize
    let add_observer = LEVarPriv.add_observer
    let remove_observer = LEVarPriv.remove_observer
    let make_write_equal = LEVarPriv.make_write_equal
    let string_of_map = LEVarPriv.string_of_map
    let domain = LEVarPriv.domain
    let string_of = LEVarPriv.string_of
    let reset = LEVarPriv.reset

    let synchronize t =
      let ds = LSet.from_list (domain t) in
      let dv = get_domain_var t in
        (* Add the complete lower bound of the dv to the
           map *)
        begin
          match LSVar.get_lower dv with
            | None -> ()
            | Some ls -> 
                let to_add = LSet.diff ls ds in
                  LSet.iter
                    (fun l -> ignore
                       (find_or_add 
                          ~create:OTVar.create 
                          ~key:l 
                          ~map:t))
                    to_add 
        end;
        (* Add the domain map to the lower bound of the dv *)
        LSVar.set_lower ds dv

    let string_of_with_img lev = 
      (LEVarPriv.string_of_with_img lev)
      ^" ("^(LSVar.string_of_with_img (get_domain_var lev))^")"
  end
end
