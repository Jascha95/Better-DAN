open ProglangUtils

module type S = sig
  include GenVars.VAR_WITHOUT_IMG
  type img
  type elm
  val set_lower : elm -> t -> unit
  val set_upper : elm -> t -> unit
  val set_eq : elm -> t -> unit
  val subset : t -> t -> unit
  val get_lower : t -> elm option
  val get_upper : t -> elm option
  val string_of_img : t -> string
  val get_exact : t -> elm option
  val string_of_with_img : t -> string
end


module Make :
  functor (LU:  LowerUpper.S) ->
    functor (Var: GenVars.VAR with type img = LU.t) ->  
      S
with type elm = LU.elm
and type obs = Var.obs
and type img = LU.t

= functor (LU:  LowerUpper.S) ->
    functor (Var: GenVars.VAR with type img = LU.t) ->  
struct
  type t = { var: Var.t;
             lower: LU.elm list ref;
             upper: LU.elm list ref
           }
  type img = Var.img
  type obs = Var.obs
  open ExtList
  open LU

  let all_ts = ref []
  let get_all_ts () = !all_ts
  let create () = 
    let v = { var = Var.create (); lower = ref []; upper = ref []} 
    in
      all_ts := v :: !all_ts;
      v
  let reset () = 
    Var.reset ();
    all_ts := []
  let string_of {var = var} = Var.string_of var
  let remove_observer obs {var = var } = Var.remove_observer obs var
  let add_observer obs {var = var } = Var.add_observer obs var
  let do_normalize { var = var } = Var.do_normalize var
  let compare { var = v1 } {var = v2} = Var.compare v1 v2
  let add_alias v1 v2 =
    Var.add_alias v1.var v2.var;
    v1.lower := List.union !(v1.lower) !(v2.lower);
    v1.upper := List.union !(v1.upper) !(v2.upper);
    v2.lower := !(v1.lower);
    v2.upper := !(v1.upper)

  type elm = LU.elm

  let set_lower ls {var = var; lower = lower } = 
    if (not (List.mem ls !lower )) then begin
      lower := ls :: !lower;
      Var.add_img (create_lower ls) var
    end

  let set_upper ls {var = var; upper = upper} = 
    if (not (List.mem ls !upper)) then begin
      upper := ls :: !upper;
      Var.add_img (create_upper ls) var
    end

  let on_lower f {var = v} = match Var.get_img v with
    | Some (`Lower ls) 
    | Some (`LowerUpper (ls,_)) 
    | Some (`Eq ls) -> f ls
    | _ -> ()
  let on_upper f {var = v} = match Var.get_img v with
    | Some (`Upper ls) 
    | Some (`LowerUpper (_,ls)) 
    | Some (`Eq ls) -> f ls
    | _ -> ()
        
  let subset v1 v2 =
    (* v1 s v2 -> add each l in v1.lower to v2.lower *)
    on_lower (fun ls -> set_lower ls v2) v1;
    (* v1 s v2 -> remove all l from v1.upper that are not part of v2.upper *)
    on_upper (fun ls -> set_upper ls v1) v2
 
  let get_lower {var = v} = match Var.get_img v with
    | Some (`Lower ls) 
    | Some (`Eq ls)
    | Some (`LowerUpper (ls,_)) -> Some ls
    | _ -> None
  let get_upper {var = v} = match Var.get_img v with
    | Some (`Upper ls) 
    | Some (`Eq ls)
    | Some (`LowerUpper (_,ls)) -> Some ls
    | _ -> None
        
  let set_eq ls var =
    set_lower ls var;
    set_upper ls var
      
  let string_of_img {var = v}  = 
    let img = Var.get_img v in
      match img with
        | None -> ""
        | Some l -> LU.string_of l

  let string_of_with_img v =
    string_of v ^ ":" ^ string_of_img v

  let get_exact {var = v} =
    match Var.get_img v with
      | None -> None
      | Some ls -> LU.get_exact ls

  let total_ord {var = v1} {var = v2} = Var.total_ord v1 v2
end
