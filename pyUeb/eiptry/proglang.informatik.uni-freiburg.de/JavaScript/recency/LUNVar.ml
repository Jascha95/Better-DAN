open LUVar

module type S = sig
  include LUVar.S
  val set_neg : elm -> t -> unit
  val disjoint : t -> t -> unit
  val get_neg : t -> elm option    
end


module Make :
  functor (LUN:  LowerUpperNeg.S) ->
    functor (Var: GenVars.VAR with type img = LUN.t) ->  
      S
  with type elm = LUN.elm
  and type obs = Var.obs
= functor (LUN:  LowerUpperNeg.S) ->
    functor (Var: GenVars.VAR with type img = LUN.t) ->  
struct


  include Var
  open LUN
  type elm = LUN.elm

  let set_lower ls var = add_img (create_lower ls) var
  let set_neg ls var = add_img (create_neg ls) var
  let set_upper ls var = add_img (create_upper ls) var
    
  let on_lower f v = match get_img v with
    | Some (`Lower ls) 
    | Some (`LowerNeg (ls,_)) 
    | Some (`LowerUpper (ls,_)) -> f ls
    | _ -> ()
  let on_upper f v = match get_img v with
    | Some (`Upper ls) | Some (`LowerUpper (_,ls)) -> f ls
    | _ -> ()
  let on_neg f v = match get_img v with
    | Some (`Neg ls) | Some (`LowerNeg (_,ls)) -> f ls
    | _ -> ()        
        
        
  let subset v1 v2 =
    (* v1 s v2 -> add each l in v1.lower to v2.lower *)
    on_lower (fun ls -> set_lower ls v2) v1;
    (* v1 s v2 -> remove all l from v1.upper that are not part of v2.upper *)
    on_upper (fun ls -> set_upper ls v1) v2;
    (* v1 s v2 -> add all l from v2.neg into v1.neg *)
    on_neg (fun ls -> set_neg ls v1) v2
      
  let disjoint v1 v2 =
    on_lower (fun ls -> set_neg ls v2) v1;
    on_lower (fun ls -> set_neg ls v1) v2
      
  let get_lower v = match get_img v with
    | Some (`Lower ls) 
    | Some (`LowerNeg (ls,_)) 
    | Some (`LowerUpper (ls,_)) -> Some ls
    | _ -> None
  let get_upper v = match get_img v with
    | Some (`Upper ls) 
    | Some (`LowerUpper (_,ls)) -> Some ls
    | _ -> None
  let get_neg v = match get_img v with
    | Some (`Neg ls)
    | Some (`LowerNeg (_,ls)) -> Some ls
    | _ -> None
        
        
  let set_eq ls var =
    add_img (create_lowerupper ls ls) var 
      
  let string_of_img lsv  = 
    let img = get_img lsv in
      match img with
        | None -> ""
        | Some l -> LUN.string_of l

  let string_of_with_img lsv =
    Var.string_of lsv ^ ":" ^ string_of_img lsv

  let get_exact lsv = 
    match get_img lsv with
      | None -> None
      | Some ls -> LUN.get_exact ls
end


