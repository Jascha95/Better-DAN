open ProglangUtils
open Location
open ExtList
open GenVars

exception Simplify_False of string

module NoDebug = struct
  let pstart s = ()
  let pdebug s = ()
  let pende s = ()
end
module DEBUG = struct
  let i = ref 0

  let pstart s =
    print_endline ((String.make (2 * !i) ' ') ^ "S: " ^ s);
    i := !i + 1

  let pdebug s =
    print_endline ((String.make (2 * !i) ' ') ^ "D: " ^ s)

  let pende s = 
    i := !i - 1;
    print_endline ((String.make (2 * !i) ' ') ^ "E: " ^ s)

end
open NoDebug
open NoValue
open TopBottom

module rec TypeR : sig
  type pre = Pre.t
  type q = [ pre | `PreVar of Vars.PrVar.t ]
  type phi = [
    | `LocSet of LSet.t
    | `LocSetVar of Vars.LSVar.t
  ]
  type easy_types = [
    | tb
    | `Obj of q * phi
    | `Undef
    | `Int
  ]    
  type tau = [
    | easy_types
    | `Fun of Vars.LEVar.t * t * t * phi * Vars.LEVar.t * t 
  ]
  and t = [ 
    | easy_types
    | `Fun of Vars.LEVar.t * t * t * phi * Vars.LEVar.t * t
    | `TypeVar of Vars.TVar.t
  ]
  and lenv = [
    | `LEnv of (Loc.t * r) list
    | `LEnvVar of Vars.LEVar.t
  ]
  and r = [
    | `ObjectType of (Syntax.label * Vars.TVar.t) list
    | `OTVar of Vars.OTVar.t
  ]

  val create_t_undef : unit -> tau
  val create_t_int : unit -> tau
  val create_t_obj : q -> phi -> t
  val create_t_fun : Vars.LEVar.t -> t -> t -> phi -> Vars.LEVar.t -> t -> t
  val create_t_tv : Vars.TVar.t -> t

  val create_t_obj_tau : q -> phi -> tau
  val create_t_fun_tau : Vars.LEVar.t -> t -> t -> phi -> Vars.LEVar.t -> t -> tau

  val create_empty_lenv : unit -> lenv
  val add_to_lenv : lenv -> Loc.t -> r -> lenv
  val union_lenv : lenv -> lenv -> lenv
  val create_lenv_levar : Vars.LEVar.t -> lenv
  val create_empty_obj : unit -> r
  val add_to_obj : r -> Syntax.label -> Vars.TVar.t -> r
  val create_r_otv : Vars.OTVar.t -> r
  val create_phi_ls : LSet.t -> phi
  val create_phi_lsvar : Vars.LSVar.t -> phi
  val create_q_in : unit -> q
  val create_q_ex : unit -> q
  val create_q_pv : Vars.PrVar.t -> q

  val compare : t -> t -> int
  val compare_tau : tau -> tau -> int
  val compare_q : q -> q -> int

  val string_of : t -> string
  val string_of_tau : tau -> string
  val string_of_lenv : lenv -> string
  val string_of_r : r -> string
  val string_of_p : q * phi -> string
  val string_of_phi : phi -> string
  val string_of_q : q -> string


  val union_tau : tau -> tau -> tau
  val inter_tau : tau -> tau -> tau
  val subtype : t -> t -> bool option
  val subtype_tau : tau -> tau -> bool option
  val flow : Vars.LEVar.t -> t -> t -> bool option
  val flow_obj : Vars.LEVar.t -> Vars.OTVar.t -> Vars.OTVar.t -> bool option


  val normalize : t -> t
  val normalize_tau : tau -> tau
  val make_eq_eq_tau : tau -> tau -> bool * tau
  val make_eq_lower_tau : tau -> tau -> bool * tau
  val make_eq_upper_tau : tau -> tau -> bool * tau

  val add_obs : ConstBasic.obs -> tau -> unit
  val remove_obs : ConstBasic.obs -> tau -> unit
  val demote_tau : phi -> tau -> tau option
  val get_locs : t -> LSet.t

  val get_lower_phi : phi -> LSet.t option
  val get_upper_phi : phi -> LSet.t option
  val is_demotation_tau : tau -> LSet.t -> LSet.t -> tau -> bool option
end 
= struct
  open Vars
  open NoValue
  open TopBottom

  type obs = ConstBasic.obs
  type pre = Pre.t
  type q = [ pre | `PreVar of PrVar.t ]
  type phi = [
    | `LocSet of LSet.t
    | `LocSetVar of LSVar.t
  ]
  type r = [
    | `ObjectType of (Syntax.label * Vars.TVar.t) list
    | `OTVar of OTVar.t
  ]
  type lenv = [
    | `LEnv of (Loc.t * r) list
    | `LEnvVar of LEVar.t
  ]

  type easy_types = [
    | tb
    | `Obj of q * phi
    | `Undef
    | `Int
  ]    
  type tau = [
    | easy_types
    | `Fun of Vars.LEVar.t * t * t * phi * Vars.LEVar.t * t 
  ]
  and t = [ 
    | easy_types
    | `Fun of Vars.LEVar.t * t * t * phi * Vars.LEVar.t * t
    | `TypeVar of Vars.TVar.t
  ]


  let create_t_undef () = `Undef
  let create_t_int () = `Int
  let create_t_obj q phi  = `Obj (q,phi)
  let create_t_obj_tau q phi = `Obj (q,phi)
  let create_t_fun le1 t1 t2 phi le2 t3 = `Fun (le1,t1,t2,phi,le2,t3)
  let create_t_fun_tau le1 t1 t2 phi le2 t3 = `Fun (le1,t1,t2,phi,le2,t3)
  let create_t_tv tv = `TypeVar tv

  let create_p_inexact ls = (`In, `LocSet ls)
  let create_p_exact l = (`Ex, `LocSet (LSet.singleton l))
  let create_p_var q phi = (q,phi)
    
  let create_empty_lenv () = `LEnv []
  let add_to_lenv lenv label r =
    match lenv with
      | `LEnv ls -> `LEnv (List.add (label,r) ls)
      | `LEnvVar _ -> failwith "adding bindings to variable not possible"
          
  let union_lenv l1 l2 = 
    match l1,l2 with
      | `LEnv l1, `LEnv l2 -> 
          `LEnv (List.union l1 l2)
      | _ -> failwith "Union is only allowed if you give two maps as parameter"
  let create_lenv_levar : LEVar.t -> lenv = fun levar ->
    `LEnvVar levar

          

  let create_empty_obj () = `ObjectType []
  let add_to_obj obj label tv =
    match obj with
      | `ObjectType o -> 
          if List.mem_assoc label o then
            `ObjectType o
          else
            `ObjectType (List.add (label,tv) o)
      | `OTVar otv -> 
          (* TODO: If there is already an otv,
             raise an error? *)
          let _ = OTVar.find_or_add 
            ~create:(fun () -> tv)
            ~key:label
            ~map:otv
          in
            `OTVar otv
                


  let create_r_otv otv = `OTVar otv
          
  let create_phi_ls ls = `LocSet ls
  let create_phi_lsvar lsv = `LocSetVar lsv
    
  let create_q_in () = `In
  let create_q_ex () = `Ex
  let create_q_pv pv = `PreVar pv

  let compare_tau = Pervasives.compare
  let compare_q = Pervasives.compare
  let compare : t -> t -> int = fun t1 t2 -> match t1,t2 with
    | `TypeVar tv1, `TypeVar tv2 -> TVar.total_ord tv1 tv2
    | #tau as t1, (#tau as t2) -> compare_tau t1 t2
    | _ -> Pervasives.compare t1 t2
  let string_of_label l = l

  let rec string_of = function 
    | #tau as tau -> string_of_tau tau
    | `TypeVar tv -> TVar.string_of tv
  and string_of_et = function
    | #TopBottom.tb as tb -> TopBottom.string_of tb
    | `Obj p -> "obj(" ^ string_of_p p ^ ")"
    | `Undef -> "undefined"
    | `Int -> "int"
  and string_of_tau = function
    | #easy_types as et -> string_of_et et
    | `Fun (lev,t1,t2,phi,lev2,t3) ->
        "(" ^ LEVar.string_of lev ^ "," 
        ^ (string_of t1) ^ " x " ^ (string_of t2) 
        ^ ")-"^(string_of_phi phi)^"->(" ^ LEVar.string_of lev2 ^ "," 
        ^ string_of t3 ^ ")"
  and string_of_p = function
    | (q,phi) -> string_of_q q ^ string_of_phi phi
  and string_of_lenv = function
    | `LEnv lrl -> 
        "[" 
        ^ (String.concat "," 
             (List.map 
                (fun (l,r) -> Loc.string_of l ^ "->" ^ string_of_r r) 
                lrl)) 
        ^ "]"
    | `LEnvVar lv -> LEVar.string_of lv
  and string_of_r = function
    | `ObjectType ltvl ->
        "{" ^
          (String.concat ";"
             (List.map
                (fun (l,tv) -> Syntax.string_of_label l ^ ":" 
                   ^ TVar.string_of tv)
                ltvl
             ))
        ^ "}"
    | `OTVar ov -> OTVar.string_of ov
  and string_of_phi = function 
    | `LocSet ls -> LSet.string_of_complex ~start_char:"{" ~end_char:"}" ls
    | `LocSetVar lsv -> LSVar.string_of lsv
  and string_of_q = function
    | #pre as p -> Pre.string_of p
    | `PreVar pv -> PrVar.string_of pv


  let check_false cond get_lower get_upper subset v1v v2v =
    match get_lower v1v, get_upper v2v with
      | Some v1, Some v2 -> begin
          match subset v1 v2 with
            | Some false -> Some false
            | _ -> cond v1v v2v
        end
      | _ -> cond v1v v2v

  let check_true cond get_lower get_upper subset v1v v2v =
    match get_upper v1v, get_lower v2v with
      | Some v1, Some v2 -> begin
          match subset v1 v2 with
            | Some true -> Some true
            | _ -> begin
                cond v1v v2v
              end
        end
      | _ -> cond v1v v2v
      
  let check_both get_lower get_upper subset v1 v2 =
    check_false 
      (check_true (fun _ _ -> None) get_lower get_upper subset)
      get_lower
      get_upper
      subset
      v1 v2

  open ExtUtils
  open ExtList
  

  let subtype_tv, subtype_otv, subtype_tau, subtype, subtype_q, subtype_phi =
    let tvtvl = ref [] in
    let otvotvl = ref [] in

      
    let rec subtype_tv tv1 tv2 = 
      if ((TVar.compare tv1 tv2 == 0) 
          or (List.mem (tv1,tv2) !tvtvl)) 
      then begin
        Some true
      end else begin
        tvtvl := (tv1,tv2) :: !tvtvl;
        check_both TVar.get_lower TVar.get_upper subtype_tau tv1 tv2
      end        
    and subtype_otv otv1 otv2 = 
      if ((OTVar.compare otv1 otv2 == 0) 
          or (List.mem (otv1,otv2) !otvotvl)) 
      then begin
        Some true
      end else begin 
        let dm1 = OTVar.domain otv1 in
        let dm2 = OTVar.domain otv2 in
          if (List.subset dm2 dm1) then begin
            (* The domains dm1 is a super set of dm2. So if 
               forall label \in dom(otv1) : otv1(label) <: otv2(label)
               then dom(otv1) = dom(otv2)
            *)
            List.maybe_forall 
              (fun label -> 
                 match (OTVar.find' label otv1, OTVar.find' label otv2) with
                   | Some i1, Some i2 -> subtype_tv i1 i2
                   | _ -> Some false)
              dm1
          end else begin
            Some false
          end  
      end

    and subtype_et t1 t2 = match t1,t2 with
      | `Top, `Top | _, `Top | `Bottom, `Bottom | `Bottom, _  -> Some true
      | `Top, _ -> Some false
      | _, `Bottom -> Some false
      | `Undef, `Undef -> Some true
      | `Undef, _ | _, `Undef -> Some false
      | `Obj (q1,phi1), `Obj (q2,phi2) -> 
          Option.bind
            (subtype_q q1)
            (subtype_phi phi1)
            q2
            phi2
      | `Obj _, _ | _, `Obj _ -> Some false
      | `Int, `Int -> Some true
      | `Int, _ | _, `Int -> Some false
      | _ -> failwith "Do not call function with these parameters (subtype_et)"
    and subtype_tau t1 t2 = match t1, t2 with
      | #easy_types as e1, (#tau as t2) -> subtype_et e1 t2
      | #tau as t1, (#easy_types as e2) -> subtype_et t1 e2
      | `Fun (le2_1,t0_1,t2_1,ls,le1,t1), 
          `Fun (le2_2,t0_2,t2_2,ls',le1',t1') ->
          begin
            let cmp = Utils.compare_to_equal 
              (Utils.compare_2_tup (compare_tau,compare_tau)) 
            in  
            let dm1l = lazy (LEVar.domain le1) in
            let dm2l = lazy (LEVar.domain le1') in
              Option.oands 
                (* TODO: if variables are no equal then maybe we can
                   return Some false, if the bounds allows us to 
                   deduce, that the images of the two variables
                   will not be the same after the complete inference.
                *)
                [(if (LEVar.compare le2_1 le2_2 == 0) then Some true else None);
                 (if (compare_tau t0_1 t0_2 == 0) then Some true else None);
                 (if (compare_tau t2_1 t2_2 == 0) then Some true else None);
                 begin
                   List.maybe_forall 
                     (fun loc -> 
                        match (LEVar.find' loc le1, LEVar.find' loc le1') with
                          | Some i1, Some i2 -> subtype_otv i1 i2
                          | _ -> Some false)
                     (Lazy.force dm1l)
                 end]
          end
      (* | `Fun _, _ | _, `Fun _ -> Some false *)
    and subtype : t -> t -> bool option = fun t1 t2 -> 
      match (t1,t2) with
        | _, `Top | `Bottom, _ -> Some true
        | (#tau as t1), (#tau as t2) -> subtype_tau t1 t2
        | `TypeVar tv1, `TypeVar tv2 -> subtype_tv tv1 tv2
            
        (* TODO: add all TypeVar,* and *,TypeVar cases here 
           The lower and upper bounds may help ansering the
           questions.
           On doubt we have to return None.
        *)
        | `TypeVar tv, _ -> None
        | _, `TypeVar tv -> None
    and subtype_q = fun q1 q2 -> match q1,q2 with
      | #pre as p1, (#pre as p2) -> Pre.subset p1 p2
      | `PreVar pv1, `PreVar pv2 ->
          if (PrVar.compare pv1 pv2 == 0) then begin
            Some true
          end else begin
            check_both PrVar.get_lower PrVar.get_upper Pre.subset pv1 pv2
          end
      | `PreVar pv1, (#pre as p2) ->
          check_both 
            fst
            snd
            Pre.subset 
            (PrVar.get_lower pv1, PrVar.get_upper pv1) 
            (Some p2, Some p2)
      | #pre as p1, `PreVar pv2 -> 
          check_both 
            fst
            snd
            Pre.subset 
            (Some p1, Some p1)
            (PrVar.get_lower pv2, PrVar.get_upper pv2) 

    and subtype_q_option = fun q1o q2o -> match q1o,q2o with
      | Some q1, Some q2 -> subtype_q q1 q2
      | _ -> None
    and subtype_phi = fun phi1 phi2 -> match phi1,phi2 with
      | `LocSet ls1, `LocSet ls2 ->
          Some (LSet.subset ls1 ls2)
      | `LocSetVar lsv, `LocSet ls ->
          check_both
            fst
            snd
            (fun ls1 ls2 -> Some (LSet.subset ls1 ls2))
            (LSVar.get_lower lsv, LSVar.get_upper lsv)
            (Some ls,Some ls)
      | `LocSet ls, `LocSetVar lsv ->
          check_both
            fst
            snd
            (fun ls1 ls2 -> Some (LSet.subset ls1 ls2))
            (Some ls,Some ls)
            (LSVar.get_lower lsv, LSVar.get_upper lsv)
      | `LocSetVar lsv1, `LocSetVar lsv2 -> 
          check_both 
            LSVar.get_lower 
            LSVar.get_upper 
            (fun ls1 ls2 -> Some (LSet.subset ls1 ls2))
            lsv1 
            lsv2
    in
      subtype_tv, subtype_otv, subtype_tau, subtype, subtype_q, subtype_phi



  let (union_tau : tau -> tau -> tau) =
    (* Here we life in our own world, so let's try a little bit *)
    (* Everything is allowed, of cause throwing errors, too. *)

    let union_q : q -> q -> q = fun q1 q2 -> match q1, q2 with
      | #pre as pre1, (#pre as pre2) -> (Pre.union pre1 pre2 :> q)
      | #pre as pre, `PreVar pv 
      | `PreVar pv, (#pre as pre) ->
          begin
            match subtype_q pre (`PreVar pv) with
              | Some true -> `PreVar pv
              | None -> begin
                  match subtype_q (`PreVar pv) pre with
                    | Some true -> pre
                    | None -> 
                        let pv' = Vars.PrVar.create () in
                        let _ = Vars.PrVar.set_lower pre pv in
                        let bc = ConstBasic.create_c_subpre pv pv' in
                        let _ = Const.add_both bc in
                          `PreVar pv'
                    | Some false -> `NVUp
                end
              | Some false ->
                  `NVUp
          end
      | `PreVar pv1, `PreVar pv2 -> 
          begin
            match subtype_q (`PreVar pv1) (`PreVar pv2) with
              | Some true -> `PreVar pv1
              | None -> 
                  let pv' = Vars.PrVar.create () in
                  let bc1 = ConstBasic.create_c_subpre pv1 pv' in
                  let bc2 = ConstBasic.create_c_subpre pv2 pv' in
                  let _ = Const.add_both bc1 in
                  let _ = Const.add_both bc2 in
                    `PreVar pv'
              | Some false -> `NVUp
          end
    in
    let create_union_phi : phi -> phi -> phi = fun phi1 phi2 ->
      let phi = `LocSetVar (Vars.LSVar.create ()) in
      let bc1 = ConstBasic.create_c_subset phi1 phi in
      let bc2 = ConstBasic.create_c_subset phi2 phi in
      let _ = Const.add_both bc1 in
      let _ = Const.add_both bc2 in
        phi
    in      
    let union_phi : phi -> phi -> phi = fun phi1 phi2 -> match (phi1,phi2) with
        (* This is easy, just add some constraints and we are happy. We will
           always find a union on these values. Only if the location variables
           has bad upper bound then we maybe get in trouble. But this will 
           be solved by the constraint normalization steps. So don't think
           about this here. 
        *)
      | `LocSet ls1, `LocSet ls2 ->
          `LocSet (LSet.union ls1 ls2)
      | `LocSet _, `LocSetVar _
      | `LocSetVar _, `LocSet _
      | `LocSetVar _, `LocSetVar _ ->
          begin
            match subtype_phi phi1 phi2 with
              | Some true -> phi2
              | _ -> begin
                  match subtype_phi phi2 phi1 with
                    | Some true -> phi1
                    | _ -> create_union_phi phi1 phi2
                end
          end
    in
    let union_p p1 p2 = match (p1,p2) with
        (* Hm, just call the helps and hope they can do their work. *)
      | (q1,phi1), (q2,phi2) -> union_q q1 q2,union_phi phi1 phi2
    in
    let union_et : ([> easy_types ] as 'a) -> 'a -> 'a = 
      fun t1 t2 -> match (t1,t2) with
        | #tb as t1, t2 -> union_tb t1 t2
        | t1, (#tb as t2) -> union_tb t1 t2
        | `Undef, `Undef -> `Undef (* To same values, no problem *)
        | `Int, `Int -> `Int
        | (`Obj p1, `Obj p2) -> 
            begin
              try
                let p = union_p p1 p2 in
                  `Obj p
              with (EUnion _) -> 
                (* Seems this is to complicated. So approximate it with 'Top *)
                `Top
            end
        | `Undef, _ | _, `Undef
        | `Int, _ | _, `Int 
        | (`Obj _, _) | (_, `Obj _) -> `Top
        | _ -> failwith "methode (union_et) was called with not allowed parameters"
    in
    let union_tau : tau -> tau -> tau = fun t1 t2 -> match (t1,t2) with
      | #easy_types as e1, (#tau as t2) -> (union_et e1 t2 :> tau)
      | #tau as t1, (#easy_types as e2) -> (union_et t1 e2 :> tau)
      | `Fun (lev1,t0,t1,mu,lev2,t2) , 
          `Fun (lev1',t0',t1',mu',lev2',t2') -> 
          begin
            if (List.subset (Vars.LEVar.domain lev1) 
                  (Vars.LEVar.domain lev2)) then begin
              (* TODO: create ne lev, create a constraint ensuring
                 that the new lev includes both, lev1,lev1' and 
                 if these two variables aren't equivalent, the new
                 one is assigned to `Top. This the later will result
                 in the function type to become `Top. 
              *)
              LEVar.add_alias lev1 lev1';
              let alpha_r = TVar.create () in
              let t_r = TypeR.create_t_tv alpha_r in
              let lev_r = LEVar.create () in
              let mu_r = TypeR.create_phi_lsvar (LSVar.create ()) in
              let bcl = 
                [ConstBasic.create_c_subtype t2 t_r;
                 ConstBasic.create_c_subtype t2' t_r;
                 ConstBasic.create_c_subset mu mu_r;
                 ConstBasic.create_c_subset mu' mu_r;
                 ConstBasic.create_c_subenv lev_r lev2;
                 ConstBasic.create_c_subenv lev_r lev2']
              in
                List.iter (fun bc -> Const.add_both bc) bcl;
                `Fun (lev1',t0',t1',mu_r,lev_r,t_r)
            end else begin
              `Top
            end
          end
    in
      union_tau

  let rec inter_tau t1 t2 =
    match (t1,t2) with
      | #easy_types as t1, t2 -> (inter_et t1 t2 :> tau)
      | t1, (#easy_types as t2) -> (inter_et t1 t2 :> tau)

      | `Fun (lev1,t0,t1,mu,lev2,t2), `Fun (lev1',t0',t1',mu',lev2',t2' ) -> 
          begin
            if (List.subset (Vars.LEVar.domain lev1) 
                  (Vars.LEVar.domain lev2)) 
            then begin
              (* TODO: create a new lev, create a constraint ensuring
                 that the new lev includes both, lev1,lev1' and 
                 if these two variables aren't equivalent, the new
                 one is assigned to `Bottom. This will result
                 in the function type to become `Bottom. 
              *)
              LEVar.add_alias lev1 lev1';
              let bc1 = ConstBasic.create_c_subtype t2' t2 in
              let bc2 = ConstBasic.create_c_subset mu' mu in
              let bc3 = ConstBasic.create_c_subenv lev2' lev2 in
                Const.add_both bc1;
                Const.add_both bc2;
                Const.add_both bc3;
                `Fun (lev1',t0',t1',mu',lev2',t2')
            end else begin
              `Bottom
            end
          end
      (* | `Fun _, _ | _, `Fun _ -> `Bottom *)
      (* | _ -> failwith "Do not call the function with these parameters (inter_tau)" *)
  and create_inter_phi : phi -> phi -> phi = fun phi1 phi2 ->
    let lsv' = create_phi_lsvar (LSVar.create ()) in
    let bc1 = ConstBasic.create_c_subset lsv' phi1 in
    let bc2 = ConstBasic.create_c_subset lsv' phi2 in
      Const.add_both bc1;
      Const.add_both bc2;
      lsv'
    
  and inter_phi phi1 phi2 = match phi1,phi2 with
    | `LocSet ls1, `LocSet ls2 -> `LocSet (LSet.inter ls1 ls2)
    | `LocSetVar _, `LocSet _
    | `LocSet _, `LocSetVar _
    | `LocSetVar _, `LocSetVar _ ->
        begin
          match subtype_phi phi1 phi2 with
            | Some true -> phi1
            | _ -> begin
                match subtype_phi phi2 phi1 with
                  | Some true -> phi2
                  | _ -> create_inter_phi phi1 phi2
              end
        end
  and inter_q q1 q2 = match q1,q2 with
    | #pre as q1, (#pre as q2) -> (Pre.inter q1 q2 :> q)
    | #pre as q, `PreVar pv 
    | `PreVar pv, (#pre as q) ->

        let pv' = Vars.PrVar.create () in
          Vars.PrVar.set_upper q pv;
          let bc = ConstBasic.create_c_subpre pv' pv in
            Const.add_both bc;
            `PreVar pv'       
    | `PreVar pv1, `PreVar pv2 ->
        let pv = Vars.PrVar.create () in
        let bc1 = ConstBasic.create_c_subpre pv pv1 in
        let bc2 = ConstBasic.create_c_subpre pv pv2 in
          Const.add_both bc1;
          Const.add_both bc2;
          `PreVar pv
  and inter_et e1 e2 = match e1,e2 with
    | #tb as t1, t2 -> inter_tb t1 t2
    | t1, (#tb as t2) -> inter_tb t1 t2
    | `Obj (pre1,lsv1), `Obj (pre2,lsv2) -> 
        begin
          `Obj (inter_q pre1 pre2, inter_phi lsv1 lsv2)
        end
    | `Undef, `Undef -> `Undef
    | `Int, `Int -> `Int
    | _ -> `Bottom        

  let rec normalize_tau : tau -> tau = function
    | `Obj (`NVUp, _) -> `Top
    | `Obj (`NVDown, _) -> `Bottom
    | `Obj (`PreVar pv, phi) as t ->
        let phi = normalize_phi phi in
          begin
            match PrVar.get_exact pv with
              | None -> begin
                  match PrVar.get_lower pv with
                    | Some `NVUp -> `Top
                    | Some (`Ex| `In | `NVDown as lb) ->
                        begin
                          match PrVar.get_upper pv with
                            | Some `NVDown -> `Bottom
                            | Some `NVUp -> t
                            | Some (`Ex | `In as ub) -> 
                                if (Pre.compare lb ub == 0) then
                                  (`Obj (lb,phi) :> tau)
                                else
                                  t
                            | None -> t
                        end
                    | None -> t
                end
              | Some p -> (`Obj (p,phi) :> tau)
          end
    | `Obj (q,phi) ->
        `Obj (q,normalize_phi phi)
    | `Fun (lev1,t1,t2,phi,lev2,t3) ->
        let t1' = normalize t1 in
        let t2' = normalize t2 in
        let t3' = normalize t3 in
          (* TODO: check local env. to be `NVUp or `NVDown, if
             the image later will contain these values.
          *)
          `Fun(lev1,t1',t2',phi,lev2,t3')
    | t -> t
  and normalize_phi = function
    | `LocSetVar lsv -> 
        begin
          match LSVar.get_exact lsv with
            | None -> `LocSetVar lsv
            | Some ls -> `LocSet ls
        end
    | ls -> ls

  and normalize = function
    | #tau as t -> (normalize_tau t :> t)
    | `TypeVar tv -> begin
        match TVar.get_exact tv with
          | None -> begin
              match TVar.get_lower tv with
                | Some `Top -> `Top
                | Some (#tau as lb) -> begin
                    let lb = normalize_tau lb in
                      match TVar.get_upper tv with
                        | Some `Bottom -> `Bottom
                        | Some (#tau as ub) -> 
                            let ub = normalize_tau ub in
                              if (compare_tau lb ub == 0) then
                                (lb :> t)
                              else
                                `TypeVar tv
                        | None -> `TypeVar tv
                  end
                | None -> `TypeVar tv
            end
          | Some t -> (normalize_tau t :> t)
      end



(*    let ask_for_obs t =   *)
(*      let rec aux : 'a list -> ('a list -> 'a list) list -> 'a list =  *)
(*        fun acc -> function  *)
(*          | [] -> acc  *)
(*          | f :: fs -> aux (f acc) fs  *)
(*      in  *)
(*      let rec obs_t : ConstBasic.obs list -> t -> ConstBasic.obs list =  *)
(*        fun acc -> function  *)
(*          | #tb | `Undef | `Int -> acc  *)
(*          | `TypeVar tv -> `VTVar tv :: acc  *)
(*          | `Fun (lev,t1,t2,phi,lev2,t3) ->  *)
(*              aux   *)
(*                (`VLEVar lev :: `VLEVar lev2 :: acc)  *)
(*                [(fun acc -> obs_phi acc phi);  *)
(*                 (fun acc -> obs_t acc t1);   *)
(*                 (fun acc -> obs_t acc t2);  *)
(*                 (fun acc -> obs_t acc t3)]  *)
(*          | `Obj (q,phi) ->  *)
(*              aux *)
(*                acc *)
(*                [(fun acc -> obs_phi acc phi); *)
(*                 (fun acc -> obs_q acc q)] *)
(*      and obs_phi : ConstBasic.obs list -> phi -> ConstBasic.obs list =  *)
(*        fun acc -> function  *)
(*          | `LocSetVar lsv -> `VLSetVar lsv :: acc  *)
(*          | `LocSet _ -> acc  *)
(*      and obs_q : ConstBasic.obs list -> q -> ConstBasic.obs list = *)
(*        fun acc -> function *)
(*          | `PreVar pv -> `VPreVar pv :: acc *)
(*          | _ -> acc *)
(*      in  *)
(*      let r = obs_t [] (t :> t) in *)
(*        print_endline "Bla"; *)
(*        List.map *)
(*          (fun obs -> print_endline (ConstBasic.string_of_obs obs)) *)
(*          r; *)
(*        r *)

  let change_obs ftv flsv fpv flev obs tau = 
    let rec obs_t : t -> unit = function
      | #tb | `Undef | `Int -> ()
      | `Obj (q,phi) ->
          obs_q q;
          obs_phi phi
      | `Fun (lev1,t1,t2,phi,lev2,t3) ->
          flev obs lev1;
          flev obs lev2;
          obs_t t1;
          obs_t t2;
          obs_t t3;
          obs_phi phi
      | `TypeVar tv -> ftv obs tv
    and obs_q : q -> unit = function
      | #pre -> ()
      | `PreVar pv -> fpv obs pv
    and obs_phi : phi -> unit = function
      | `LocSetVar lsv -> flsv obs lsv
      | `LocSet _ -> ()
    in
      obs_t (tau :> t)
          
  let add_obs = change_obs 
    TVar.add_observer
    LSVar.add_observer
    PrVar.add_observer
    LEVar.add_observer
  let remove_obs = change_obs 
    TVar.remove_observer
    LSVar.remove_observer
    PrVar.remove_observer
    LEVar.remove_observer

  let demote_tau : phi -> tau -> tau option
    = fun phi1 -> function
    | `Obj (q,phi2) ->
        begin
          let two_lsets ls1 ls2 = 
            if (LSet.cardinal ls2 > 1) then
              Some (`Top)
            else
              if (LSet.subset ls2 ls1) then
                Some (`Obj (`In, phi2))
              else
                None
          in            
          match q with
            | `NVUp -> Some (`Top)
            | `NVDown -> Some (`Bottom)
            | `In -> Some (`Obj (`In,phi2))
            | `Ex -> begin
                match phi1,phi2 with
                  | `LocSet ls1, `LocSet ls2 -> 
                      two_lsets ls1 ls2
                  | `LocSetVar lsv, `LocSet ls2 ->
                      begin
                        match LSVar.get_lower lsv with
                          | None -> None
                          | Some ls1 ->
                              two_lsets ls1 ls2
                      end
                  | `LocSet ls1, `LocSetVar lsv ->
                      begin
                        match LSVar.get_upper lsv with
                          | None -> None
                          | Some ls2 ->
                              if (LSet.cardinal ls2 > 1) then
                                None
                              else
                                two_lsets ls1 ls2
                      end
                  | _ -> None
              end
            | `PreVar pv -> None
        end
    | `Fun (lev1,t1,t2,phi,lev2,t3) ->
        let lev1' = LEVar.create () in
        let lev2' = LEVar.create () in
        let t1',t2',t3' = TVar.create (), TVar.create (), TVar.create () in
        let t1',t2',t3' = TypeR.create_t_tv t1', 
          TypeR.create_t_tv t2', TypeR.create_t_tv t3' 
        in
        let bc1 = ConstBasic.create_c_demotation_lenv lev1' phi1 lev1 in
        let bc2 = ConstBasic.create_c_demotation_lenv lev2' phi1 lev2 in
        let bc3 = ConstBasic.create_c_demotation_type t1' phi1 t1 in
        let bc4 = ConstBasic.create_c_demotation_type t2' phi1 t2 in
        let bc5 = ConstBasic.create_c_demotation_type t3' phi1 t3 in
          Const.add_both bc1;
          Const.add_both bc2;
          Const.add_both bc3;
          Const.add_both bc4;
          Const.add_both bc5;
          Some (`Fun (lev1',t1',t2',phi,lev2',t3'))
    | #tb | `Undef | `Int as tau -> Some tau

  let get_locs_tb : tb -> LSet.t = function
    | `Bottom -> LSet.empty
    | `Top -> LSet.empty (* TODO: Here we have to return alle locations
                            of the program *)
  let get_locs_phi : phi -> LSet.t = function
    | `LocSet ls -> ls
    | `LocSetVar lsv -> begin
        match LSVar.get_lower lsv with
          | None -> LSet.empty
          | Some ls -> ls
      end
  let get_locs_tau : tau -> LSet.t = function
    | `Fun _ | `Undef | `Int -> LSet.empty
    | `Obj (q,phi) -> get_locs_phi phi
    | #tb as tb -> get_locs_tb tb
  let get_locs : t -> LSet.t = function
    | #tau as tau -> get_locs_tau tau
    | `TypeVar tv -> 
        begin
          match TVar.get_lower tv with
            | None -> LSet.empty
            | Some tau -> get_locs_tau tau
        end


  let get_lower_phi = function
    | `LocSet ls -> Some ls
    | `LocSetVar lsv -> LSVar.get_lower lsv
  let get_upper_phi = function
    | `LocSet ls -> Some ls
    | `LocSetVar lsv -> LSVar.get_upper lsv

  let get_exact : q -> Pre.t option = function
    | #pre as pre -> Some pre
    | `PreVar pv -> PrVar.get_exact pv

  type neg = bool -> bool
  let add_options :
      ('a -> 'b -> 'b -> 'a -> 'c option) 
      -> 'a option -> 'b -> 'b -> 'a option -> 'c option
    = fun f a1o b1 b2 a2o ->
      match a1o,a2o with
        | None,_ | _,None -> None
        | Some a1, Some a2 -> f a1 b1 b2 a2
  let id_check_equal : neg -> Loc.t -> LSet.t -> LSet.t -> Loc.t -> bool option
    = fun neg l1 ls_lb ls_ub l2 ->
      if (Loc.compare l1 l2 != 0) then begin
        Some false
      end else begin
         if (LSet.mem l1 ls_lb) then
           Some (neg false)
         else
           if (LSet.mem l1 ls_ub) then
             None        
           else
             Some (neg true)
      end

  let is_demotation_phi_IElo_exlo neg = add_options (id_check_equal neg)

  let id_check_lsv_ls = fun f lsv ls_lb ls_ub ls ->
    if ((LSVar.is_one_possible lsv) 
        && (LSet.cardinal ls == 1) 
        && (LSVar.equal_possible_lset lsv ls))
    then begin
      f (LSVar.choose_lower lsv) ls_lb ls_ub (LSet.choose' ls)
    end else begin
      Some false
    end

  let is_demotation_phi_IElsv_exls neg = 
    id_check_lsv_ls (is_demotation_phi_IElo_exlo neg)
     
  let id_check_lsv_lsv : 
      (Loc.t option -> LSet.t -> LSet.t -> Loc.t option -> bool option)
      -> LSVar.t -> LSet.t -> LSet.t -> LSVar.t -> bool option
    = fun f lsv1 ls_lb ls_ub lsv2 ->
      if ((LSVar.is_one_possible lsv1) 
          && (LSVar.is_one_possible lsv2)
          && (LSVar.equal_possible lsv1 lsv2))
      then begin
        f (LSVar.choose_lower lsv1) ls_lb ls_ub (LSVar.choose_lower lsv2)
      end else begin
        Some false
      end
      
  let is_demotation_phi_IEls_exls neg ls1 ls_lb ls_ub ls2 =
    if ((LSet.equal ls1 ls2) && (LSet.cardinal ls2 == 1)) then begin
      let l = LSet.choose ls2 in
        id_check_equal neg l ls_lb ls_ub l
    end else begin
      Some false
    end
  
  let is_demotation_phi_IE_ex : neg -> 
    phi -> LSet.t -> LSet.t -> phi -> bool option
    = fun neg phi1 ls_lb ls_ub phi2 -> 
      match phi1, phi2 with
        | `LocSet ls1, `LocSet ls2 -> 
            is_demotation_phi_IEls_exls neg ls1 ls_lb ls_ub ls2
        | `LocSetVar lsv, `LocSet ls 
        | `LocSet ls, `LocSetVar lsv -> 
            (* These two cases are symetric, because
               in both cases lsv has exaclty one element.
               Also ls has to have one element.
            *)
            is_demotation_phi_IElsv_exls neg lsv ls_lb ls_ub ls
        | `LocSetVar lsv1, `LocSetVar lsv2 ->
            id_check_lsv_lsv
              (is_demotation_phi_IElo_exlo neg)
              lsv1 
              ls_lb 
              ls_ub 
              lsv2

  let is_demotation_obj : q * phi -> LSet.t -> LSet.t -> q * phi -> bool option
    = fun (q1,phi1) ls1 ls2 (q2,phi2) ->
      match get_exact q1, get_exact q2 with
        | None, _ | _, None | Some #nv, _ | _, Some #nv -> None 
            (* TODO: The case (Some #nv) should never happen, 
               because we prever to work with normalized types
               here. Maybe throwing an error is a better option? *)
        | Some `In, Some `In ->
            subtype_tau (`Obj (q1,phi1)) (`Obj (q2,phi2))
        | Some `Ex, Some `Ex ->
            is_demotation_phi_IE_ex (fun b -> b) phi1 ls1 ls2 phi2
        | Some `In, Some `Ex ->
            is_demotation_phi_IE_ex not phi1 ls1 ls2 phi2
        | Some `Ex, Some `In ->
            Some false

  let is_demotation_tau : tau -> LSet.t -> LSet.t -> tau -> bool option
    = fun tau1 ls1 ls2 tau2 ->
      match tau1,tau2 with
          (* easy cases *)
        | _, `Top | `Bottom, _ -> Some true
            
        (* two objects, this is the interesting case *)
        | `Obj (q1,phi1), `Obj (q2,phi2) ->
            is_demotation_obj (q1,phi1) ls1 ls2 (q2,phi2)
              
        (* two tb values -> simple subtype, two undefs, of cause true *)
        | #tb as tb1, (#tb as tb2) -> subtype tb1 tb2
        | `Undef, `Undef -> Some true
            (* two functions, simple subtype *)
        | `Fun _ as f1, (`Fun _ as f2) -> 
            subtype f1 f2
              
        (* diffrent types, subtype is false *)
        | `Obj _, (`Fun _ | #tb | `Undef)
        | `Fun _, (`Obj _ | #tb | `Undef)
        | #tb, (`Obj _ | `Fun _ | `Undef)
        | `Undef, (`Obj _ | #tb | `Fun _) 
            -> Some false
            
        (* The cases where both are equal are not possible here. *)
        | `Top, _ -> Some false
        | _, `Bottom -> Some false
            
        (* TODO: my brain explodes here, so please ask later! *)
        | _ -> None

  let flow, flow_obj =
    let c lev =  
      let tvtvl = ref [] in
      let otvotvl = ref [] in
        
      let rec flow_obj_ex_in_l : Loc.t -> bool option =
        fun l ->
          match LEVar.find' l lev with
            | None -> Some false
            | Some otv1 -> flow_otv_otv otv1 (GEnv.find_or_add l)
      and check_for_one_l l ls2 =
        if LSet.mem l ls2 then begin
          let dlsv = LEVar.get_domain_var lev in
            match LSVar.get_lower dlsv with
              | None -> None
              | Some ls ->
                  if (not (LSet.mem l ls)) then begin
                    Some true
                  end else begin
                    (* second part of or *)
                    match LEVar.find' l lev with
                      | Some otv1 -> flow_otv_otv otv1 (GEnv.find_or_add l)
                      | _ -> None
                  end
        end else begin
          Some false
        end
          
      and flow_obj_ex_in : phi -> phi -> bool option =
        fun phi1 phi2 ->
          match phi1,phi2 with
            | `LocSet ls1, `LocSet ls2 -> 
                if ((LSet.cardinal ls1 == 1) && (LSet.subset ls1 ls2)) then begin
                  let l = LSet.choose ls1 in
                    check_for_one_l l ls2
                end else begin
                  Some false
                end
            | `LocSetVar lsv1, `LocSet ls2 -> 
                if (LSVar.is_one_possible lsv1) && (LSVar.subset_possible_lset lsv1 ls2) 
                then begin
                  match LSVar.choose_lower lsv1 with
                    | None -> None
                    | Some l -> check_for_one_l l ls2
                end else begin
                  Some false
                end
            | `LocSet ls1, `LocSetVar lsv2 -> 
                begin
                  match LSVar.get_lower lsv2 with
                    | None -> None
                    | Some ls2 -> 
                        if (LSet.cardinal ls1 == 1) && (LSet.subset ls1 ls2)
                        then begin
                          let l = LSet.choose ls1 in
                            match LSVar.get_upper lsv2 with
                              | None -> None
                              | Some ls -> check_for_one_l l ls
                        end else begin
                          Some false
                        end
                end
            | `LocSetVar lsv1, `LocSetVar lsv2 -> 
                if (LSVar.is_one_possible lsv1) && (LSVar.subset_possible lsv1 lsv2) 
                then begin
                  match LSVar.choose_lower lsv1, LSVar.get_upper lsv2 with
                    | Some l, Some ls -> check_for_one_l l ls
                    | _ -> None
                end else begin
                  Some false
                end
                  
      and flow_obj_ex : 
          (unit -> bool option) -> phi -> q * phi -> bool option =
        fun f phi1 (q2,phi2) -> 
          match q2 with
            | `PreVar pv2 -> None
            | `In -> flow_obj_ex_in phi1 phi2
            | _ -> f ()
                
      and flow_obj : q * phi -> q * phi -> bool option = 
        fun (q1,phi1) (q2,phi2) ->
          let f () = subtype_tau (`Obj (q1,phi1)) (`Obj (q2,phi2)) in
          let one_pre pv1 =
            match PrVar.get_lower pv1 with
              | None -> None
              | Some `In -> f ()
              | Some `Ex -> flow_obj_ex f phi1 (q2,phi2)
              | Some _ -> None
          in
            match q1,q2 with
              | `PreVar pv1, `PreVar pv2 ->
                  if (PrVar.compare pv1 pv2 == 0) then begin
                    f ()
                  end else begin
                    one_pre pv1 
                  end
              | `PreVar pv1, _ -> one_pre pv1
              | `Ex, _ -> flow_obj_ex f phi1 (q2,phi2)
              | _ -> f ()
                  
                  
      and flow_tau : tau -> tau -> bool option = fun t1 t2 ->
        match t1,t2 with
          | `Obj (q1,phi1), `Obj (q2,phi2) ->
              flow_obj  (q1,phi1) (q2,phi2)
          | (#tau as t1), (#tau as t2) -> subtype_tau t1 t2
              
      and flow : t -> t -> bool option = fun t1 t2 ->
        match t1,t2 with
          | #tau as t1, (#tau as t2) -> flow_tau t1 t2
          | `TypeVar tv1, `TypeVar tv2 -> flow_tv_tv tv1 tv2
          | `TypeVar tv1, (#tau as t2) -> 
              (* TODO: Test this case carefully *)
              begin
                match TVar.get_lower tv1 with
                  | None -> None
                  | Some t1 -> begin
                      match flow_tau t1 t2 with
                        | Some true | None -> None
                        | Some false -> Some false
                    end
              end
          | #tau as t1, `TypeVar tv2  -> 
              (* TODO: Test this case carefully *)
              begin
                match TVar.get_upper tv2 with
                  | None -> None
                  | Some t2 -> begin
                      match flow_tau t1 t2 with
                        | Some true | None -> None
                        | Some false -> Some false
                    end
              end
                
      and flow_tv_tv : TVar.t -> TVar.t -> bool option = fun tv1 tv2 ->
        if List.mem (tv1,tv2) !tvtvl then begin
          Some true
        end else begin
          tvtvl := (tv1,tv2) :: !tvtvl;
          check_both TVar.get_lower TVar.get_upper flow_tau tv1 tv2
        end        
          
      and flow_otv_otv : OTVar.t -> OTVar.t -> bool option = fun otv1 otv2 ->
        if List.mem (otv1,otv2) !otvotvl then begin
          Some true
        end else begin
          otvotvl := (otv1,otv2) :: !otvotvl;
          OTVar.fold 
            (fun prop tv1 b -> 
               match OTVar.find' prop otv2 with
                 | None -> Some false
                 | Some tv2 -> begin
                     match flow_tv_tv tv1 tv2,b with
                       | Some false,_ | _, Some false -> Some false
                       | None, _ | _, None -> None
                       | Some true, Some true -> Some true
                   end
            )
            otv1
            (Some true)
        end
      in
        flow, flow_otv_otv
  in
      (fun lev -> fst (c lev)), (fun lev -> snd (c lev))

  let make_eq_eq_tau tau1 tau2 = match tau1,tau2 with (* TODO *)
    | `Top, `Top -> true, `Top
    | _ -> (true, tau2)
  let make_eq_lower_tau (lower : tau) (tau : tau) = (* TODO *)
    match lower,tau with
      | `Bottom , _ -> true, tau
      | `Obj (q1,phi1), `Obj(q2,phi2) ->
          let bc1 = ConstBasic.create_c_eqset phi1 phi2 in
          let bc2 = ConstBasic.create_c_eqpres q1 q2 in
            Const.add_both bc1;
            Const.add_both bc2;
            true, tau
      
      | `Fun (lev11,t11,t12,phi1,lev12,t13),
          `Fun (lev21,t21,t22,phi2,lev22,t23) ->
          let _ = LEVar.add_alias lev11 lev21 in
          let _ = LEVar.add_alias lev12 lev22 in
          let bc1 = ConstBasic.create_c_eqtype t11 t21 in
          let bc2 = ConstBasic.create_c_eqtype t12 t22 in
          let bc3 = ConstBasic.create_c_eqset phi1 phi2 in
          let bc4 = ConstBasic.create_c_eqtype t13 t23 in
          let _ = List.iter
            (fun bc -> Const.add_both bc)
            [bc1;bc2;bc3;bc4]
          in
            true, tau
      | _ -> true, tau
  let make_eq_upper_tau upper tau = (* TODO *)
    match upper,tau with
      | `Top , _ -> true, tau
      | `Obj (q1,phi1), `Obj(q2,phi2) ->
          let bc1 = ConstBasic.create_c_eqset phi1 phi2 in
          let bc2 = ConstBasic.create_c_eqpres q1 q2 in
            Const.add_both bc1;
            Const.add_both bc2;
            true, tau
      | `Fun (lev11,t11,t12,phi1,lev12,t13),
          `Fun (lev21,t21,t22,phi2,lev22,t23) ->
          let _ = LEVar.add_alias lev11 lev21 in
          let _ = LEVar.add_alias lev12 lev22 in
          let bc1 = ConstBasic.create_c_eqtype t11 t21 in
          let bc2 = ConstBasic.create_c_eqtype t12 t22 in
          let bc3 = ConstBasic.create_c_eqset phi1 phi2 in
          let bc4 = ConstBasic.create_c_eqtype t13 t23 in
          let _ = List.iter
            (fun bc -> Const.add_both bc)
            [bc1;bc2;bc3;bc4]
          in
            true, tau
      | _ -> true, tau
end


and V : sig
  include Gvars.VARS 
  val reset : unit -> unit
end
with type TVar.elm = TypeR.tau
and type PrVar.elm = TypeR.pre
and type OTVar.I.key = Syntax.label
and type TVar.obs = ConstBasic.obs
and type LSVar.obs = ConstBasic.obs
and type PrVar.obs =  ConstBasic.obs
and type OTVar.obs = ConstBasic.obs
and type LEVar.obs = ConstBasic.obs
  = 
struct
  module T = struct
    type t = TypeR.tau
    type obs = ConstBasic.obs
    let union = TypeR.union_tau
    let inter = TypeR.inter_tau
    let subset = TypeR.subtype_tau
    let make_eq_eq = TypeR.make_eq_eq_tau
    let make_eq_lower = TypeR.make_eq_lower_tau
    let make_eq_upper = TypeR.make_eq_upper_tau
    let is_empty = function
      | `Bottom -> true
      | _ -> false
    let string_of = TypeR.string_of_tau
    let compare = TypeR.compare_tau
    let normalize = TypeR.normalize_tau
  end
  include Gvars.Make(
    struct
      type t = ConstBasic.obs
      let fire = ConstBasic.fire
      let string_of = ConstBasic.string_of_obs
      let compare = ConstBasic.compare_obs
    end)(T)(Const)
  let reset () =
    TVar.reset ();
    LSVar.reset ();
    PrVar.reset ();
    OTVar.reset ();
    LEVar.reset ()
end

and Vars : sig
  include Gvars.VARS 
  val reset : unit -> unit
end
  with type TVar.elm = TypeR.tau
  and type PrVar.elm = TypeR.pre
  and type OTVar.I.key = Syntax.label
  and type TVar.obs = ConstBasic.obs
  and type LSVar.obs = ConstBasic.obs
  and type PrVar.obs =  ConstBasic.obs
  and type OTVar.obs = ConstBasic.obs
  and type LEVar.obs = ConstBasic.obs
  and type TVar.t = V.TVar.t
= struct
  module TVar = struct
    type t = V.TVar.t
    type img = V.TVar.img
    type obs = V.TVar.obs

    let create = V.TVar.create
    let compare = V.TVar.compare
    let total_ord = V.TVar.total_ord
    let add_alias = V.TVar.add_alias
    let do_normalize = V.TVar.do_normalize
    let add_observer = V.TVar.add_observer
    let remove_observer = V.TVar.remove_observer
    let string_of = V.TVar.string_of
    let reset = V.TVar.reset
    type elm = V.TVar.elm 

    let do_option f t = function
      | None -> ()
      | Some i -> 
          f (`VTVar t) i
    let do_r f t = 
      let l_img = V.TVar.get_lower t in
      let u_img = V.TVar.get_upper t in
        do_option f t l_img;
        do_option f t u_img


    let set_upper img t = 
      let img = TypeR.normalize_tau img in
        V.TVar.set_upper img t;
        do_r TypeR.add_obs t

    let set_lower img t = 
      let img = TypeR.normalize_tau img in
        V.TVar.set_lower img t;
        do_r TypeR.add_obs t

    let set_eq img t = 
      let img = TypeR.normalize_tau img in
        V.TVar.set_eq img t;
        do_r TypeR.add_obs t

    let subset = V.TVar.subset
    let get_lower = V.TVar.get_lower
    let get_upper = V.TVar.get_upper
    let string_of_img = V.TVar.string_of_img
    let get_exact = V.TVar.get_exact
    let do_on_exact = V.TVar.do_on_exact
    let do_on_lower = V.TVar.do_on_lower
    let do_on_upper = V.TVar.do_on_upper
    let get_all_ts = V.TVar.get_all_ts
    let string_of_with_img = V.TVar.string_of_with_img
  end
  module PrVar = V.PrVar
  module ObjectType = V.ObjectType
  module OTVar = V.OTVar
  module LSVar = V.LSVar
  module LEVar = V.LEVar
  let reset = V.reset
end

and ConstBasic : sig
  type vars = [
    | `VTVar of Vars.TVar.t  
    | `VLSetVar of Vars.LSVar.t
    | `VPreVar of Vars.PrVar.t
    | `VOTVar of Vars.OTVar.t
    | `VLEVar of Vars.LEVar.t
  ]
  type eq = [
    | `EqType of TypeR.t * TypeR.t
    | `EqLenv of TypeR.lenv * TypeR.lenv
    | `EqObj of TypeR.r * TypeR.r
    | `EqSet of TypeR.phi * TypeR.phi
    | `EqPres of TypeR.q * TypeR.q
  ]
  type set = [
    | `Include of Loc.t * TypeR.phi
    | `Exclude of Loc.t * TypeR.phi
    | `Subset of TypeR.phi * TypeR.phi * int ref
    | `Disjoint of TypeR.phi * TypeR.phi * int ref
  ]
  type bools = [
    | `True
    | `False of string option
  ]
  type demotation = [ 
    | `DemotationE of Vars.LEVar.t * TypeR.phi * TypeR.phi * Vars.LEVar.t * LSetRef.t
    | `DemotationT of TypeR.t * TypeR.phi * TypeR.t
    | `DemotationO of Vars.OTVar.t * TypeR.phi * Vars.OTVar.t 
        * Syntax.LabelSetRef.t
  ]
  type prop = [
      (* sigma |-r X varphi.a : t *) 
    | `Read of Vars.LEVar.t * Vars.PrVar.t * Vars.LSVar.t 
        * Syntax.label * Vars.TVar.t 
      (* sigma |-w X varphi.a = t, sigma *) 
    | `Write of Vars.LEVar.t * Vars.TVar.t * Syntax.label * Vars.TVar.t * Vars.LEVar.t
  ]
  type domain = [
      (* phi = dom(sigma) *)
    | `DomainEq of TypeR.phi * Vars.LEVar.t
      (* *)
    | `DomainEmptyObj of Vars.LEVar.t * Vars.LEVar.t * Loc.t
  ]
  type subtype = [
    (* t <: t *)
    | `SubType of TypeR.t * TypeR.t * int ref
    | `SubPre of Vars.PrVar.t * Vars.PrVar.t * int ref
    | `SubEnv of Vars.LEVar.t * Vars.LEVar.t * LSet.t ref * int ref
    | `SubObj of Vars.OTVar.t * Vars.OTVar.t * Syntax.LabelSetRef.t * int ref
  ]
  type flow = [
      (* sigma, t1 <| t2 *)
    | `FlowType of Vars.LEVar.t * TypeR.t * TypeR.t 
    | `FlowObj of Vars.LEVar.t * Vars.OTVar.t * Vars.OTVar.t
  ]
  type locs = [ `Locs of TypeR.phi * TypeR.t ]
  type t = [
    | locs | eq | set | bools | demotation | prop | domain | subtype | flow
  ]
  type obs = [ t | vars ]
      
  val c_true : t
  val c_false : string option -> t
  val create_c_demotation_lenv : Vars.LEVar.t -> TypeR.phi -> Vars.LEVar.t -> t
  val create_c_demotation_lenv_phi : Vars.LEVar.t -> TypeR.phi -> TypeR.phi -> Vars.LEVar.t -> t  
  val create_c_demotation_type : TypeR.t -> TypeR.phi -> TypeR.t -> t
  val create_c_demotation_object : 
    Vars.OTVar.t -> TypeR.phi -> Vars.OTVar.t -> t
  val create_c_include : Loc.t -> TypeR.phi -> t
  val create_c_exclude : Loc.t -> TypeR.phi -> t
  val create_c_subset : ?prio:int -> TypeR.phi -> TypeR.phi -> t 
  val create_c_disjoint : ?prio:int -> TypeR.phi -> TypeR.phi -> t
  val create_c_subtype : ?prio:int -> TypeR.t -> TypeR.t -> t
  val create_c_subpre : ?prio:int -> Vars.PrVar.t -> Vars.PrVar.t -> t
  val create_c_subenv : ?prio:int -> Vars.LEVar.t -> Vars.LEVar.t -> t
  val create_c_subobj : ?prio:int -> Vars.OTVar.t -> Vars.OTVar.t -> t
  val create_c_eqtype : TypeR.t -> TypeR.t -> t
  val create_c_eqlenv : TypeR.lenv -> TypeR.lenv -> t
  val create_c_eqobj : TypeR.r -> TypeR.r -> t
  val create_c_eqset : TypeR.phi -> TypeR.phi -> t
  val create_c_eqpres : TypeR.q -> TypeR.q -> t
  val create_c_domain_eq : TypeR.phi -> Vars.LEVar.t -> t
  val create_c_lev_empty_obj : 
    Vars.LEVar.t -> Vars.LEVar.t -> Location.Loc.t -> t
  val create_c_flow : Vars.LEVar.t -> TypeR.t -> TypeR.t -> t
  val create_c_flowobj : Vars.LEVar.t -> Vars.OTVar.t -> Vars.OTVar.t -> t
  val create_c_read : Vars.LEVar.t -> Vars.PrVar.t -> 
    Vars.LSVar.t -> Syntax.label -> Vars.TVar.t -> t
  val create_c_write : Vars.LEVar.t -> 
    Vars.TVar.t -> Syntax.label -> 
    Vars.TVar.t -> Vars.LEVar.t -> t
  val create_c_locs : TypeR.phi -> TypeR.t -> t
  val string_of : t -> string
  val compare : t -> t -> int

  val fire : obs -> unit
  val get_prio : obs -> int
  val string_of_obs : obs -> string
  val compare_obs : obs -> obs -> int

  val register_obs : t -> unit
  val unregister_obs : t -> unit
  val obs_to_t : obs -> t option
end = struct
  exception InternalSolveError of string
  open Vars

  type vars = [
    | `VTVar of Vars.TVar.t  
    | `VLSetVar of Vars.LSVar.t
    | `VPreVar of Vars.PrVar.t
    | `VOTVar of Vars.OTVar.t
    | `VLEVar of Vars.LEVar.t
  ]
  type eq = [
    | `EqType of TypeR.t * TypeR.t
    | `EqLenv of TypeR.lenv * TypeR.lenv
    | `EqObj of TypeR.r * TypeR.r
    | `EqSet of TypeR.phi * TypeR.phi
    | `EqPres of TypeR.q * TypeR.q
  ]
  type set = [
    | `Include of Loc.t * TypeR.phi
    | `Exclude of Loc.t * TypeR.phi
    | `Subset of TypeR.phi * TypeR.phi * int ref
    | `Disjoint of TypeR.phi * TypeR.phi * int ref
  ]
  type bools = [
    | `True
    | `False of string option
  ]
  type demotation = [ 
    | `DemotationE of LEVar.t * TypeR.phi * TypeR.phi * LEVar.t * LSetRef.t
    | `DemotationT of TypeR.t * TypeR.phi * TypeR.t
    | `DemotationO of Vars.OTVar.t * TypeR.phi * Vars.OTVar.t * Syntax.LabelSetRef.t
  ]
  type prop = [
      (* sigma |-r X varphi.a : t *) 
    | `Read of LEVar.t * PrVar.t * LSVar.t 
        * Syntax.label * Vars.TVar.t
      (* sigma |-w X varphi.a = t, sigma *) 
    | `Write of LEVar.t * TVar.t * Syntax.label * TVar.t * LEVar.t
  ]
  type domain = [
      (* phi = dom(sigma) *)
    | `DomainEq of TypeR.phi * LEVar.t
      (* *)
    | `DomainEmptyObj of LEVar.t * LEVar.t * Loc.t
  ] 
  type subtype = [
    (* t <: t *)
    | `SubType of TypeR.t * TypeR.t * int ref
    | `SubPre of Vars.PrVar.t * Vars.PrVar.t * int ref
    | `SubEnv of Vars.LEVar.t * Vars.LEVar.t * LSet.t ref * int ref
    | `SubObj of Vars.OTVar.t * Vars.OTVar.t * Syntax.LabelSetRef.t * int ref
  ]
  type flow = [
      (* sigma, t1 <| t2 *)
    | `FlowType of Vars.LEVar.t * TypeR.t * TypeR.t 
    | `FlowObj of Vars.LEVar.t * Vars.OTVar.t * Vars.OTVar.t
  ]
  type locs = [ `Locs of TypeR.phi * TypeR.t ]
  type t = [
    | locs | eq | set | bools | demotation | prop | domain | subtype | flow
  ]
  type obs = [ t | vars ]
  let apply_twice f v1 v2 = f v1; f v2

  let c_true = `True
  let c_false s = `False s 
  let create_c_demotation_lenv levar1 phi levar2 = 
    `DemotationE (levar1,phi,(TypeR.create_phi_ls LSet.empty),levar2,LSetRef.create ())
  let create_c_demotation_lenv_phi levar1 phi1 phi2 levar2 = 
    `DemotationE (levar1,phi1,phi2,levar2,LSetRef.create ())

  let create_c_demotation_type t1 phi t2 = `DemotationT (t1,phi,t2)
  let create_c_demotation_object otv1 phi otv2 = 
    `DemotationO (otv1,phi,otv2,Syntax.LabelSetRef.create ())

  let create_c_include l phi = `Include (l,phi)
  let create_c_exclude l phi = `Exclude (l,phi)
  let create_c_subset ?(prio = 110) phi1 phi2 = `Subset (phi1,phi2,ref prio)
  let create_c_disjoint ?(prio = 110) phi1 phi2 = `Disjoint (phi1,phi2, ref prio)
    
  let create_c_subtype ?(prio = 110) t1 t2 = `SubType (t1,t2,ref prio)
  let create_c_subpre ?(prio = 110) pv1 pv2 = `SubPre (pv1,pv2,ref prio)
  let create_c_subenv ?(prio = 110) lev1 lev2 = `SubEnv (lev1,lev2, ref LSet.empty, ref prio)
  let create_c_subobj ?(prio = 110) otv1 otv2 = 
    `SubObj (otv1,otv2, Syntax.LabelSetRef.create (), ref prio)
  let create_c_eqtype t1 t2 = `EqType (t1,t2)

  let create_c_eqlenv le1 le2 = `EqLenv (le1,le2)
  let create_c_eqobj r1 r2 = `EqObj (r1,r2)
  let create_c_eqset phi1 phi2 = `EqSet (phi1,phi2)
  let create_c_eqpres q1 q2 = `EqPres (q1,q2)
    
  let create_c_domain_eq phi le = `DomainEq (phi,le)
  let create_c_lev_empty_obj le le2 l = `DomainEmptyObj (le,le2,l)

  let create_c_flow lenv t1 t2 = `FlowType (lenv,t1,t2)
  let create_c_flowobj lev otv1 otv2 = `FlowObj (lev,otv1,otv2)

  let create_c_read lenv prevar lsvar a tv = `Read (lenv,prevar,lsvar,a,tv)
  let create_c_write lenv tv1 a tv2 lenv' = 
    `Write (lenv,tv1,a,tv2,lenv')
  let create_c_locs ls t = `Locs (ls,t)


  let demotation_str s1 s2 s3 = s1 ^ " :=#" ^ s2 ^ " " ^ s3
  let string_eq s1 s2 = s1 ^ " = " ^ s2
    
  let string_of_eq = function
    | `EqType (t1,t2) ->
        string_eq (TypeR.string_of t1) (TypeR.string_of t2)
    | `EqLenv (lenv1, lenv2) ->
        string_eq (TypeR.string_of_lenv lenv1) (TypeR.string_of_lenv lenv2)
    | `EqObj (o1,o2) ->
        string_eq (TypeR.string_of_r o1) (TypeR.string_of_r o2)
    | `EqSet (phi1,phi2) ->
        string_eq (TypeR.string_of_phi phi1) (TypeR.string_of_phi phi2) 
    | `EqPres (q1,q2) ->
        string_eq (TypeR.string_of_q q1) (TypeR.string_of_q q2)
  let string_of_set = function
    | `Include (l,phi) ->
        Loc.string_of l ^ " IN " ^ TypeR.string_of_phi phi
    | `Exclude (l,phi) ->
        Loc.string_of l ^ " EX " ^ TypeR.string_of_phi phi
    | `Subset (phi1,phi2,_) ->
        TypeR.string_of_phi phi1 ^ " s " ^ TypeR.string_of_phi phi2
    | `Disjoint (phi1,phi2,_) ->
        TypeR.string_of_phi phi1 ^ " d " ^ TypeR.string_of_phi phi2
  let string_of_prop = function
    | `Read (lev,prev,lsv,a,tv) ->
        LEVar.string_of lev ^ " |-r " 
        ^ PrVar.string_of prev ^ " " 
        ^ LSVar.string_of lsv
        ^ "." ^ Syntax.string_of_label a
        ^ " : " ^ TVar.string_of tv
    | `Write (lev,tv1,a,tv2,lev2) ->
        LEVar.string_of lev ^ " |-w " 
        ^ TVar.string_of tv1
        ^ "." ^ Syntax.string_of_label a
        ^ " = " ^ TVar.string_of tv2 
        ^ " => " ^ LEVar.string_of lev2
  let string_of_subtype = function
    | `SubType (t1,t2,_) ->
        TypeR.string_of t1 ^ " <: " ^ TypeR.string_of t2
    | `SubPre (pv1,pv2,_) ->
        Vars.PrVar.string_of pv1 ^ " <: " ^ Vars.PrVar.string_of pv2
    | `SubEnv (lev1,lev2,_,_)  ->
        Vars.LEVar.string_of lev1 ^ " <: " ^ Vars.LEVar.string_of lev2
    | `SubObj (otv1,otv2,_,_) ->
        Vars.OTVar.string_of otv1 ^ " <: " ^ Vars.OTVar.string_of otv2
  let string_of_flow = function
    | `FlowType (lev,t1,t2) ->
        LEVar.string_of lev ^ ", " ^ TypeR.string_of t1 
        ^ " <| " ^ TypeR.string_of t2
    | `FlowObj (lev,otv1,otv2) ->
        LEVar.string_of lev ^ ", " ^ OTVar.string_of otv1
        ^ " <| " ^ OTVar.string_of otv2
  let string_of_bools = function
    | `True -> "TRUE"
    | `False s -> "FALSE"
  let string_of_demotation = function
    | `DemotationT (t,phi,t2) ->
        demotation_str 
          (TypeR.string_of t) 
          (TypeR.string_of_phi phi) 
          (TypeR.string_of t2)
    | `DemotationE (lenv1,phi1,phi2,lenv2,_) -> 
        demotation_str 
          (LEVar.string_of lenv1) 
          ((TypeR.string_of_phi phi1)
             ^(match TypeR.get_upper_phi phi2 with
                 | Some ls -> 
                     if (LSet.is_empty ls) then ""
                     else TypeR.string_of_phi phi2
                 | _ -> TypeR.string_of_phi phi2)
          )
          (LEVar.string_of lenv2)
    | `DemotationO (otv1,phi,otv2,_) ->
        demotation_str
          (OTVar.string_of otv1)
          (TypeR.string_of_phi phi)
          (OTVar.string_of otv2)
  let string_of_domain = function
    | `DomainEq (phi,lev) ->
        string_eq 
          (TypeR.string_of_phi phi)
          ("dom(" ^ (LEVar.string_of lev) ^ ")") 
    | `DomainEmptyObj (lev1,lev2,l) -> 
        LEVar.string_of lev1
        ^ " = " ^ LEVar.string_of lev2
        ^ "[" ^ Loc.string_of l ^ " -> {}]"
  let string_of_vars = function
    | `VTVar tv -> TVar.string_of tv
    | `VLSetVar lsv -> LSVar.string_of lsv
    | `VPreVar psv -> PrVar.string_of psv
    | `VOTVar otv -> OTVar.string_of otv
    | `VLEVar lev -> LEVar.string_of lev

  let string_of = function 
    | #eq as eq -> string_of_eq eq
    | #set as s -> string_of_set s
    | #prop as p -> string_of_prop p
    | #subtype as st -> string_of_subtype st
    | #flow as f -> string_of_flow f
    | #bools as b -> string_of_bools b
    | #demotation as d -> string_of_demotation d
    | #domain as d -> string_of_domain d
    | `Locs (phi,t) ->
        TypeR.string_of_phi phi ^ "<L" ^ TypeR.string_of t 

  let compare_demotation d1 d2 = match d1,d2 with
    | `DemotationE (lev1,phi11,phi12,lev2,_),`DemotationE (lev3,phi21,phi22,lev4,_) -> 
        Pervasives.compare (lev1,phi11,phi12,lev2) (lev3,phi21,phi22,lev4)
    | `DemotationO (otv1,phi1,otv2,_), `DemotationO (otv3,phi2,otv4,_) ->
        Pervasives.compare (otv1,phi1,otv2) (otv3,phi2,otv4)
    | d1,d2 -> Pervasives.compare d1 d2
  open ExtUtils
  open Utils
  let cmp_t f (t11,t12) (t21,t22) = f t11 t21 <.< (fun () -> f t12 t22)
  let compare_subtype st1 st2 = match st1,st2 with
    | `SubType (t11,t12,_),`SubType (t21,t22,_) -> 
        cmp_t TypeR.compare (t11,t12) (t21,t22)
    | `SubPre (pv11,pv12,_),`SubPre (pv21,pv22,_) -> 
        cmp_t PrVar.total_ord (pv11,pv12) (pv21,pv22)
    | `SubEnv (lev11,lev12,_,_),`SubEnv (lev21,lev22,_,_) -> 
        cmp_t LEVar.total_ord (lev11,lev12) (lev21,lev22)
    | `SubObj (otv11,otv12,_,_),`SubObj (otv21,otv22,_,_) -> 
        cmp_t OTVar.total_ord (otv11,otv12) (otv21,otv22)
    | _ -> Pervasives.compare st1 st2

  let compare_flow f1 f2 = match f1,f2 with
    | `FlowType (lev1,t11,t12), `FlowType (lev2,t21,t22) ->
        LEVar.total_ord lev1 lev2 <.< 
          (fun () -> cmp_t TypeR.compare (t11,t12) (t21,t22))
    | `FlowObj (lev1,otv11,otv12), `FlowObj (lev2,otv21,otv22) ->
        LEVar.total_ord lev1 lev2 <.< 
          (fun () -> cmp_t OTVar.total_ord (otv11,otv12) (otv21,otv22))
    | _ -> Pervasives.compare f1 f2          

  let compare : t -> t -> int = fun t1 t2 -> match t1,t2 with
    | (#demotation as d1), (#demotation as d2) -> compare_demotation d1 d2
    | (#subtype as st1), (#subtype as st2) -> compare_subtype st1 st2
    | (#flow as f1), (#flow as f2) -> compare_flow f1 f2
    | t1,t2 -> Pervasives.compare t1 t2

  let debug c =
    let c_str = string_of c in
      pdebug ("Fire: " ^ c_str)

  let internal_error s = raise (InternalSolveError s) 

  type fs = {
    ftv: obs -> TVar.t -> unit;
    flsv: obs -> LSVar.t -> unit;
    fpv: obs -> PrVar.t -> unit;
    flev: obs -> LEVar.t -> unit;
    fotv: obs -> OTVar.t -> unit;
  }
  type c_obs = {
    vars: fs;
    cphi: t -> TypeR.phi -> unit;
    cq : t -> TypeR.q -> unit;
    cr : t -> TypeR.r -> unit;
    clenv : t -> TypeR.lenv -> unit;
    ct : t -> TypeR.t -> unit;
  }
  let change_obs_vars : fs -> vars -> unit = fun c -> function
    | `VTVar tv as bc -> c.ftv bc tv
    | `VLSetVar lsv as bc -> c.flsv bc lsv
    | `VPreVar pv as bc -> c.fpv bc pv
    | `VOTVar otv as bc -> c.fotv bc otv
    | `VLEVar lev as bc -> c.flev bc lev
  let change_obs c (bc : t) = 
    match bc with
      | #bools -> ()
      | `DomainEq (phi,le) -> 
          c.vars.flev (bc :> obs) le;
          c.cphi bc phi
      | `EqPres (q1,q2) -> apply_twice (c.cq bc) q1 q2
      | `EqSet (ls1,ls2) -> c.cphi bc ls1; c.cphi bc ls2;
      | `EqObj (r1,r2) -> apply_twice (c.cr bc) r1 r2
      | `EqLenv (le1,le2) -> apply_twice (c.clenv bc) le1 le2
      | `EqType (t1,t2) -> apply_twice (c.ct bc) t1 t2
      | `Disjoint (phi1,phi2,_) -> apply_twice (c.cphi bc) phi1 phi2
      | `Subset (phi1,phi2,_) -> apply_twice (c.cphi bc) phi1 phi2
      | `Include (_,phi) | `Exclude (_,phi) -> c.cphi bc phi
      | `Read (lev1,pv,lsv,_,tv) ->
          c.vars.flev (bc :> obs) lev1;
          c.vars.fpv (bc :> obs) pv;
          c.vars.flsv (bc :> obs) lsv;
          c.vars.ftv (bc :> obs) tv
      | `Write (lev1,tv1,_,tv2,lev2) ->
          apply_twice (c.vars.flev (bc :> obs)) lev1 lev2;
          apply_twice (c.vars.ftv (bc :> obs)) tv1 tv2
      | `SubType (t1,t2,_) -> apply_twice (c.ct bc) t1 t2
      | `SubEnv (lev1,lev2,_,_) -> 
          apply_twice (c.vars.flev (bc :> obs)) lev1 lev2
      | `SubObj (otv1,otv2,_,_) ->
          apply_twice (c.vars.fotv (bc :> obs)) otv1 otv2
      | `SubPre (pv1,pv2,_) ->
          apply_twice (c.vars.fpv (bc :> obs)) pv1 pv2
      | `FlowObj (lev,otv1,otv2) ->
          apply_twice (c.vars.fotv (bc :> obs)) otv1 otv2;
          c.vars.flev (bc :> obs) lev
      | `FlowType (lev,t1,t2) ->
          c.vars.flev (bc :> obs) lev;
          apply_twice (c.ct bc) t1 t2
      | `DemotationE(lev1,phi1,phi2,lev2,_) ->
          c.cphi bc phi1;
          c.cphi bc phi2;
          c.vars.flev (bc :> obs) lev1;
          c.vars.flev (bc :> obs) lev2
      | `DemotationO (otv1,phi,otv2,_) ->
          c.cphi bc phi;
          apply_twice (c.vars.fotv (bc :> obs)) otv1 otv2
      | `DemotationT (t1,phi,t2) ->
          c.cphi bc phi;
          apply_twice (c.ct bc) t1 t2
      | `DomainEmptyObj (lev1,lev2,_) ->
          apply_twice (c.vars.flev (bc :> obs)) lev1 lev2
      | `Locs (phi,t) -> 
          c.ct bc t;
          c.cphi bc phi

  let c_functions f = 
    let c_phi (bc : t) = function
      | `LocSet ls -> ()
      | `LocSetVar lsv -> 
          f.flsv (bc :> obs) lsv
    in
    let c_t (bc : t) = function
      | `TypeVar tv -> f.ftv (bc :> obs) tv
      | _ -> ()
    in
    let c_q (bc : t) = function
      | `PreVar pv -> f.fpv (bc :> obs) pv
      | _ -> ()
    in
    let c_lenv (bc : t) = function
      | `LEnvVar lev -> f.flev (bc :> obs) lev
      | _ -> ()
    in
    let c_r (bc : t) = function
      | `OTVar ov -> f.fotv (bc :> obs) ov
      | _ -> ()
    in
      { vars = f;
        cphi = c_phi;
        cq = c_q;
        cr = c_r;
        clenv = c_lenv;
        ct = c_t;
      }

  let register_obs = 
    let fs = 
      { flsv = LSVar.add_observer;
        ftv = TVar.add_observer;
        fpv = PrVar.add_observer;
        flev = LEVar.add_observer;
        fotv = OTVar.add_observer;
      } 
    in
      change_obs (c_functions fs)

  let unregister_obs = 
    let fs = 
      { flsv = LSVar.remove_observer; 
        ftv = TVar.remove_observer;
        fpv = PrVar.remove_observer;
        flev = LEVar.remove_observer;
        fotv = OTVar.remove_observer;
      } 
    in
      change_obs (c_functions fs)

  let ans_false s = Const.is_false s

  let do_include = function 
      | (l,`LocSet l2) ->
          if (not (LSet.mem l l2)) then 
            ans_false "include not allowed"
      | (l,`LocSetVar ls) ->
          LSVar.set_lower (LSet.singleton l) ls
  let do_exclude = function 
    | (l,`LocSet l2) ->
        if (LSet.mem l l2) then 
          ans_false "exclude not allowed" 
    | (l,`LocSetVar ls) ->
        LSVar.set_neg (LSet.singleton l) ls

  let simplify_subset (`Subset (s1,s2,_) as bc) = 
    match s1,s2 with
      | `LocSet l1, `LocSet l2 ->
          if (LSet.subset l1 l2) then 
            Const.remove bc
          else 
            ans_false "subset not possible"
      | `LocSet l, `LocSetVar ls ->
          Const.remove bc;
          LSVar.set_lower l ls
      | `LocSetVar ls1, `LocSetVar ls2 ->
          LSVar.subset ls1 ls2;
      | `LocSetVar lsv, `LocSet ls ->
          Const.remove bc;
          LSVar.set_upper ls lsv

  let simplify_eqset (`EqSet (phi1,phi2)) =
    let bc = `EqSet (phi1,phi2) in
      match phi1,phi2 with
        | `LocSetVar ls1, `LocSetVar ls2 ->
            Const.remove bc;
            LSVar.add_alias ls1 ls2
        | `LocSetVar ls, `LocSet l
        | `LocSet l, `LocSetVar ls ->
            Const.remove bc;
            LSVar.set_eq l ls;
        | `LocSet l1, `LocSet l2 ->
            if (LSet.compare l1 l2 == 0) then
              Const.remove bc
            else
              Const.is_false "Sets not equal"

  let simplify_eqpres (`EqPres p) = 
    let c = `EqPres p in
      match p with 
        | `In, `In | `Ex, `Ex | `NVUp, `NVUp | `NVDown, `NVDown ->
            Const.remove c
        | (#nv | `In | `Ex as q), `PreVar pv 
        | `PreVar pv, (#nv | `In | `Ex as q) ->
            Const.remove c;
            PrVar.set_eq q pv
        | `PreVar pv1, `PreVar pv2 ->
            Const.remove c;
            PrVar.add_alias pv1 pv2
        | _ -> ans_false "Precise and Imprecise equality found"

  let simplify_eqtype ((`EqType t) as bc) =
    Const.remove bc;
    match t with
      | `TypeVar tv1, `TypeVar tv2 ->
          TVar.add_alias tv1 tv2
      | (#TypeR.tau as t),`TypeVar tv 
      | `TypeVar tv,(#TypeR.tau as t) ->
          TVar.set_eq t tv;
      | `Undef, `Undef | `Top, `Top | `Bottom, `Bottom | `Int, `Int -> Const.remove bc
      | `Obj (q1,phi1), `Obj (q2,phi2) ->
          let bc = create_c_eqpres q1 q2 in
          let bc2 = create_c_eqset phi1 phi2 in
            Const.add_both bc2;
            Const.add_both bc;
      | (`Fun (lev11,t11,t12,phi1,lev12,t13),
         `Fun (lev21,t21,t22,phi2,lev22,t23)) ->
          LEVar.add_alias lev11 lev21;
          LEVar.add_alias lev12 lev22;
          Const.add_both (create_c_eqtype t11 t21);
          Const.add_both (create_c_eqtype t12 t22);
          Const.add_both (create_c_eqtype t13 t23);
          Const.add_both (create_c_eqset phi1 phi2);
      | `Fun _,  (`Obj _  | `Undef | `Top   | `Bottom | `Int    )
      | `Obj _,  (`Fun _  | `Undef | `Top   | `Bottom | `Int    )
      | `Top,    (`Fun _  | `Undef | `Obj _ | `Bottom | `Int    ) 
      | `Undef,  (`Fun _  | `Top   | `Obj _ | `Bottom | `Int    ) 
      | `Bottom, (`Fun _  | `Top   | `Obj _ | `Undef  | `Int    ) 
      | `Int,    (`Fun _  | `Top   | `Obj _ | `Undef  | `Bottom ) 
          ->
          Const.is_false "Types are not equal"

  let simplify_eqobj (`EqObj (o1,o2)) = 
    let c = `EqObj (o1,o2) in
      match o1,o2 with
        | `ObjectType ltl, `OTVar otv
        | `OTVar otv, `ObjectType ltl ->
            Const.remove c;
            let dm = OTVar.domain otv in
            let dot = List.map fst ltl in
            let dm_not_ot = List.diff dm dot in
              if (not (List.is_empty dm_not_ot)) then
                ans_false "Object type domain is to small."
              else
                let dot_not_m = List.diff dot dm in
                  List.iter 
                    (fun label ->
                       ignore
                         (OTVar.find_or_add 
                            ~create:(fun () -> List.assoc label ltl)
                            ~key:label                            
                            ~map:otv)
                    )
                    dot_not_m
        | `OTVar otv1, `OTVar otv2 ->
            Const.remove c;
            OTVar.add_alias otv1 otv2
        | `ObjectType ltvl1, `ObjectType ltvl2 ->
            Const.remove c;
            let d1 = List.map fst ltvl1 in
            let d2 = List.map fst ltvl2 in
              if (not (List.compare_ignore_order d1 d2)) then
                Const.is_false "Object types do not have same domain."
              else
                List.iter 
                  (fun (l,tv1) -> 
                     let tv2 = List.assoc l ltvl2 in
                       TVar.add_alias tv1 tv2
                  )
                  ltvl1

  let simplify_disjoint (`Disjoint (phi1,phi2,_) as bc) = 
    match phi1,phi2 with
      | `LocSet ls, `LocSetVar lsv
      | `LocSetVar lsv, `LocSet ls ->
          Const.remove bc;
          LSVar.set_neg ls lsv
      | `LocSet ls1, `LocSet ls2 ->
          let inter = LSet.inter ls1 ls2 in
            if (LSet.is_empty inter) then
              Const.remove bc
            else
              ans_false "Disjoint location sets are not disjoint"
      | `LocSetVar lsv1, `LocSetVar lsv2 ->
          LSVar.disjoint lsv1 lsv2

  let simplify_subtype (`SubType (t1,t2,i) as bc) =
    let t = (t1,t2,i) in
      i := !i + 10;
      match (t1,t2) with
        | _, `Top -> Const.remove bc
        | `Bottom, _ -> Const.remove bc
        | `TypeVar tv1, `TypeVar tv2 -> 
            TVar.subset tv1 tv2
        | ((`Top | `Undef | `Fun _ | `Obj _ | `Int as t1),
           ( `Undef | `Fun _ | `Obj _ | `Bottom | `Int as t2)) ->
            begin
              match TypeR.subtype_tau t1 t2 with
              | Some false -> Const.is_false "Subtype is not possible"
              | Some true -> Const.remove bc
              | _ -> ()
            end
        | ((`Top | `Undef | `Fun _ | `Obj _ | `Int as t1), `TypeVar tv) ->
            TVar.set_lower t1 tv;
            Const.remove bc
        | (`TypeVar tv, ( `Undef | `Fun _ | `Obj _ | `Bottom | `Int as t1)) ->
            TVar.set_upper t1 tv;
            Const.remove bc

  let simplify_subpre (`SubPre (pv1,pv2,_)) =
    begin
      match PrVar.get_lower pv1 with
        | None -> ()
        | Some pr -> PrVar.set_lower pr pv2
    end;
    begin
      match PrVar.get_upper pv2 with
        | None -> ()
        | Some pr -> PrVar.set_upper pr pv1
    end


  let simplify_subenv (`SubEnv (lev1,lev2,lsref,_)) = 
    let lsv1 = LEVar.get_domain_var lev1 in
    let lsv2 = LEVar.get_domain_var lev2 in
      LSVar.add_alias lsv1 lsv2;
      LEVar.synchronize lev1;
      LEVar.synchronize lev2;
      let lower = 
        match LSVar.get_lower lsv1 with
          | None -> LSet.empty 
          | Some ls -> ls
      in
      let todo = LSet.diff lower !lsref in
        LSet.iter
          (fun l -> 
             lsref := LSet.add l !lsref;
             let bc = create_c_subobj 
               (LEVar.find_or_add OTVar.create l lev1) 
               (LEVar.find_or_add OTVar.create l lev2)
             in
               Const.add_both bc
          )
          todo

  let simplify_subobj (`SubObj (otv1,otv2,labsr,_)) =
    let add_const swap otv key img =
      let img2 = OTVar.find_or_add TVar.create key otv in
        if (not (Syntax.LabelSetRef.mem key labsr)) then begin
          Syntax.LabelSetRef.add key labsr;
          let bc = if swap then 
            create_c_subtype 
              (TypeR.create_t_tv img)
              (TypeR.create_t_tv img2)
          else
            create_c_subtype 
              (TypeR.create_t_tv img2)
              (TypeR.create_t_tv img)
          in
            Const.add_both bc
        end
    in
      OTVar.iter (add_const false otv2) otv1;
      OTVar.iter (add_const true otv1) otv2

  let simplify_eqdom (`DomainEq (phi,lev) as bc) = 
    let extend_lev lev ls = 
      let domain = LEVar.domain lev in
      let add_to_domain = List.diff (LSet.elements ls) domain in
        List.iter
          (fun loc -> ignore 
             (LEVar.find_or_add ~key:loc ~create:OTVar.create ~map:lev))
          add_to_domain
    in
      Const.remove bc;
      match phi,lev with
        | `LocSet ls, lev -> extend_lev lev ls
        | `LocSetVar lsv, lev ->
            let dlsv = LEVar.get_domain_var lev in
              LSVar.add_alias dlsv lsv;
              LEVar.synchronize lev;
              match LSVar.get_lower lsv with
                | Some ls -> extend_lev lev ls
                | None -> ()

  let simplify_eqlenv (`EqLenv (lenv1,lenv2) as bc) = match lenv1,lenv2 with
    | `LEnv lrl1, `LEnv lrl2 ->
        Const.remove bc;
        if (not (List.compare_ignore_order 
                   (List.map fst lrl1) 
                   (List.map fst lrl2)))
        then begin
          Const.is_false "These two environments are not equal"
        end else begin
          List.iter 
            (fun (l,r) -> 
               match List.assoc' l lrl2 with
                 | None -> 
                     Const.is_false "These two environments are not equal"
                 | Some r2 -> 
                     let nbc = create_c_eqobj r r2 in
                       Const.add nbc
            )
            lrl1
        end
    | `LEnvVar lev, `LEnv lrl 
    | `LEnv lrl, `LEnvVar lev -> 
        Const.remove bc;
        let dv = LEVar.get_domain_var lev in
          LSVar.set_eq (LSet.from_list (List.map fst lrl)) dv;
          LEVar.iter 
            (fun k otv -> 
               match List.assoc k lrl with
                 | `ObjectType (ltvl) -> 
                     List.iter
                       (fun (l,tv) ->
                          let tvm = OTVar.find_or_add TVar.create l otv in
                            TVar.add_alias tvm tv
                       )
                       ltvl
                       (* TODO: Check if there are entries in the mapping otv,
                          that are not part of the list ltvl *)
                 | `OTVar otv2 -> 
                     OTVar.add_alias otv otv2
            )
            lev

    | `LEnvVar lev1, `LEnvVar lev2 -> 
        LEVar.add_alias lev1 lev2;
        Const.remove bc


  let simplify_read_otv otv label tv =
    let tvimg = 
      OTVar.find_or_add 
        ~key:label 
        ~create:(fun () -> tv) 
        ~map:otv
    in
      if (TVar.compare tv tvimg != 0) then
        let bc = create_c_subtype 
          (TypeR.create_t_tv tvimg) 
          (TypeR.create_t_tv tv) 
        in
          Const.add_both bc

  let simplify_read_in lsv label tv () =
    let check_l l = 
      let otv = GEnv.find_or_add l in
        simplify_read_otv otv label tv
    in
      begin
        match LSVar.get_upper lsv with
          | None -> ()
          | Some ls ->
              if (LSet.cardinal ls == 1) then
                LSVar.set_lower ls lsv
      end;
      LSVar.lower_iter
        check_l
        lsv

  let simplify_read_ex lev label tv l =
    let otv = LEVar.find_or_add OTVar.create l lev in
      simplify_read_otv otv label tv

      
      
  let simplify_read (`Read (lev,prev,lsv,label,tv) as bc) =
    let ex () =
      LSVar.lower_one_element
        ~exact:(simplify_read_ex lev label tv)
        ~tomuch:(Const.is_false_u
                   ("Size of Location Set is bigger than one,"
                    ^ "but it is used as a location set following "
                    ^"a precise tag in a read constraint"))
        lsv
    in
    let ie = simplify_read_in lsv label tv in
    let make_ex pv prev = 
      match pv with
          | Some `In -> PrVar.set_eq `In prev
          | Some `Ex -> PrVar.set_eq `Ex prev
          | _ -> ()
    in
      make_ex (PrVar.get_lower prev) prev;
      make_ex (PrVar.get_upper prev) prev;
      PrVar.do_on_lower
        ~exact:ex
        ~inexact:ie
        ~nv:(do_f ~up:(Const.is_false_u 
                         ("Precise variable has T value as lower "
                          ^"bound in read constraint")))
        prev

  let check_pre_write_one lev l label tv lev2 =
    (* do everything needed for l' /= l, l' in dom(lev) 
       - add alias if both have an otv
       - add otv to the other variable if
       for one l' only one lev has the
       otv as an image.
    *)
    let _ = 
      LEVar.make_write_equal 
        Loc.compare 
        OTVar.add_alias
        lev
        lev2
        l
    in
      (* do everything needed for the l:
         - ensure that both lev has an otv as 
         an image for l, where
         otv = lev(l) and otv2 = lev2(l)
         - ensure that:
         dom(otv) \cup {l} = dom(otv2) \cap {l}
         - ensure that: 
         otv2(a) = t
         - ensure that 
         otv(a') = otv2(a') for all 'a /= a, 'a in dom(otv)
      *)
    let otv = LEVar.find_or_add OTVar.create l lev in
    let otv2 = LEVar.find_or_add OTVar.create l lev2 in
(*       print_endline ("Find or add: " ^ (LEVar.string_of_with_img lev)); *)
(*       print_endline ("Find or add: " ^ (LEVar.string_of_with_img lev2)); *)
    let _ = 
      OTVar.make_write_equal
        Syntax.compare_label
        TVar.add_alias
        otv
        otv2
        label
    in
      (* set otv2(label) = tv *)
      ignore
        (OTVar.find_or_add ~key:label ~create:(fun () -> tv) ~map:otv2)
        
  let simplify_write (`Write (lev, tv1, label, tv2, lev2)) = 
    let do_on_lower ?(nothing=fun () -> ()) ?(exact = fun () -> ()) 
        ?(inexact = fun () -> ()) ?(nv = fun _ -> ())
        = function
          | `In -> inexact ()
          | `Ex -> exact ()
          | #nv as n -> nv n
          | `PreVar pv -> 
              PrVar.do_on_lower ~nothing:nothing ~exact:exact ~inexact:inexact ~nv:nv pv
    in
    let do_on_upper ?(nothing=fun () -> ()) ?(exact = fun () -> ()) 
        ?(inexact = fun () -> ()) ?(nv = fun _ -> ())
        = function
          | `In -> inexact ()
          | `Ex -> exact ()
          | #nv as n -> nv n
          | `PreVar pv -> 
              PrVar.do_on_upper ~nothing:nothing ~exact:exact ~inexact:inexact ~nv:nv pv
    in
    let lower_one_element e tm = function
      | `LocSet ls -> 
          if (LSet.cardinal ls == 1) then
            e (LSet.choose ls)
          else
            tm ()
      | `LocSetVar lsv ->
          LSVar.lower_one_element
            ~exact:e
            ~tomuch:tm
            lsv
    in
    let lower_iter f = function
      | `LocSet ls -> LSet.iter f ls
      | `LocSetVar lsv -> LSVar.lower_iter f lsv
    in
    let do_sth q phi =
      do_on_lower
        ~exact:(fun () ->  
                  lower_one_element
                    (fun l ->
(*                        print_endline "exact, exact done"; *)
                       check_pre_write_one lev l label tv2 lev2) 
                    (Const.is_false_u 
                       ("Location set variable has a " 
                        ^"lower bound with more than one locations, " 
                        ^"but it is used in a write constraint with " 
                        ^"a precise variable assigned to @"))       
                    phi) 
        ~inexact:(fun () ->  
                    let tvt = TypeR.create_t_tv tv2 in 
                      lower_iter
                        (fun l ->  
                           let otv = GEnv.find_or_add l in 
                           let tv2 = OTVar.find_or_add  
                             ~create:TVar.create  
                             ~key:label  
                             ~map:otv  
                           in 
                           let tv2t = TypeR.create_t_tv tv2 in 
                           let bc = create_c_subtype tvt tv2t in 
                             Const.add bc) 
                        phi) 
        ~nv:(do_f ~up:(Const.is_false_u  
                         ("Precise variable has T value as lower " 
                          ^"bound in write constraint"))) 
        q; 
      do_on_upper 
        ~nv:(do_f ~down:(Const.is_false_u 
                           ("Precise variable has B value as upper " 
                            ^"bound in write constraint"))) 
        q
    in      
      TVar.do_on_upper
        ~something:(
          function 
            | `Obj(q,phi) -> do_sth q phi
            | _ -> ()
        )
        tv1
      
      
  let simplify_demotation_t (`DemotationT (t1,phi,t2) as bc) = 
    let add_upper_bound ?(r=true) tv t =
      match TypeR.demote_tau phi t with
        | None -> ()
        | Some tau ->
            if r then Const.remove bc;
            TVar.set_upper tau tv
    in
    let add_lower_bound ?(r=true) tv t =
      match TypeR.demote_tau phi t with
        | None -> ()
        | Some tau ->
            if r then Const.remove bc;
            TVar.set_lower tau tv
    in
    let set_demote_eq ?(r=true) tv t =
      match TypeR.demote_tau phi t with
        | None -> ()
        | Some tau ->
            if r then Const.remove bc;
            TVar.set_eq tau tv
    in


      match t1,t2 with
        | #TypeR.tau as t1, (#TypeR.tau as t2) -> () (* TODO *)
        | `TypeVar tv, (#TypeR.tau as t2) -> 
            set_demote_eq ~r:true tv t2
        | #TypeR.tau as t, `TypeVar tv -> () (* TODO *)
(*             TVar.do_on_upper *)
(*               ~something:( *)
(*                 fun t2 -> set_demote_upper phi  *)
                  
(*             TVar.do_on_exact *)
(*               ~something:( *)
(*                 fun t2 -> *)
(*                   match TypeR.get_lower_phi phi, TypeR.get_upper_phi phi with *)
(*                     | None,_ | _, None -> () *)
(*                     | Some ls1, Some ls2 -> *)
(*                         begin *)
(*                           match TypeR.is_demotation_tau t ls1 ls2 t2 with *)
(*                             | None -> () *)
(*                             | Some true ->  *)
(*                                 Const.remove bc *)
(*                             | Some false ->  *)
(*                                 Const.is_false *)
(*                                   "Demotation not possible." *)
(*                         end) *)
(*               tv *)
        | `TypeVar tv1, `TypeVar tv2 -> 
            TVar.do_on_upper
              ~something:(add_upper_bound tv1)
              tv2;
            TVar.do_on_lower
              ~something:(add_lower_bound tv1)
              tv2;
            TVar.do_on_exact
              ~something:(set_demote_eq tv1)
              tv2

  let simplify_demotation_e (`DemotationE (lev1,phi1,phi2,lev2,lsref) as bc) =
    let f l otv =
      let ls = match TypeR.get_upper_phi phi2 with
        | Some ls -> ls
        | None -> failwith "This should never happen"
      in
      if (not ((LSetRef.mem l lsref) || (LSet.mem l ls))) then begin
        LSetRef.add l lsref;
        let g = LEVar.find_or_add ~create:OTVar.create ~key:l in
        let bc = create_c_demotation_object
           (g ~map:lev1)
           phi1
           (g ~map:lev2)
        in
          Const.add_both bc
      end
    in
      LEVar.iter f lev1;
      LEVar.iter f lev2

  let simplify_demotation_o (`DemotationO (otv1,phi,otv2,lasref) as bc) =
    let f la _ =
      if (not (Syntax.LabelSetRef.mem la lasref)) then begin
        Syntax.LabelSetRef.add la lasref;
        let g m = TypeR.create_t_tv 
          (OTVar.find_or_add 
             ~create:TVar.create 
             ~key:la 
             ~map:m)
        in
        let bc = create_c_demotation_type
          (g otv1)
          phi
          (g otv2)
        in
          Const.add_both bc
      end
    in
      OTVar.iter f otv1;
      OTVar.iter f otv2

  let simplify_flowtype (`FlowType (lev,t1,t2) as bc) =
    match (t1,t2) with
      | _, `Top -> Const.remove bc
      | `Bottom, _ -> Const.remove bc
      | `Obj (`Ex, `LocSet ls1), `Obj (`In, `LocSet ls2) -> 
          begin
            let l = LSet.choose ls1 in
              match LEVar.find' l lev with
                | None -> ()
                | Some otv1 -> 
                    let otv2 = GEnv.find_or_add l in
                    let bc = create_c_flowobj lev otv1 otv2 in
                      Const.add_both bc;
                      ()
          end
      | `TypeVar tv1, `TypeVar tv2 -> 
(*           let varss () =  *)
(*             let tvarss = List.map  *)
(*               Vars.TVar.string_of_with_img *)
(*               (Vars.TVar.get_all_ts ()) *)
(*             in *)
(*             let pvars = List.map *)
(*               Vars.PrVar.string_of_with_img *)
(*               (Vars.PrVar.get_all_ts ()) *)
(*             in *)
(*             let lsvars = List.map *)
(*               Vars.LSVar.string_of_with_img *)
(*               (Vars.LSVar.get_all_ts ()) *)
(*             in *)
(*             let levs = List.map *)
(*               Vars.LEVar.string_of_with_img *)
(*               (Vars.LEVar.get_all_ts ()) *)
(*             in *)
(*             let otvs = List.map *)
(*               Vars.OTVar.string_of_with_img *)
(*               (Vars.OTVar.get_all_ts ()) *)
(*             in *)
(*               pvars @ tvarss @ lsvars @ levs @ otvs *)
(*           in *)
(*             print_endline (String.concat "; " (varss ())); *) (* TODO *)
            let set_lower tv = function
              | `Top | `Undef | `Fun _ | `Int as tau -> 
                  TVar.set_lower tau tv
              | `Bottom -> ()
              | `Obj _ -> () (* TODO *) 
            in
            let set_upper tv = function
              | `Bottom | `Undef | `Fun _ | `Int as tau ->
                  TVar.set_upper tau tv
              | `Top -> ()
              | `Obj _ -> () (* TODO *)
            in
              (* TODO: correct this *)
              TVar.do_on_lower ~something:(set_lower tv2) tv1;
              TVar.do_on_upper ~something:(set_upper tv1) tv2
      | ((`Top | `Undef | `Fun _ | `Obj _ | `Int as t1),
         ( `Undef | `Fun _ | `Obj _ | `Bottom | `Int as t2)) ->
          begin
            match TypeR.flow lev t1 t2 with
              | Some false -> Const.is_false "Flow is not possible"
              | Some true -> Const.remove bc
              | _ -> ()
          end
      | ((`Top | `Undef | `Fun _ | `Int as t1), `TypeVar tv) ->
          TVar.set_lower t1 tv;
          Const.remove bc
      | (`TypeVar tv, ( `Undef | `Fun _ | `Bottom | `Int as t1)) ->
          TVar.set_upper t1 tv;
          Const.remove bc
      | `TypeVar tv, (`Obj _ as t1) -> () (* TODO *)
      | (`Obj _ as t1, `TypeVar tv) -> () (* TODO *)

  let simplify_flowobj (`FlowObj (lev,otv1,otv2) as bc) =
    OTVar.iter 
      (fun a img -> 
         let img2 = OTVar.find_or_add 
           ~create:TVar.create
           ~key:a
           ~map:otv2 
         in
         let bc = create_c_flow lev 
           (TypeR.create_t_tv img) 
           (TypeR.create_t_tv img2) 
         in
           Const.add_both bc
      )
      otv1;
    OTVar.iter 
      (fun a img2 -> 
         let img = OTVar.find_or_add 
           ~create:TVar.create
           ~key:a
           ~map:otv1 
         in
         let bc = create_c_flow lev 
           (TypeR.create_t_tv img) 
           (TypeR.create_t_tv img2) 
         in
           Const.add_both bc
      )
      otv2
      
  let fire_eq : eq -> unit = function
(*t*) | `EqSet _ as bc -> simplify_eqset bc
(*t*) | `EqPres _ as bc -> simplify_eqpres bc
(*t*) | `EqType _ as bc -> simplify_eqtype bc
(*t*) | `EqObj _ as bc -> simplify_eqobj bc
(* *) | `EqLenv _ as bc -> simplify_eqlenv bc


  let fire_bools : bools -> unit = function
      | `False (Some s) -> Const.is_false s
      | `False None -> Const.is_false "No reason given"
      | `True as bc -> Const.remove bc; ()

  let fire_set : set -> unit = function
(*t*) | `Exclude (l,phi) as c -> Const.remove c; do_exclude (l,phi)
(*t*) | `Include (l,phi) as c -> Const.remove c; do_include (l,phi)
(*t*) | `Subset s as c -> simplify_subset c
(*t*) | `Disjoint phi as c -> simplify_disjoint c

  let fire_prop : prop -> unit = function
      | `Write _ as bc -> simplify_write bc
      | `Read _ as bc -> simplify_read bc
  let fire_demotation : demotation -> unit = function
      | `DemotationT _ as bc -> simplify_demotation_t bc
      | `DemotationE _ as bc -> simplify_demotation_e bc
      | `DemotationO _ as bc -> simplify_demotation_o bc
  let fire_subtype : subtype -> unit = function
(*t*) | `SubType _ as bc -> simplify_subtype bc
      | `SubPre _ as bc -> simplify_subpre bc 
      | `SubEnv _ as bc -> simplify_subenv bc
      | `SubObj _ as bc -> simplify_subobj bc
  let fire_flow : flow -> unit = function
      | `FlowType _ as fc -> simplify_flowtype fc
      | `FlowObj _ as fc -> simplify_flowobj fc
  let fire_domain : domain -> unit = function
(* *) | `DomainEq _ as bc -> simplify_eqdom bc
      | `DomainEmptyObj (lev1, lev2, l) -> 
          let d1 = LEVar.domain lev1 in
          let d2 = LEVar.domain lev2 in
          let d = List.union d1 d2 in
          let d = List.add l d in
            List.iter
              (fun l1 -> 
                 if Loc.compare l1 l != 0 then begin
                   let otv1 = LEVar.find_or_add OTVar.create l1 lev1 in
                   let otv2 = LEVar.find_or_add OTVar.create l1 lev2 in
                     OTVar.add_alias otv1 otv2
                 end else begin
                   let otv = LEVar.find_or_add OTVar.create l lev1 in
                   let d = OTVar.domain otv in
                     match d with
                       | [] -> ()
                       | _ -> Const.is_false "Domain empty Object Constraint is violated"
                 end)
              d
            

  let fire_vars : vars -> unit = function
    | `VTVar tv -> 
        (* print_endline ("fire_var: " ^ (TVar.string_of_with_img tv)); *)
        TVar.do_normalize tv
    | `VLSetVar lsv -> LSVar.do_normalize lsv
    | `VPreVar pv -> PrVar.do_normalize pv
    | `VOTVar otv -> OTVar.do_normalize otv
    | `VLEVar lev -> LEVar.do_normalize lev

  let fire_locs : locs -> unit = function
    | `Locs (phi,t) ->
        let ls_t = TypeR.get_locs t in
          begin
            match phi with
              | `LocSet ls -> 
                  if (LSet.subset ls_t ls) then 
                    ()
                  else
                    Const.is_false 
                      ("The Location set is not a subset of "
                       ^"all locations reachable throw the type.")
              | `LocSetVar lsv -> LSVar.set_lower ls_t lsv
          end

  let fire : obs -> unit = function
    | #vars as v -> fire_vars v
    | #set as s -> fire_set s
    | #eq as eq -> fire_eq eq
    | #bools as b -> fire_bools b
    | #prop as p -> fire_prop p
    | #demotation as d -> fire_demotation d
    | #subtype as st -> fire_subtype st
    | #flow as f -> fire_flow f
    | #domain as d -> fire_domain d
    | #locs as bc -> fire_locs bc

  let get_prio = function
    | `False _ -> 1
    | `True -> 2
    | #vars -> 10
    | `EqSet _ | `EqPres _ | `EqType _ | `EqObj _ -> 10
    | `Exclude _ | `Include _ -> 15
    | `Subset _ | `Disjoint _ -> 20
    | `DomainEq _ -> 25
    | `SubType (_,_,i) -> !i 
    | `EqLenv _ -> 30
    | _ -> 400

  let compare_vars : vars -> vars -> int = Pervasives.compare

  let compare_obs obs1 obs2 = match (obs1,obs2) with
    | #t as t1, (#t as t2) -> compare t1 t2
    | #t, _ -> 1
    | _, #t -> -1
    | #vars as v1, (#vars as v2) -> compare_vars v1 v2
  let string_of_obs = function
    | #t as t -> string_of t
    | #vars as v -> string_of_vars v

  let obs_to_t : obs -> t option = function
    | #vars -> None
    | #t as t -> Some t


end

and Const : 
sig
  type obs 
  val add : ConstBasic.t -> unit
  val is_false : string -> unit
  val is_false_u : string -> unit -> unit
  val remove : ConstBasic.t -> unit
  val remove_wl : ConstBasic.t -> unit

  val add_wl : obs -> unit
  val add_both : ConstBasic.t -> unit

  val solve : unit ->  unit 
  val string_of : unit -> string

  val reset : unit -> unit
  val full_visit : unit -> unit

end
  with type obs = ConstBasic.obs
= struct  
  type obs = ConstBasic.obs

  module C = struct   
    module BCs = struct
      type t = ConstBasic.t
      let compare bc1 bc2 = 
(*         print_endline "Compare:"; *)
(*         print_endline (ConstBasic.string_of bc1); *)
(*         print_endline (ConstBasic.string_of bc2); *)
        ConstBasic.compare bc1 bc2
      let sep = "; "
      let string_of = ConstBasic.string_of
    end
    module CSet = ExtSSet.Make(BCs)
    type t = 
        [ `True
        | `False of string option
        | `Set of CSet.t 
        ]

    let normalize = function
      | `True -> `True
      | `False s -> `False s
      | `Set s ->
          if (CSet.is_empty s) then
            `True
          else
            `Set s

    let string_of = function
      | `True -> "true"
      | `False _ -> "false"
      | `Set s -> CSet.string_of s

    let add (c: t) (bc : ConstBasic.t) = match (c,bc) with
      | `False _, _ | _, `True -> c
      | _, `False str -> `False str
      | `True, bc -> `Set (CSet.singleton bc)
      | `Set s, bc -> `Set (CSet.add bc s)

    let mem bc c = 
      match (bc,c) with
        | `True, `True -> true
        | _, `True -> false
        | _, `False _ -> true
        | bc, `Set s -> CSet.mem bc s

    let remove bc (`Set cs) = normalize (`Set (CSet.remove bc cs))
    let iter f = function 
      | ` True | `False _ -> ()
      | `Set cs -> CSet.iter f cs
  end

  module PQ = PrioQueue.Make(
    struct 
      type t = int 
      let compare = (-) 
      let string_of = string_of_int
    end)(
    struct
      type t = ConstBasic.obs
      let compare = ConstBasic.compare_obs
      let string_of = ConstBasic.string_of_obs
      let hash = Hashtbl.hash
    end)
        
  type state = {
    mutable const: C.t;
    qu: PQ.t;
  }


  let s = { 
    const = `True;  
    qu = PQ.create ();
  }

  let reset () = 
    PQ.clear s.qu;
    s.const <- `True

  let string_of () =  C.string_of s.const

  let dump () =
    ("Constraint: " ^ string_of () ^ "\n") ^ 
    ("Workliste: " ^ PQ.string_of s.qu)


  let abstract_add f bc = 
    (* pstart "abstract add"; *)
    let _ = 
      match bc with
        | `True -> ()
        | `False str -> 
            PQ.clear s.qu;
            s.const <- `False str
        | bc -> f bc
    in
      (* pende "abstract add" *) ()

  let add_to_wl (obs : obs) =
    match obs with
      | #ConstBasic.t as t -> 
(*           pstart ("add_to_wl: " ^ (ConstBasic.string_of_obs obs)); *)
          if (C.mem t s.const) then begin
(*             pdebug ("mid_to_wl: " ^ (ConstBasic.string_of_obs obs)); *)
            PQ.push (ConstBasic.get_prio obs) obs s.qu;
          end;
          (* pdebug (dump ()); *)
(*           pende "add_to_wl" *)
      | #ConstBasic.vars as v -> 
          PQ.push (ConstBasic.get_prio v) obs s.qu
  let add_to_set bc = 
    s.const <- C.add s.const bc;
    ConstBasic.register_obs bc
    (* pdebug (ConstBasic.string_of bc) *)


  let add_wl (bc : obs)  = 
(*     pstart ("add_wl: " ^ (ConstBasic.string_of_obs bc)); *)
    abstract_add add_to_wl bc
(*     pende "add_wl" *)
    let add    bc   = 
    (* pstart "add"; *)
    abstract_add add_to_set bc
    (* pende "add" *)

  let add_both bs = add bs; add_wl (bs :> obs)

  let is_false str = add (ConstBasic.c_false (Some str))

  let remove bc =
    (* pstart ("remove: " ^ (ConstBasic.string_of bc)); *)
    let _ = 
      match s.const with
        | `True | `False _ -> ()
        | `Set cs -> 
            ConstBasic.unregister_obs bc;
            PQ.remove (bc :> ConstBasic.obs) s.qu;
            s.const <- C.remove bc (`Set cs)
    in
      (* pende "remove" *) ()
  let remove_wl bc =
    (* pstart ("remove from wl: " ^ (ConstBasic.string_of bc)); *)
    let _ =
      match s.const with
        | `True | `False _ -> ()
        | `Set cs ->
            PQ.remove (bc :> ConstBasic.obs) s.qu
    in
      (* pende "remove_wl" *) ()
      
  let full_visit () =
(*     pstart "full_visit"; *)
    let _ = C.iter (fun bc -> add_wl (bc :> obs)) s.const in
      (* pdebug (dump ()); *)
(*       pende "full_visit"; *)
      ()
        
  exception NoSol of string

  let get_next () = 
    (* pstart "get_next"; *)
    let n = PQ.pop s.qu in
      (* pende "get_next"; *)
      n

  let on_step () =
      try
        let bc = get_next () in
          begin
            match bc with
              | #ConstBasic.t as bc -> 
                  pdebug ((ConstBasic.string_of bc));
              | _ -> ();
          end;
          ConstBasic.fire (bc :> ConstBasic.obs);
          (* pdebug (dump ()); *)
      with e -> begin
        match e with
          | Simplify_False s | GenVars.EUnion s -> is_false s
          | Queue.Empty -> ()
          | e -> raise e
      end

  let rec iter () =
    on_step ();
    if PQ.is_empty s.qu then
      ()
    else
      iter ()
                  
        
  let solve () = 
    full_visit ();
    iter ()

  let is_false_u s = 
    fun () -> is_false s

end

and GEnv : sig
  type t
  val find : Loc.t -> Vars.OTVar.t option
  val find_or_add : Loc.t -> Vars.OTVar.t
  val string_of : unit -> string
  val reset : unit -> unit
end
= struct
  module M = 
    OwnMap.AddS
      (struct
         type t = Loc.t
         let string_of = Loc.string_of
       end)
      (struct
         type t = Vars.OTVar.t
         let string_of = Vars.OTVar.string_of_with_img
       end)
      (OwnMap.Make
         (struct
            type t = Loc.t
            let compare = Loc.compare
          end)
         (struct
            type t = Vars.OTVar.t
            let compare = Vars.OTVar.compare
          end))
  type t = M.t
  let map = ref M.empty
  let reset () = 
    map := M.empty

  let add l o = map := M.add l o !map
  let find l = M.find' l !map
  let find_or_add l = 
    match find l with
      | None ->
          let otv = Vars.OTVar.create () in
            add l otv;
            otv
      | Some otv -> otv
  let string_of () = M.string_of !map
end

(* This module is exactly the same as the TypeR module except 
   that is contains some value definitions. This is not allowed
   in the recursive module definition of TypeR. So there you
   have to use the polymorphic variant directly or use the
   create_t_top, ... functions to create the values.
*)
module Type = struct
  include TypeR
  let t_top = `Top
  let t_undef = `Undef
  let t_int = `Int
  let t_bottom = `Bottom
  let q_in = `In
  let q_ex = `Ex
  let empty_lenv = `LEnv []
  let empty_obj = `ObjectType []
end

type lsvar = Vars.LSVar.t
type prevar = Vars.PrVar.t
type otvar = Vars.OTVar.t
type levar = Vars.LEVar.t
type tvar = Vars.TVar.t


(* ================================================================= *)
(* ================================================================= *)
(* ================================================================= *)
(* ================================================================= *)
(* ================================================================= *)
(* ================================================================= *)
(*                                                                   *)
(* ######  ######  ######  ######      ##    ######  ######    ##    *)
(*   ##    ##      ##        ##       ####   ##  ##  ##       ####   *)
(*   ##    ####    ######    ##      ##  ##  ######  ####    ##  ##  *)
(*   ##    ##          ##    ##      ######  ####    ##      ######  *)
(*   ##    ######  ######    ##      ##  ##  ##  ##  ######  ##  ##  *)
(*                                                                   *)
(* ================================================================= *)
(* From here we collect all the test code for this file.             *)
(* At most you will find unit tests. To exectue the test call the    *)
(* binary with the command line option --test.                       *)
(* ================================================================= *)
(* ================================================================= *)

include TestUtils.Make(
  struct 
    let reset () = 
      Location.reset_all ();
      Vars.reset ();
      Const.reset ()
  end)

module TypeTest_Basics = struct
  open Vars
  open Type
  open ExtUtils
  open Test
  open TestUtils

  let init_tests () = 
    let t_top = t_top in
    let t_undef = t_undef in
    let tv = TVar.create () in
    let lv = LEVar.create () in
    let lsv = LSVar.create () in
    let pv = PrVar.create () in
    let phi1 = create_phi_lsvar lsv in
    let phi2 = create_phi_ls LSet.empty in
    let l1 = create_phi_ls LSet.empty in
    let l2 = create_phi_ls (LSet.singleton (Loc.create ())) in
(*     let p1 = create_p_inexact (Location.LSet.empty) in *)
(*     let p2 = create_p_exact (Location.Loc.create ()) in *)
(*     let p3 = create_p_var q_in phi1 in   *)
    let assert_string s1 s2 = 
      assert_equal
        ~cmp:(Utils.compare_to_equal String.compare)
        ~printer:(fun x -> x)
        s1
        s2
    in
      
    let t1 () =
      let t_obj = create_t_obj q_in l1 in  
      let t_fun = create_t_fun lv t_undef t_undef phi1 lv t_top in
      let s_top = string_of t_top in
      let s_undef = string_of t_undef in

      let s_obj = string_of (t_obj :> Type.t) in
      let s_fun = string_of t_fun in
        assert_string "T" s_top;
        assert_string "undefined" s_undef;
        assert_string "obj(~{})" s_obj;
        assert_string 
          "(le_0,undefined x undefined)-mu_0->(le_0,T)"
          s_fun
    in
    let t2 () =
      let s_p1 = string_of_p (q_in,l1) in
      let s_p2 = string_of_p (q_ex,l2) in
      let s_p3 = string_of_p (q_in,phi1) in
        assert_string "~{}" s_p1;
        assert_string "@{l0}" s_p2;
        assert_string "~mu_0" s_p3;
    in
    let t3 () =
      let s_phi1 = string_of_phi phi1 in
      let s_phi2 = string_of_phi phi2 in
        assert_string "mu_0" s_phi1;        assert_string "{}" s_phi2;
    in
    let t4 () =
      let s_qin = string_of_q (q_in) in
      let s_qex = string_of_q (q_ex) in
      let s_prev = string_of_q (create_q_pv pv) in
        assert_string "~" s_qin;
        assert_string "@" s_qex;
        assert_string "pv_0" s_prev;
    in
    let t5 () =
      let le_e = empty_lenv in
      let le_oe = 
        (add_to_lenv 
           le_e (Loc.create ()) empty_obj)
      in
      let le_te = 
        (add_to_lenv 
           le_oe (Loc.create ()) empty_obj)
      in
      let le_oe_2 = 
        (add_to_lenv 
           le_e (Loc.create ()) empty_obj)
      in
      let le_var = create_lenv_levar (LEVar.create ()) in
      let le_three_e = union_lenv le_oe_2 le_te in
      let s_le_e = string_of_lenv le_e in
      let s_le_oe = string_of_lenv le_oe in
      let s_le_te = string_of_lenv le_te in  
      let s_le_three_e = string_of_lenv le_three_e in  
      let s_le_v = string_of_lenv le_var in
        assert_string "[]" s_le_e;
        assert_string "[l0->{}]" s_le_oe;
        assert_string "[l1->{},l0->{}]" s_le_te;
        assert_string "[l2->{},l1->{},l0->{}]" s_le_three_e;
        assert_string "le_0" s_le_v;
    in
    let t6 () = 
      let o_e = empty_obj in
      let o_h = add_to_obj o_e (Syntax.create_label "hallo") tv in
      let s_o_e = string_of_r o_e in
      let s_o_h = string_of_r o_h in
        assert_string "{}" s_o_e;
        assert_string "{hallo:tv_0}" s_o_h;
        
    in
      wrap_list
        [("type create test, type string_of test", t1);
         ("type create test, type string_of_p test", t2);
         ("type create test, type string_of_phi test", t3);
         ("type create test, type string_of_q test", t4);
         ("type create test, type string_of_lenv test", t5);
         ("type create test, type string_of_r test", t6);
        ] 
   
  let _ =
    install_tests "Inf.Type_Basics" (wrap_reset init_tests) 

end

module TypeTest_Demotation = struct
  open Vars
  open Type
  open ExtUtils
  open Test
  open TestUtils

  let init_tests () =
    let assert_boolo s bo1 bo2 =
      assert_equal
        ~cmp:(=)
        ~printer:(function
                    | Some true -> "true" 
                    | Some false -> "false"
                    | None -> "None" 
                 )
        ~msg:s
        bo1
        bo2
    in
    let assert_boolo_true s bo = assert_boolo s (Some true) bo in
    let assert_boolo_false s bo = assert_boolo s (Some false) bo in
    let assert_boolo_none s bo = assert_boolo s None bo in

    let string_of t1 t2 =
      (string_of (t1 :> Type.t)), (string_of (t2 :> Type.t))
    in

    let t1 () = 
      let t1 = Type.t_undef in
      let t2 = Type.t_top  in
        assert_boolo_true 
          "undef :=# T should be evaluate to true"
          (Type.is_demotation_tau t1 (LSet.empty) (LSet.empty) t2);
        assert_boolo_true
          "B :=# t should be evaluate to true"
          (Type.is_demotation_tau Type.t_bottom LSet.empty LSet.empty Type.t_undef);
        ()          
    in

    let t2 () =
      let l1 = Loc.create () in
      let phi1 = Type.create_phi_ls (LSet.singleton l1) in
      let phi2 = Type.create_phi_ls (LSet.singleton (Loc.create ())) in
      let t1 = Type.create_t_obj_tau Type.q_ex phi1 in
      let t2 = Type.create_t_obj_tau Type.q_in phi2 in
      let t3 = Type.t_undef in
      let s1,s2 = string_of t1 t2 in
      let s3 = Type.string_of (t3 :> Type.t) in
        assert_boolo_false
          (s1^" :=# "^ s2 ^ " should be evaluated to false")
          (Type.is_demotation_tau t1 (LSet.empty) (LSet.empty) t2);
        assert_boolo_false
          (s2^" :=# "^ s1 ^ " should be evaluated to false")
          (Type.is_demotation_tau t2 (LSet.empty) (LSet.empty) t1);
        assert_boolo_false
          (s1^" :=# "^ s3 ^ " should be evaluated to false")
          (Type.is_demotation_tau t1 (LSet.empty) (LSet.empty) t3);
        assert_boolo_true
          (s1^" :=# "^ s1 ^ " should be evaluated to true")
          (Type.is_demotation_tau t1 (LSet.empty) (LSet.empty) t1);
        assert_boolo_none
          (s1^" :=# "^ s1 ^ " should be evaluated to none")
          (Type.is_demotation_tau t1 (LSet.empty) (LSet.singleton l1) t1);
        assert_boolo_false
          (s1^" :=# "^ s1 ^ " should be evaluated to false")
          (Type.is_demotation_tau t1 (LSet.singleton l1) (LSet.singleton l1) t1)
    in

    let t3 () =
      let l1 = Loc.create () in
      let phi1 = Type.create_phi_ls (LSet.singleton l1) in
      let t1 = Type.create_t_obj_tau Type.q_in phi1 in
      let t2 = Type.create_t_obj_tau Type.q_ex phi1 in
      let s1,s2 = string_of t1 t2 in
        assert_boolo_true
          (s1 ^ " :=# " ^ s2 ^ "should be evaluated to true")
          (Type.is_demotation_tau t1 (LSet.singleton l1) (LSet.singleton l1) t2);
        assert_boolo_none
          (s1 ^ " :=#_{}^{l} " ^ s2 ^ "should be evaluated to none")
          (Type.is_demotation_tau t1 (LSet.empty) (LSet.singleton l1) t2);
        assert_boolo_false
          (s1 ^ " :=#_{}^{} " ^ s2 ^ "should be evaluated to false")
          (Type.is_demotation_tau t1 (LSet.empty) (LSet.empty) t2)
    in

    let t4 () =
      let l1 = Loc.create () in
      let l2 = Loc.create () in
      let ls  = LSet.add l2 (LSet.singleton l1) in
      let phi = Type.create_phi_ls ls in
      let t1 = Type.create_t_obj_tau Type.q_in phi in
      let t2 = Type.create_t_obj_tau Type.q_ex phi in
      let s1,s2 = string_of t1 t2 in
        assert_boolo_false
          (s1 ^ " :=# " ^ s2 ^ " contains invalid types, so this should ealuated into false")
          (Type.is_demotation_tau t1 ls ls t2)
    in

      wrap_list
        [("type test, is_demotation_tau, T,B", t1);
         ("type test, is_demotation_tau, obj, ex_in, ex_ex", t2);
         ("type test, is_demotation_tau, obj, in_ex", t3);
         ("type test, is_demotation_tau, obj, failiures", t4);
        ] 
        

  let _ =
    install_tests "Inf.Type_Demotation" (wrap_reset init_tests) 


end


module TypeTest_normalize = struct
  open Vars
  open Type
  open ExtUtils
  open Test
  open TestUtils

  let init_tests () =
    let t1 () = 
      let phi = Type.create_phi_ls (LSet.empty) in
      let t1 = `Obj (`NVUp, phi) in
      let t2 = Type.normalize_tau t1 in
        assert_equal
          ~cmp:(Utils.compare_to_equal Type.compare_tau)
          ~printer:(Type.string_of_tau)
          ~msg:("Normalization value wrong!")
          t2
          Type.t_top
    in
      
    let t2 () =
      let phi = Type.create_phi_ls (LSet.empty) in
      let t1 = `Obj (`NVDown, phi) in
      let t2 = Type.normalize_tau t1 in
        assert_equal
          ~cmp:(Utils.compare_to_equal Type.compare_tau)
          ~printer:(Type.string_of_tau)
          ~msg:("Normalization value wrong!")
          t2
          Type.t_bottom
    in

    let t3 () =
      let l = Loc.create () in
      let ls = LSet.singleton l in
      let phi = Type.create_phi_ls ls in
      let prev = PrVar.create () in
      let t = Type.create_t_obj_tau (Type.create_q_pv prev) phi in
      let _ = PrVar.set_lower `In prev in
      let _ = PrVar.set_upper `In prev in
      let t2 = Type.normalize_tau t in
        assert_equal
          ~cmp:(Utils.compare_to_equal Type.compare_tau)
          ~printer:(Type.string_of_tau)
          ~msg:("Normalization value wrong!")
          (Type.create_t_obj_tau `In phi)
          t2
    in

    let t4 () = 
      let l = Loc.create () in
      let ls = LSet.add (Loc.create ()) (LSet.singleton l) in
      let phi = Type.create_phi_ls ls in
      let tv = TVar.create () in
      let t = Type.create_t_tv tv in
      let ty = Type.create_t_obj_tau `In phi in
      let _ = TVar.set_lower ty tv in
      let _ = TVar.set_upper ty tv in
      let tr = Type.normalize t in
        assert_equal
          ~cmp:(Utils.compare_to_equal Type.compare)
          ~printer:(Type.string_of)
          ~msg:("Normalization value wrong!")
          (ty :> Type.t)
          tr;
        let lev = LEVar.create () in
        let tf = Type.create_t_fun lev t t phi lev Type.t_undef in
        let tfr = Type.normalize tf in
        let ty = (ty :> Type.t) in
        assert_equal
          ~cmp:(Utils.compare_to_equal Type.compare)
          ~printer:(Type.string_of)
          ~msg:("Normalization value wrong!")
          (Type.create_t_fun lev ty ty phi lev Type.t_undef)
          tfr
    in

      wrap_list
        [("type test, normalize, `Obj(`NVUp,_)", t1);
         ("type test, normalize, `Obj(`NVDown,_)", t2);
         ("type test, normalize, remove PrVar, if PrVar_ = PrVar^", t3);
         ("type test, normalize, remove TVar, if TVar_ = TVar^", t4);
        ]

  let _ =
    install_tests "Inf.Type_Normalization" (wrap_reset init_tests) 

end

module TypeTest_SubtypeFlow = struct
  open Vars
  open Type
  open ExtUtils
  open Test
  open TestUtils

  let init_tests () =
    let bo_to_string = function
      | Some true -> "true" 
      | Some false -> "false"
      | None -> "None" 
    in        
    let assert_boolo s bo1 bo2 =
      assert_equal
        ~cmp:(=)
        ~printer:bo_to_string
        ~msg:s
        bo1
        bo2
    in
    let string_of t1 t2 =
      (string_of (t1 :> Type.t)), (string_of (t2 :> Type.t))
    in
    let t_subtype (bo : bool option) (t1 : t) (t2 : t) =
      let s1,s2 = string_of t1 t2 in
        assert_boolo
          (s1 ^ " <: " ^ s2 ^ " -> " ^ bo_to_string bo )
          bo
          (Type.subtype t1 t2);
        match bo with
          | Some true ->
              let lev = LEVar.create () in
                assert_boolo
                  (s1 ^ " <| " ^ s2 ^ " -> " ^ bo_to_string bo)
                  (Some true)
                  (Type.flow lev t1 t2)
          | _ -> ()
    in


    let t1 () = 
      t_subtype (Some true) Type.t_undef Type.t_top;
      t_subtype (Some true) Type.t_bottom Type.t_undef
    in
    let t2 () =
      let phi = Type.create_phi_ls (LSet.singleton (Loc.create ())) in
      let tobj = Type.create_t_obj `In phi in
        t_subtype (Some true) Type.t_undef Type.t_undef;
        t_subtype (Some false) Type.t_undef Type.t_bottom;
        t_subtype (Some false) tobj Type.t_undef;
        t_subtype (Some false) Type.t_undef tobj
          
    in

    let t3 () =
      let l0 = LSet.singleton (Loc.create ()) in
      let l1 = LSet.singleton (Loc.create ()) in
      let l2 = LSet.singleton (Loc.create ()) in
      let l3 = LSet.singleton (Loc.create ()) in
      let t1 = Type.create_t_obj `Ex (Type.create_phi_ls l0) in
      let t2 = Type.create_t_obj `Ex (Type.create_phi_ls l1) in
      let t3 = Type.create_t_obj `In (Type.create_phi_ls l0) in
      let t4 = Type.create_t_obj `In (Type.create_phi_ls l1) in
      let t5 = Type.create_t_obj `In 
        (Type.create_phi_ls (LSet.union l2 (LSet.union l0 l1))) 
      in
      let t6 = Type.create_t_obj `In
        (Type.create_phi_ls (LSet.union l3 (LSet.union l0 l1)))
      in
        t_subtype (Some true) t1 t1;
        t_subtype (Some false) t1 t2;
        t_subtype (Some false) t1 t3;
        t_subtype (Some false) t1 t4;
        t_subtype (Some false) t1 t5;
        t_subtype (Some false) t1 t6;

        t_subtype (Some false) t2 t1;
        t_subtype (Some true) t2 t2;
        t_subtype (Some false) t2 t3;
        t_subtype (Some false) t2 t4;
        t_subtype (Some false) t2 t5;
        t_subtype (Some false) t2 t6;

        t_subtype (Some false) t3 t1;
        t_subtype (Some false) t3 t2;
        t_subtype (Some true) t3 t3;
        t_subtype (Some false) t3 t4;
        t_subtype (Some true) t3 t5;
        t_subtype (Some true) t3 t6;

        t_subtype (Some false) t4 t1;
        t_subtype (Some false) t4 t2;
        t_subtype (Some false) t4 t3;
        t_subtype (Some true) t4 t4;
        t_subtype (Some true) t4 t5;
        t_subtype (Some true) t4 t6;

        t_subtype (Some false) t5 t1;
        t_subtype (Some false) t5 t2;
        t_subtype (Some false) t5 t3;
        t_subtype (Some false) t5 t4;
        t_subtype (Some true) t5 t5;
        t_subtype (Some false) t5 t6;

        t_subtype (Some false) t6 t1;
        t_subtype (Some false) t6 t2;
        t_subtype (Some false) t6 t3;
        t_subtype (Some false) t6 t4;
        t_subtype (Some false) t6 t5;
        t_subtype (Some true) t6 t6;
    in

    let t4 () =
      let pv = PrVar.create () in
      let phi = Type.create_phi_ls (LSet.singleton (Loc.create ())) in
      let t1 = Type.create_t_obj (`PreVar pv) phi in
        t_subtype (Some true) t1 t1
    in
    let t5 () =
      let phi = Type.create_phi_ls (LSet.singleton (Loc.create ())) in
      let lev = LEVar.create () in
      let lev2 = LEVar.create () in
      let tv = TVar.create () in
      let tv_t = Type.create_t_tv tv in
      let tv2_t = Type.create_t_tv (TVar.create ()) in
      let t1 = Type.create_t_fun lev tv_t tv_t phi lev tv_t in
      let t2 = Type.create_t_fun lev2 tv_t tv_t phi lev tv_t in
      let t3 = Type.create_t_fun lev tv2_t tv_t phi lev tv_t in
        t_subtype (Some true) t1 t1;
        t_subtype None t1 t2;
        t_subtype None t2 t1;
        t_subtype None t1 t3;
    in

      wrap_list
        [("type test, Subtype, Top and Bottom", t1);
         ("type test, Subtype, Undef", t2);
         ("type test, Subtype, `Obj", t3);
         ("type test, Subtype, var test", t4);
         ("type test, Subtype, var test 2", t5);
        ]

  let _ =
    install_tests "Inf.Type_Subtype" (wrap_reset init_tests) 

end

module ConstTest = struct
  open Vars
  open Type
  open TestUtils
  open Test
  open ExtUtils
  open ExtString

  let init_tests () =
    let new_const bc = Const.add bc; () in
(*     let new_const' bc = Const.add bc; bc in *)

    let create_init_vars () = 
      let lev = LEVar.create () in
      let lsv = LSVar.create () in
      let phi = create_phi_lsvar lsv in
      let type1 = t_undef in
      let type2 = t_top in
        lev,lsv,phi,type1,type2
    in
(*     let assert_string s1 s2 =  *)
(*       assert_equal *)
(*         ~cmp:(Utils.compare_to_equal String.compare) *)
(*         ~printer:(fun x -> x) *)
(*         s1 *)
(*         s2 *)
(*     in *)
    let assert_const c1 c2 =
      let c1l = String.nsplit c1 "; " in
      let c2l = String.nsplit c2 "; " in
      let cmp = List.compare_ignore_order 
          ~equal:(Utils.compare_to_equal String.compare)
      in
        assert_equal
          ~cmp:cmp
          ~printer:(fun sl -> String.concat "; " sl)
          c1l
          c2l
    in
      
    let t1 () = 
      let s_true = ConstBasic.string_of (ConstBasic.c_true) in
      let s_false = ConstBasic.string_of (ConstBasic.c_false None) in
        assert_const "TRUE" s_true;
        assert_const "FALSE" s_false;
    in
    let t2 () =
      let lev,lsv,phi,type1,type2 = create_init_vars () in
      let s_dem_lenv = ConstBasic.string_of 
        (ConstBasic.create_c_demotation_lenv lev phi lev)
      in
      let s_dem_type = ConstBasic.string_of
        (ConstBasic.create_c_demotation_type type1 phi type2)
      in
        assert_const "le_0 :=#mu_0 le_0" s_dem_lenv;
        assert_const "undefined :=#mu_0 T" s_dem_type
    in
    let t3 () =
      let lev,lsv,phi,type1,type2 = create_init_vars () in
      let s_include = ConstBasic.string_of
        (ConstBasic.create_c_include (Location.Loc.create ()) phi)
      in
      let s_exclude = ConstBasic.string_of
        (ConstBasic.create_c_exclude (Location.Loc.create ()) phi)
      in
        assert_const "l0 IN mu_0" s_include;
        assert_const "l1 EX mu_0" s_exclude;
    in
    let t4 () =
      let lev,lsv,phi,type1,type2 = create_init_vars () in
      let s_subset = ConstBasic.string_of 
        (ConstBasic.create_c_subset phi phi) 
      in
      let s_disjoint = ConstBasic.string_of 
        (ConstBasic.create_c_disjoint phi phi) 
      in
        assert_const "mu_0 s mu_0" s_subset;
        assert_const "mu_0 d mu_0" s_disjoint;
    in
    let t5 () =
      let lev,lsv,phi,type1,type2 = create_init_vars () in
      let s_subtype = ConstBasic.string_of 
        (ConstBasic.create_c_subtype type1 type2) 
      in
        assert_const "undefined <: T" s_subtype
    in

    let t6 () =
      let lev,lsv,phi,type1,type2 = create_init_vars () in
      let s_eqtype = ConstBasic.string_of 
        (ConstBasic.create_c_eqtype type1 type2) 
      in
      let le = create_lenv_levar lev in
      let s_eqlenv = ConstBasic.string_of (ConstBasic.create_c_eqlenv le le) in
      let obj = empty_obj in
      let s_eqobj = ConstBasic.string_of (ConstBasic.create_c_eqobj obj obj) in
      let s_eqset = ConstBasic.string_of (ConstBasic.create_c_eqset phi phi) in
      let q1 = q_in in
      let q2 = q_ex in
      let s_pres = ConstBasic.string_of
        (ConstBasic.create_c_eqpres q1 q2)
      in
        assert_const "le_0 = le_0" s_eqlenv;
        assert_const "{} = {}" s_eqobj;
        assert_const "mu_0 = mu_0" s_eqset;
        assert_const "undefined = T" s_eqtype;
        assert_const "~ = @" s_pres
    in
    let t7 () =
      let lev,lsv,phi,type1,type2 = create_init_vars () in
      let s_flow = ConstBasic.string_of 
        (ConstBasic.create_c_flow lev type1 type2)
      in
        assert_const "le_0, undefined <| T" s_flow;
    in
    let t8 () =
      let lev,lsv,phi,type1,type2 = create_init_vars () in
      let s_domaineq = ConstBasic.string_of
        (ConstBasic.create_c_domain_eq phi lev)
      in
        assert_const "mu_0 = dom(le_0)" s_domaineq
    in
    let t9 () =
      let lev,lsv,phi,type1,type2 = create_init_vars () in
      let tv = TVar.create () in
      let s_read = ConstBasic.string_of
        (ConstBasic.create_c_read 
           (LEVar.create ()) 
           (PrVar.create ())
           (LSVar.create ())
           (Syntax.create_label "prop")
           tv)
      in
      let lev2 = LEVar.create () in
      let tv2 = TVar.create () in
      let _ = TVar.set_upper 
        (Type.create_t_obj_tau 
           (Type.create_q_pv (PrVar.create ()))
           (Type.create_phi_lsvar lsv))
        tv2
      in
      let s_write = ConstBasic.string_of
        (ConstBasic.create_c_write 
           lev
           tv2
           (Syntax.create_label "p")
           tv
           lev2)
      in
        assert_const "le_1 |-r pv_0 mu_1.prop : tv_0" s_read;
        assert_const "le_0 |-w tv_1.p = tv_0 => le_2" s_write 
    
    in
    let t10 () = 
      let lev,lsv,phi,type1,type2 = create_init_vars () in
      let tv = TVar.create () in
        assert_const "true" (Const.string_of ()); 
        new_const (ConstBasic.c_false None);
        assert_const "false" (Const.string_of ()); 
        Const.reset ();
        new_const (ConstBasic.create_c_read  
                     lev
                     (PrVar.create ()) 
                     lsv
                     (Syntax.create_label "prop") 
                     tv);
        assert_const  
          "le_0 |-r pv_0 mu_0.prop : tv_0"  
          (Const.string_of ());
        new_const (ConstBasic.create_c_flow lev type1 type2);
        assert_const  
          "le_0, undefined <| T; le_0 |-r pv_0 mu_0.prop : tv_0"  
          (Const.string_of ());

    in
      wrap_list 
        [("const create string_of test, true, false", t1);
         ("const create string_of test, create_c_demoation_*", t2);
         ("const create string_of test, create_c_(in|ex)clude", t3);
         ("const create string_of test, create_c_(subset|disjoint)", t4);
         ("const create string_of test, create_c_subtype", t5);
         ("const create string_of test, create_c_eq*", t6);
         ("const create string_of test, create_c_flow", t7);
         ("const create string_of test, create_c_domain_eq", t8);
         ("const create string_of test, create_c_(read|write)", t9);
         ("const c_add(s) test", t10)]

  let _ =
    install_tests "Inf.Const" (wrap_reset init_tests) 

end

module TestSolve = struct
  open Type
  open Test
  open TestUtils
  open ConstBasic
  open ExtUtils
  open Vars
  open ExtString


  let init_tests () = 
    let new_const bc = Const.add bc; () in
    let assert_string s1 s2 = 
      assert_equal
        ~cmp:(Utils.compare_to_equal String.compare)
        ~printer:(fun x -> x)
        s1
        s2
    in
      (*    let assert_string_n s1 s2 = 
            assert_string ("\n" ^ s1 ^ "\n") ("\n" ^ s2 ^ "\n")
            in *)

    let to_string_ls_var_sol lsv =
      let s = LSVar.string_of lsv in
        s ^ ": " ^ LSVar.string_of_img lsv
    in
    let to_string_pvar_sol pv =
      let s = PrVar.string_of pv in
        s ^ ": " ^ PrVar.string_of_img pv
    in
    let to_string_tvar_sol tv =
      let s = TVar.string_of tv in
        s ^ ": " ^ (TVar.string_of_img tv)
    in
    let create_new_lset anz = 
      let ll = List.map 
        (Utils.apply' ())
        (Utils.replicate anz Loc.create) 
      in
        ll
    in
    let assert_const c1 c2 =
      let c1l = String.nsplit c1 "; " in
      let c2l = String.nsplit c2 "; " in
      let cmp = List.compare_ignore_order 
        ~equal:(Utils.compare_to_equal String.compare)
      in
        assert_equal
          ~cmp:cmp
          ~printer:(fun sl -> String.concat "; " 
                      (List.map (fun s -> "'"^s^"'") sl))
          c1l
          c2l
    in

    let t1 () = 
      let mu_0 = LSVar.create () in 
      let mu_1 = LSVar.create () in 
      let phi_0 = create_phi_lsvar mu_0 in 
      let phi_1 = create_phi_lsvar mu_1 in 
      let l0 = (Loc.create ()) in 
        List.iter
          new_const
          [create_c_subset 
             (create_phi_lsvar mu_0) 
             (create_phi_lsvar mu_1); 
           create_c_include l0 phi_0; 
           create_c_include (Loc.create ()) phi_0; 
           create_c_exclude (Loc.create ()) phi_0; 
           create_c_exclude (Loc.create ()) phi_1; 
          ];
        let _ = Const.solve () in 
          assert_string 
            "mu_0 s mu_1" 
            (Const.string_of ()); 
          assert_string  
            "mu_0: l0,l3 < -l1,l2" 
            (to_string_ls_var_sol mu_0); 
          assert_string 
            "mu_1: l0,l3 < -l1" 
            (to_string_ls_var_sol mu_1); 
    in
    let t2 () =
      let pv1 = PrVar.create () in
      let pv2 = PrVar.create () in
      let q1 = create_q_pv pv1 in
      let q2 = create_q_pv pv2 in
      let bc1 = ConstBasic.create_c_eqpres q1 q_ex in
      let bc2 = ConstBasic.create_c_eqpres q2 q_in in
      let bc3 = ConstBasic.create_c_eqpres q_in q_in in
      let bc4 = ConstBasic.create_c_eqpres q_ex q_ex in
        new_const bc1;
        new_const bc2;
        new_const bc3;
        new_const bc4;
        let _ = Const.solve () in
          assert_string  
            "pv_0: = @" 
            (to_string_pvar_sol pv1); 
          assert_string  
            "pv_1: = ~" 
            (to_string_pvar_sol pv2); 
          assert_string
            "true"
            (Const.string_of ());
          Const.add (ConstBasic.create_c_eqpres q1 q2);
          let _ = Const.solve () in
            assert_string  
              "pv_0: = @" 
              (to_string_pvar_sol pv1); 
            (* no alias is created, that's why here the string
               is not pv_1[0].*)
            assert_string  
              "pv_1: = ~" 
              (to_string_pvar_sol pv2); 
            assert_string
              "false"
              (Const.string_of ());
            ()
    in
    let t3 () =
      let l = create_phi_ls (LSet.singleton (Loc.create ())) in
      let lev1 = LEVar.create () in
      let lev2 = LEVar.create () in
      let phi = create_phi_ls (LSet.singleton (Loc.create ())) in
      let t1 = t_top in
      let t2 = t_undef in
      let t3 = create_t_obj q_ex l in
      let t4 = create_t_fun lev1 t2 t2 phi lev2 t2 in
        (*       let tv1 = TVar.create () in *)
        (*       let tv2 = TVar.create () in *)
      let bc1 = ConstBasic.create_c_eqtype t1 t1 in
      let bc2 = ConstBasic.create_c_eqtype t2 t2 in
      let bc3 = ConstBasic.create_c_eqtype t3 t3 in
      let bc4 = ConstBasic.create_c_eqtype t4 t4 in
      let bcf1 = ConstBasic.create_c_eqtype t1 t2 in
      let bcf2 = ConstBasic.create_c_eqtype t2 t3 in
      let bcf3 = ConstBasic.create_c_eqtype t3 t4 in
      let false_after_solve () = 
        Const.solve ();
        assert_string
          "false"
          (Const.string_of ());
      in        
        new_const bc1;
        new_const bc2;
        new_const bc3;
        new_const bc4;
        assert_const
          ("T = T; undefined = undefined; obj(@{l0}) = obj(@{l0}); "
           ^"(le_0,undefined x undefined)-{l1}->(le_1,undefined) "
           ^"= (le_0,undefined x undefined)-{l1}->(le_1,undefined)")
          (Const.string_of ());
        Const.solve ();
        assert_string 
          "true"
          (Const.string_of ());
        new_const bcf1;
        assert_string
          "T = undefined"
          (Const.string_of ());
        false_after_solve ();

        Const.reset ();
        new_const bcf2;
        assert_string
          "undefined = obj(@{l0})"
          (Const.string_of ());
        false_after_solve ();        
        Const.reset ();
        new_const bcf3;
        assert_string
          "obj(@{l0}) = (le_0,undefined x undefined)-{l1}->(le_1,undefined)"
          (Const.string_of ());
        false_after_solve ();        
    in
    let t4 () =
      let ll1 = create_new_lset 10 in
      let ll2 = create_new_lset 10 in
      let l0 = LSet.from_list ll1 in
      let l1 = LSet.from_list ll2 in
      let l2 = LSet.from_list (List.rev_append ll1 ll2) in
        assert_string 
          "l0,l1,l2,l3,l4,l5,l6,l7,l8,l9"
          (LSet.string_of l0);
        assert_string 
          "l10,l11,l12,l13,l14,l15,l16,l17,l18,l19"
          (LSet.string_of l1);
        assert_string 
          ("l0,l1,l2,l3,l4,l5,l6,l7,l8,l9,"
           ^"l10,l11,l12,l13,l14,l15,l16,l17,l18,l19")
          (LSet.string_of l2);
        let mu0 = LSVar.create () in
        let mu1 = LSVar.create () in
        let mu2 = LSVar.create () in
        let bc1 = ConstBasic.create_c_eqset 
          (create_phi_ls l0)
          (create_phi_lsvar mu0)
        in
        let bc2 = ConstBasic.create_c_eqset 
          (create_phi_ls l1)
          (create_phi_lsvar mu1)
        in
          assert_string 
            "true"
            (Const.string_of ());
          Const.add bc1;
          Const.add bc2;
          assert_string 
            ("{l0,l1,l2,l3,l4,l5,l6,l7,l8,l9} = mu_0; "
             ^"{l10,l11,l12,l13,l14,l15,l16,l17,l18,l19} = mu_1")
            (Const.string_of ());
          Const.solve ();
          assert_string 
            "true"
            (Const.string_of ());
          let bc3 = ConstBasic.create_c_subset
            (create_phi_lsvar mu0)
            (create_phi_lsvar mu2)
          in
          let bc4 = ConstBasic.create_c_subset
            (create_phi_lsvar mu1)
            (create_phi_lsvar mu2)
          in
            Const.add bc3;
            Const.add bc4;
            Const.solve ();
            assert_string
              "mu_0 s mu_2; mu_1 s mu_2"
              (Const.string_of ());
            assert_string  
              ("mu_0: l0,l1,l2,l3,l4,l5,l6,l7,l8,l9 <"
               ^" l0,l1,l2,l3,l4,l5,l6,l7,l8,l9")
              (to_string_ls_var_sol mu0); 
            assert_string  
              ("mu_1: l10,l11,l12,l13,l14,l15,l16,l17,l18,l19 <"
               ^" l10,l11,l12,l13,l14,l15,l16,l17,l18,l19")
              (to_string_ls_var_sol mu1); 
            assert_string  
              ("mu_2: l0,l1,l2,l3,l4,l5,l6,l7,l8,l9,"
               ^"l10,l11,l12,l13,l14,l15,l16,l17,l18,l19 <")
              (to_string_ls_var_sol mu2); 
    in
    let t5 () =
      let ll1 = create_new_lset 10 in
      let ll2 = create_new_lset 10 in
      let l0 = LSet.from_list ll1 in
      let l1 = LSet.from_list ll2 in
      let l2 = LSet.from_list (List.rev_append ll1 ll2) in
      let lsv = (LSVar.create ()) in
      let phi0 = Type.create_phi_ls l0 in
      let phi1 = Type.create_phi_ls l1 in
      let phi2 = Type.create_phi_ls l2 in
      let phi3 = Type.create_phi_lsvar lsv in
      let bc1 = ConstBasic.create_c_disjoint phi0 phi1 in
      let bc2 = ConstBasic.create_c_disjoint phi0 phi2 in
      let bc3 = ConstBasic.create_c_disjoint phi0 phi3 in
        Const.add bc1;
        assert_string 
          ("{l0,l1,l2,l3,l4,l5,l6,l7,l8,l9} d"
           ^" {l10,l11,l12,l13,l14,l15,l16,l17,l18,l19}")
          (Const.string_of ());
        Const.solve ();
        assert_string 
          "true"
          (Const.string_of ());
        Const.add bc1;
        Const.add bc2;
        assert_string 
          ("{l0,l1,l2,l3,l4,l5,l6,l7,l8,l9} d"
           ^" {l10,l11,l12,l13,l14,l15,l16,l17,l18,l19}; "
           ^"{l0,l1,l2,l3,l4,l5,l6,l7,l8,l9} d"
           ^" {l0,l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,"
           ^"l11,l12,l13,l14,l15,l16,l17,l18,l19}")
          (Const.string_of ());
        Const.solve ();
        assert_string 
          "false"
          (Const.string_of ());
        Const.reset ();
        Const.add bc3;
        assert_string 
          "{l0,l1,l2,l3,l4,l5,l6,l7,l8,l9} d mu_0"
          (Const.string_of ());
        Const.solve ();
        assert_string 
          "true"
          (Const.string_of ());
        assert_string 
          "mu_0: - l0,l1,l2,l3,l4,l5,l6,l7,l8,l9"
          (to_string_ls_var_sol lsv);
        ()
    in
    let t6 () =
      let otv = OTVar.create () in
      let tv0 = TVar.create () in
      let tv1 = TVar.create () in
      let tv2 = TVar.create () in
      let tv3 = TVar.create () in
      let r1 = create_r_otv otv in
      let r2 = add_to_obj empty_obj 
        (Syntax.create_label "a") 
        tv0
      in
      let r2 = add_to_obj r2 
        (Syntax.create_label "b") 
        tv1
      in
      let r3 = add_to_obj empty_obj 
        (Syntax.create_label "a") 
        tv2
      in
      let r3 = add_to_obj r3 
        (Syntax.create_label "b") 
        tv3
      in

      let bc = ConstBasic.create_c_eqobj r2 r3 in
      let _ = Const.add bc in
      let _ = assert_string
        "{b:tv_1;a:tv_0} = {b:tv_3;a:tv_2}"
        (Const.string_of ());
      in
      let _ = Const.solve () in
      let _ = assert_string
        "true"
        (Const.string_of ()) 
      in
      let _ = 
        assert_equal
          ~cmp:(Utils.compare_to_equal TVar.compare)
          ~printer:TVar.string_of
          tv1
          tv3
      in
      let _ = assert_equal
        ~cmp:(Utils.compare_to_equal TVar.compare)
        ~printer:TVar.string_of
        tv0
        tv2
      in
      let bc = ConstBasic.create_c_eqobj r1 r3 in
      let _ = Const.add bc in
      let _ = assert_string
        "ov_0 = {b:tv_3[1];a:tv_2[0]}"
        (Const.string_of ())
      in
      let _ = Const.solve () in
      let _ = assert_string
        "true"
        (Const.string_of ());
      in
      let _ = assert_equal
        ~cmp:(Utils.compare_to_equal TVar.compare)
        ~printer:TVar.string_of
        tv1
        tv3
      in
      let _ = 
        assert_equal
          ~cmp:(Utils.compare_to_equal TVar.compare)
          ~printer:TVar.string_of
          tv0
          tv2
      in
      let _ =  
        assert_equal 
          ~printer:(fun s -> s) 
          "ov_0: b : tv_3[1]; a : tv_2[0]" 
          (OTVar.string_of_with_img otv) 
      in           
        ()



    in 
    let t7 () =
      let t1 = t_top in
      let t2 = t_undef in
      let l1 = Loc.create () in
      let ls1 = LSet.singleton l1 in
      let t3 = create_t_obj q_in (create_phi_ls ls1) in
      let bc = ConstBasic.create_c_subtype t1 t2 in
      let _ = Const.add bc in
      let _ = assert_string  "T <: undefined" (Const.string_of ()) in
      let _ = Const.solve () in
      let _ = assert_string "false" (Const.string_of ()) in
      let _ = Const.reset () in
      let _ = assert_string "true" (Const.string_of ()) in
      let bc = ConstBasic.create_c_subtype t2 t1 in
      let _ = Const.add bc in
      let _ = assert_string "undefined <: T" (Const.string_of ()) in
      let _ = Const.solve () in
      let _ = assert_string "true" (Const.string_of ()) in
      let bc = create_c_subtype t3 t1 in
      let _ = Const.add bc in
      let _ = Const.solve () in
      let _ = assert_string "true" (Const.string_of ()) in
      let _ = Const.reset () in
      let bc = ConstBasic.create_c_subtype t1 t2 in
      let _ = Const.add bc in
      let _ = Const.solve () in
      let _ = assert_string "false" (Const.string_of ()) in
      let _ = Const.reset () in
      let bc = ConstBasic.create_c_subtype t2 t3 in
      let _ = Const.add bc in
      let _ = assert_string "undefined <: obj(~{l0})" (Const.string_of ()) in
      let _ = Const.solve () in
      let _ = assert_string "false" (Const.string_of ()) in
      let _ = Const.reset () in
      let bc = ConstBasic.create_c_subtype t3 t2 in
      let _ = Const.add bc in
      let _ = assert_string "obj(~{l0}) <: undefined" (Const.string_of ()) in
      let _ = Const.solve () in
      let _ = assert_string "false" (Const.string_of ()) in
        ()
    in
      
    let t8 () =
      let l = Type.create_phi_ls (LSet.singleton (Loc.create ())) in
      let mu = Type.create_phi_lsvar (Vars.LSVar.create ()) in
      let xhi = Type.create_q_pv (PrVar.create ()) in
      let a0 = Type.create_t_tv (TVar.create ()) in
      let a1 = Type.create_t_tv (TVar.create ()) in
      let bc1 = ConstBasic.create_c_subtype 
        (Type.create_t_obj Type.q_ex l)
        a0
      in
      let bc2 = ConstBasic.create_c_subtype a0 a1 in
      let bc3 = ConstBasic.create_c_subtype 
        (Type.create_t_obj xhi mu)
        a1
      in
        Const.add_both bc1;
        Const.add_both bc2;
        Const.add_both bc3;
        Const.solve ();
        ()
    in
    let t9 () =
      let l = LSet.singleton (Loc.create ()) in
      let tv0 = TVar.create () in
      let tv1 = TVar.create () in
      let tv2 = TVar.create () in
      let tv3 = TVar.create () in
      let pv0 = PrVar.create () in
      let pv1 = PrVar.create () in
      let mu0 = LSVar.create () in
      let mu1 = LSVar.create () in
      let bc0 = ConstBasic.create_c_subtype
        (Type.create_t_tv tv1)
        (Type.create_t_tv tv0)
      in
      let bc1 = ConstBasic.create_c_subtype
        (Type.create_t_tv tv0)
        (Type.create_t_tv tv2)
      in
      let bc2 = ConstBasic.create_c_subtype
        (Type.create_t_tv tv0)
        (Type.create_t_tv tv3)
      in
      let tobj0 = Type.create_t_obj_tau 
        (Type.create_q_pv pv0)
        (Type.create_phi_lsvar mu0)
      in
      let tobj1 = Type.create_t_obj_tau
        (Type.create_q_pv pv1)
        (Type.create_phi_lsvar mu1)
      in
      let tobj2 = Type.create_t_obj_tau
        Type.q_ex
        (Type.create_phi_ls l)
      in
        TVar.set_eq tobj0 tv2;
        TVar.set_eq tobj1 tv3;
        TVar.set_eq tobj2 tv1;
        Const.add bc0;
        Const.add bc1;
        Const.add bc2;
        assert_string
          "tv_0 <: tv_2; tv_0 <: tv_3; tv_1 <: tv_0"
          (Const.string_of ());
        Const.solve ();
        assert_string
          ("pv_2 <: pv_0; pv_2 <: pv_1; mu_2 s mu_0; mu_2 s mu_1; "
           ^"tv_0 <: tv_2; tv_0 <: tv_3; tv_1 <: tv_0")
          (Const.string_of ());
        assert_string
          "tv_1: = obj(@{l0})"
          (to_string_tvar_sol tv1);
        assert_string
          "tv_2: = obj(@{l0})"
          (to_string_tvar_sol tv2);
        assert_string
          "tv_3: = obj(@{l0})"
          (to_string_tvar_sol tv3);
        assert_string
          "tv_0: = obj(@{l0})"
          (to_string_tvar_sol tv0);

    in
      wrap_list
        ["solve test, location simplification", t1;
         "solve test, precise eq simlification", t2;
         "solve test, eq type,"
         ^" eq ref, eq set, eq pres, eq lenv simlification", t3;
         "solve test, eq set, subset", t4; 
         "solve test, disjoint test", t5;
         "solve test, object types", t6;
         "solve test, subtype, easy tests", t7;
         "solve test, subtype, object test", t8;
         "solve test, subtype, object test, snd", t9;
        ]

  let _ =
    install_tests "Inf, solving" (wrap_reset init_tests)


end
