open ProglangUtils

module type SS_WO_ME = sig
  type t
  val is_empty : t -> bool
  val subset : t -> t -> bool option
  val inter : t -> t -> t
  val union : t -> t -> t

  val compare : t -> t -> int
  val string_of : t -> string
  val normalize : t -> t
end

module type SMALLSET = sig
  include SS_WO_ME
  val make_eq_lower : t -> t -> bool * t
  val make_eq_upper : t -> t -> bool * t
  val make_eq_eq : t -> t -> bool * t
end

module type S = sig
  type 'a d = [
    | `Lower of 'a
    | `Upper of 'a
    | `LowerUpper of 'a * 'a
    | `Eq of 'a
  ]

  type elm
  type t = elm d 
  val create_lower : elm -> t
  val create_upper : elm -> t
  val create_lowerupper : elm -> elm -> t
  val compare : t -> t -> int
  val merge : t -> t -> t
  val get_exact : t -> elm option
  val string_of : t -> string
  val normalize : t -> t
end

module Make(S: SMALLSET) = struct
  open ExtUtils

  type 'a d = [
    | `Lower of 'a
    | `Upper of 'a
    | `LowerUpper of 'a * 'a
    | `Eq of 'a
  ]

  type elm = S.t
  
  let normalize = S.normalize
  type t = S.t d 
  let create_lower ls = 
    `Lower (normalize ls)
  let create_upper ls = 
    `Upper (normalize ls)
  let create_lowerupper ls1 ls2 =
    let ls1 = normalize ls1 in
    let ls2 = normalize ls2 in
      match S.subset ls1 ls2 with
        | Some false ->       
            raise (GenVars.EUnion ("Merge not possible, => "
                                   ^"constraints not solveable, => false"))
        | _ -> 
            if (S.compare ls1 ls2 == 0) then
              `Eq ls1 
            else
              `LowerUpper (ls1,ls2)
  let create_eq ls = `Eq (normalize ls)

  open ExtUtils
  open Utils
  let string_of (ls1,ls2) =
    "IN: " ^ S.string_of ls1 ^ " EX: " ^ S.string_of ls2
  let compare d1 d2 = 
    match d1,d2 with
      | `Lower d1, `Lower d2 | `Upper d1, `Upper d2 -> S.compare d1 d2
      | `Lower _, _ -> 1
      | _, `Lower _ -> -1
      | `Upper _, _ -> 1
      | _, `Upper _ -> -1
      | `LowerUpper (d11,d12), `LowerUpper (d21,d22) ->
          (S.compare d11 d21) <.< (fun () -> S.compare d12 d22)
      | `LowerUpper _, _ -> 1
      | _, `LowerUpper _ -> -1
      | `Eq s1, `Eq s2 -> S.compare s1 s2
      | `Eq _, _ -> 1
  let get_exact = function
    | `Eq s -> Some s
    | `LowerUpper (ls1,ls2) ->
        if S.compare ls1 ls2 == 0 then
          Some (ls1)
        else
          None
    | _ -> None

  let merge_help f s1 s =
    let b,s = f s1 s in
      if b then 
        create_eq s
      else
        raise (GenVars.EUnion ("Merge not possible, => "
                               ^"constraints not solveable, => false"))
            
  let merge d1 d2 = 
    let r = 
      match d1,d2 with
        | `Lower s1, `Lower s2 -> create_lower (S.union s1 s2)
        | `Lower s1, `Upper s2 
        | `Upper s2, `Lower s1 -> 
            create_lowerupper s1 s2
        | `Lower s1, `LowerUpper (s2,s3) 
        | `LowerUpper (s2,s3), `Lower s1 ->
            create_lowerupper (S.union s1 s2) s3
        | `Upper s1, `Upper s2 -> create_upper (S.inter s1 s2)
        | `Upper s1, `LowerUpper (s2,s3)
        | `LowerUpper (s2,s3), `Upper s1 ->
            create_lowerupper s2 (S.inter s1 s3)
        | `LowerUpper (s1,s2), `LowerUpper (s3,s4) ->
            create_lowerupper (S.union s1 s3) (S.inter s2 s4)
        | `Eq s, `Lower s1 | `Lower s1, `Eq s -> 
            merge_help S.make_eq_lower s1 s
        | `Eq s, `Upper s1 | `Upper s1, `Eq s -> 
            merge_help S.make_eq_upper s1 s            
        | `Eq s, `LowerUpper (s1,s2) | `LowerUpper (s1,s2), `Eq s -> 
            let (`Eq s) = merge_help S.make_eq_lower s1 s in
              merge_help S.make_eq_upper s2 s
        | `Eq s1, `Eq s2 ->
            merge_help S.make_eq_eq s1 s2
    in  
      r

  let string_of = function
    | `Lower ls -> S.string_of ls ^ " <"
    | `Upper ls -> "< " ^ S.string_of ls 
    | `LowerUpper (ls1,ls2) -> S.string_of ls1 ^ " < " ^ S.string_of ls2  
    | `Eq s -> "= " ^ S.string_of s

  let get_lower = function
    | `Lower ls | `LowerUpper (ls,_) | `Eq ls -> Some ls
    | _ -> None

  let get_upper = function
    | `Upper ls | `LowerUpper (_,ls) | `Eq ls -> Some ls
    | _ -> None

  let subset d1 d2 = 
    let secure () = 
      match (get_upper d1, get_lower d2) with
        | Some s1, Some s2 ->
            begin
              match S.subset s1 s2 with
                | Some true -> Some true
                | _ -> None
            end
        | _ -> None
    in            
      match (get_lower d1, get_upper d2) with
        | Some s1,Some s2 -> 
            begin
              match S.subset s1 s2 with
                | Some false -> Some false
                | _ -> secure ()
            end
        | _ -> secure ()

  let normalize = function
    | `Lower ls -> create_lower ls
    | `Upper ls -> create_upper ls
    | `LowerUpper (ls1,ls2) ->
        create_lowerupper ls1 ls2
    | `Eq ls -> create_eq ls
end
