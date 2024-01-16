open ProglangUtils

module type SMALLSET = sig
  include LowerUpper.SS_WO_ME
  val diff : t -> t -> t
end

module type S = sig
  type elm
  type t = [
    | `Lower of elm
    | `Upper of elm
    | `Neg of elm
    | `LowerNeg of elm * elm
    | `LowerUpper of elm * elm
  ]
  val create_lower : elm -> t
  val create_upper : elm -> t
  val create_neg : elm -> t
  val create_lowerupper : elm -> elm -> t
  val create_lowerneg : elm -> elm -> t
  val compare : t -> t -> int
  val merge : t -> t -> t
  val get_exact : t -> elm option
  val string_of : t -> string
end

module Make(Set: SMALLSET) = struct
  open ExtUtils

  type 'a d = [
    | `Lower of 'a
    | `Upper of 'a
    | `Neg of 'a
    | `LowerNeg of 'a * 'a
    | `LowerUpper of 'a * 'a
  ]

  type elm = Set.t
  
  type t = Set.t d 
  let create_lower ls = `Lower ls
  let create_upper ls = `Upper ls
  let create_neg ls = `Neg ls
  let create_lowerneg ls1 ls2 = `LowerNeg (ls1,ls2)
  let create_lowerupper ls1 ls2 = `LowerUpper (ls1,ls2)


  let string_of (ls1,ls2) =
    "IN: " ^ Set.string_of ls1 ^ " EX: " ^ Set.string_of ls2
  let compare d1 d2 = 
    match d1,d2 with
      | `Lower d1, `Lower d2 | `Upper d1, `Upper d2 | `Neg d1, `Neg d2 -> Set.compare d1 d2
      | `LowerUpper (d11,d12), `LowerUpper (d21,d22) 
      | `LowerNeg (d11,d12), `LowerNeg (d21,d22) -> 
          Utils.compare_2_tup (Set.compare,Set.compare) (d11,d12) (d21,d22)
      | `Lower _, _ -> 1
      | _, `Lower _ -> -1
      | `Upper _, _ -> 1
      | _, `Upper _ -> -1
      | `Neg _, _ -> 1
      | _, `Neg _ -> -1
      | `LowerNeg _, _ -> 1
      | _, `LowerNeg _ -> -1
      | `LowerUpper _, _ -> 1
      | _, `LowerUpper _ -> -1

  let build_lower_upper ls1 ls2 =
    match Set.subset ls1 ls2 with
      | Some false -> raise (GenVars.EUnion "Union not possible, => constraints not solveable, => false")
      | _ -> `LowerUpper (ls1,ls2)

  let build_lower_neg ls1 ls2 =
    if (Set.is_empty (Set.inter ls1 ls2)) then
      `LowerNeg (ls1,ls2)
    else
      raise (GenVars.EUnion "Union not possible, => constraints not solveable, => false")
        

  let merge d1 d2 =
    match d1,d2 with
      | `Lower ls1, `Lower ls2 -> `Lower (Set.union ls1 ls2)
      | `Upper ls1, `Lower ls2 -> build_lower_upper ls2 ls1
      | `Neg ls1, `Lower ls2 -> build_lower_neg ls2 ls1
      | `LowerNeg (ls1,ls2), `Lower ls3 -> build_lower_neg (Set.union ls1 ls3) ls2
      | `LowerUpper (ls1,ls2), `Lower ls3 -> build_lower_upper (Set.union ls1 ls3) ls2
          
      | `Lower ls1, `Upper ls2 -> build_lower_upper ls1 ls2
      | `Upper ls1, `Upper ls2 -> `Upper (Set.inter ls1 ls2)
      | `Neg ls1, `Upper ls2 -> `Upper (Set.diff ls2 ls1)
      | `LowerNeg (ls1,ls2), `Upper ls3 ->
          build_lower_upper ls1 (Set.diff ls3 ls2)
      | `LowerUpper (ls1,ls2), `Upper ls3 ->
          build_lower_upper ls1 (Set.inter ls2 ls3)

      | `Lower ls1, `Neg ls2 ->
          build_lower_neg ls1 ls2
      | `Upper ls1, `Neg ls2 -> `Upper (Set.diff ls1 ls2)
      | `Neg ls1, `Neg ls2 -> `Neg (Set.union ls1 ls2)
      | `LowerNeg (ls1,ls2), `Neg ls3 ->
          build_lower_neg ls1 (Set.union ls2 ls3)
      | `LowerUpper (ls1,ls2), `Neg ls3 ->
          build_lower_upper ls1 (Set.diff ls2 ls3)

      | `Lower ls1, `LowerNeg (ls2,ls3) ->
          build_lower_neg (Set.union ls1 ls2) ls3
      | `Upper ls1, `LowerNeg (ls2,ls3) -> 
          build_lower_upper ls2 (Set.diff ls1 ls3)
      | `Neg ls1, `LowerNeg (ls2,ls3) -> 
          build_lower_neg ls2 (Set.union ls1 ls3)
      | `LowerNeg (ls1,ls2), `LowerNeg (ls3,ls4) ->
          build_lower_neg (Set.union ls1 ls3) (Set.union ls2 ls4)
      | `LowerUpper (ls1,ls2), `LowerNeg (ls3,ls4) ->
          build_lower_upper (Set.union ls1 ls3) (Set.diff ls2 ls4)

      | `Lower ls1, `LowerUpper (ls2,ls3) ->
          build_lower_upper (Set.union ls1 ls2) ls3
      | `Upper ls1, `LowerUpper (ls2,ls3) -> 
          build_lower_upper ls2 (Set.inter ls1 ls3)
      | `Neg ls1, `LowerUpper (ls2,ls3) -> 
          build_lower_upper ls2 (Set.diff ls3 ls1)
      | `LowerNeg (ls1,ls2), `LowerUpper (ls3,ls4) ->
          build_lower_upper (Set.union ls1 ls3) (Set.diff ls4 ls2)
      | `LowerUpper (ls1,ls2), `LowerUpper (ls3,ls4) ->
          build_lower_upper (Set.union ls1 ls3) (Set.inter ls2 ls4)

  let string_of = function
    | `Lower ls -> Set.string_of ls ^ " <"
    | `Upper ls -> "< " ^ Set.string_of ls 
    | `Neg ls -> "- " ^ Set.string_of ls
    | `LowerNeg (ls1,ls2) -> Set.string_of ls1 ^ " < -" ^ Set.string_of ls2  
    | `LowerUpper (ls1,ls2) -> Set.string_of ls1 ^ " < " ^ Set.string_of ls2  
    | `Eq s -> " = " ^ (Set.string_of s)

  let get_exact = function
    | `LowerUpper (ls1,ls2) ->
        if (Set.compare ls1 ls2 == 0) then
          Some ls1
        else
          None
    | _ -> None

end
