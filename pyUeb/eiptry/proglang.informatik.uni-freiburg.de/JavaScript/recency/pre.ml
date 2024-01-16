open NoValue

type t = [ nv | `Ex | `In ]


let union : t -> t -> t = fun pre1 pre2 -> match pre1,pre2 with
  | #nv as q1, q2 -> (union_nv q1 q2 :> t)
  | q1, (#nv as q2) -> (union_nv q1 q2 :> t)
  | `In, `In -> `In 
  | `Ex, `Ex -> `Ex 
  | `In, `Ex | `Ex, `In -> `NVUp

let inter q1 q2 = match q1,q2 with
  | #nv as t1, t2 -> (inter_nv t1 t2 :> t)
  | t1, (#nv as t2) -> (inter_nv t1 t2 :> t)
  | `In, `In  -> `In
  | `Ex, `Ex -> `Ex
  | `Ex, `In | `In, `Ex -> `NVDown

let subset = fun q1 q2 -> match q1,q2 with
  | (#nv as tb1), tb2 -> subtype_nv tb1 tb2
  | tb1, (#nv as tb2) -> subtype_nv tb1 tb2
  | `In, `In | `Ex, `Ex -> Some true
  | `In, `Ex | `Ex, `In -> Some false
  | _, _ -> None

let is_empty = function
  | `NVDown -> true
  | _ -> false
      
let string_of = function
  | #nv as nv -> NoValue.string_of nv
  | `In -> "~"
  | `Ex -> "@"

let compare = Pervasives.compare

let make_eq_eq t1 t2 = compare t1 t2 == 0,t2
let make_eq_lower l t = compare l t == 0,t
let make_eq_upper l t = compare l t == 0,t
