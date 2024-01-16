(* Internal representation *)

type 'a symclass =
    Symclass_range of 'a * 'a
  | Symclass_union of 'a symclass * 'a symclass

type 'a regexp =
    Null
  | Epsilon
  | Symclass of bool * 'a symclass
  | Concat of 'a regexp * 'a regexp
  | Alternate of 'a regexp * 'a regexp
  | Repeat of 'a regexp

(* Construction and normalization *)

let symclass_range x y = 
  if y < x then failwith "upper bound of symbol class smaller than lower bound"
  else Symclass_range(x, y)
let symclass_union a b = Symclass_union(a, b)
let union_list = function
    [] -> failwith "union_list called with empty list"
  | x::xs -> List.fold_left symclass_union x xs
let symclass_symbol x = Symclass_range(x, x)
let symclass_symbols l = union_list (List.map symclass_symbol l)

let epsilon = Epsilon
let symclass_pos s = Symclass(true, s)
let symclass_neg s = Symclass(false, s)
let symbol x = Symclass(true, symclass_symbol x)
let concat r1 r2 =
  if r1 = Null or r2 = Null
  then Null
  else if r1 = Epsilon
  then r2
  else if r2 = Epsilon
  then r1
  else Concat(r1, r2)
let alternate r1 r2 =
  if r1 = Null
  then r2
  else if r2 = Null
  then r1
  else Alternate(r1, r2)
let repeat r = 
  if r = Null or r = Epsilon
  then Epsilon
  else Repeat(r)

let repeat_one r = concat r (repeat r)

let concat_list l = List.fold_left concat Epsilon l

let alternate_list l = List.fold_left alternate Null l

let is_null r = (r = Null)

(* Operations *)

let symclass_elem x pos symclass = 
  let rec prim_symclass_elem = function
      Symclass_range(y, z) -> y <= x && x <= z
    | Symclass_union(a, b) -> prim_symclass_elem a || prim_symclass_elem b
  in
    if pos then prim_symclass_elem symclass
    else not (prim_symclass_elem symclass)

let rec accepts_empty regexp =
  match regexp with
    Null -> false
  | Epsilon -> true
  | Symclass(_) -> false
  | Concat(r1, r2) ->
      accepts_empty(r1) && accepts_empty(r2)
  | Alternate(r1, r2) ->
      accepts_empty(r1) or accepts_empty(r2)
  | Repeat(r1) -> true

let rec after_symbol symbol regexp =
  match regexp with
    Null -> Null
  | Epsilon -> Null
  | Symclass(pos, a) ->
      if symclass_elem symbol pos a
      then Epsilon
      else Null
  | Concat(r1, r2) ->
      let after_1 = concat (after_symbol symbol r1) r2 in
      let after_2 = if accepts_empty r1
                    then after_symbol symbol r2
                    else Null
      in
      alternate after_1 after_2
  | Alternate(r1, r2) ->
      alternate (after_symbol symbol r1)
	        (after_symbol symbol r2)
  | Repeat(r1) ->
      concat (after_symbol symbol r1)
		    (Repeat(r1))

(* Matching *)

let rec matches regexp symbols =
  match symbols with
    [] -> accepts_empty regexp
  | symbol::rest ->
      let next_regexp = after_symbol symbol regexp in
      if is_null next_regexp
      then false
      else matches next_regexp rest
