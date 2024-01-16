(* Internal representation *)

type 'a regexp =
    Null
  | Epsilon
  | Symbol of 'a
  | Concat of 'a regexp * 'a regexp
  | Alternate of 'a regexp * 'a regexp
  | Repeat of 'a regexp

(* Construction and normalization *)

let epsilon = Epsilon
let symbol x = Symbol(x)
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

let rec accepts_empty regexp =
  match regexp with
    Null -> false
  | Epsilon -> true
  | Symbol(_) -> false
  | Concat(r1, r2) ->
      accepts_empty(r1) && accepts_empty(r2)
  | Alternate(r1, r2) ->
      accepts_empty(r1) or accepts_empty(r2)
  | Repeat(r1) -> true

let rec after_symbol symbol regexp =
  match regexp with
    Null -> Null
  | Epsilon -> Null
  | Symbol(symbol') ->
      if symbol = symbol'
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
