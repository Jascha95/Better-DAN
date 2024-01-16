type tb = [ `Bottom | `Top ]
    
let union_tb : [> tb] -> [> tb] -> [> tb] = fun tb1 tb2 ->
  match tb1,tb2 with
    | _, `Top | `Top, _ -> `Top
    | `Bottom, t | t, `Bottom -> t
        
let inter_tb : [>tb] -> [>tb] -> [>tb] = fun tb1 tb2 -> match tb1, tb2 with
  | `Top, tb | tb, `Top -> tb
  | `Bottom, _ | _, `Bottom -> `Bottom

let subtype_tb : [> tb] -> [> tb] -> bool option = 
  fun tb1 tb2 -> match tb1,tb2 with
    | `Top, `Top -> Some true
    | `Top, _ -> Some false
    | `Bottom, tb -> Some true
    | tb, `Bottom -> Some false
    | _, _ -> None
      
let create_top () = `Top
  
let string_of = function
  | `Bottom -> "B"
  | `Top -> "T"
      
let is_top = function
  | `Top -> true
  | _ -> false
      
let is_bottom = function
  | `Bottom -> true
  | _ -> false

