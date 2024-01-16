let identity x = x

let string_of_pair f g (x, y) = "(" ^ f x ^ ", " ^ g y ^ ")"

let string_of_list string_of__a ls =
   "["^ String.concat ";" (List.map string_of__a ls) ^"]"

let string_of_list_complex 
    ?(sep = ";") 
    ?(start_char = "[") 
    ?(end_char = "]") 
    string_of__a ls =
  start_char^ String.concat sep (List.map string_of__a ls) ^end_char


let string_of_array string_of__a arr =
   "[|"^ String.concat ";" (List.map string_of__a (Array.to_list arr)) ^"|]"

let string_of_option f = function
  | None -> "None"
  | Some x -> "Some(" ^ f x ^ ")"

let quote s = "\"" ^ String.escaped s ^ "\""
let quote' f s = quote (f s)
