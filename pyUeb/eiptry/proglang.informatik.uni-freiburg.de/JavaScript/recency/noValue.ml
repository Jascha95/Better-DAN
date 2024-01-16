type nv = [ `NVDown | `NVUp ]
    
let union_nv : [> nv] -> [> nv] -> [> nv] = fun nv1 nv2 ->
  match nv1,nv2 with
    | _, `NVUp | `NVUp, _ -> `NVUp
    | `NVDown, t | t, `NVDown -> t
        
let inter_nv : [>nv] -> [>nv] -> [>nv] = fun nv1 nv2 -> match nv1, nv2 with
  | `NVUp, nv | nv, `NVUp -> nv
  | `NVDown, _ | _, `NVDown -> `NVDown

let subtype_nv : [> nv] -> [> nv] -> bool option = 
  fun nv1 nv2 -> match nv1,nv2 with
    | `NVUp, `NVUp -> Some true
    | `NVUp, _ -> Some false
    | `NVDown, nv -> Some true
    | nv, `NVDown -> Some false
    | _, _ -> None
      
let create_top () = `NVUp
  
let string_of = function
  | `NVDown -> "_B_"
  | `NVUp -> "`T`"
      
let is_top = function
  | `NVUp -> true
  | _ -> false
      
let is_bottom = function
  | `NVDown -> true
  | _ -> false


let do_f ?(down = fun () -> ()) ?(up = fun () -> ()) = function
  | `NVUp -> up ()
  | `NVDown -> down ()
