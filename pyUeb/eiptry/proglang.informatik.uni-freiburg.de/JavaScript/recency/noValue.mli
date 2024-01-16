type nv = [ `NVDown | `NVUp ]
val union_nv : ([> nv ] as 'a) -> 'a -> 'a
val inter_nv : ([> nv ] as 'a) -> 'a -> 'a
val subtype_nv : [> nv]  -> [> nv ] -> bool option
val create_top : unit -> [> `NVUp ]
val string_of : [< `NVDown | `NVUp ] -> string
val is_top : [> `NVUp ] -> bool
val is_bottom : [> `NVDown ] -> bool
val do_f : 
     ?down:(unit -> unit) 
  -> ?up:(unit -> unit)
  -> nv
  -> unit
