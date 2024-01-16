type tb = [ `Bottom | `Top ]
val union_tb : ([> tb ] as 'a) -> 'a -> 'a
val inter_tb : ([> tb ] as 'a) -> 'a -> 'a
val subtype_tb : [> tb]  -> [> tb ] -> bool option
val create_top : unit -> [> `Top ]
val string_of : [< `Bottom | `Top ] -> string
val is_top : [> `Top ] -> bool
val is_bottom : [> `Bottom ] -> bool
