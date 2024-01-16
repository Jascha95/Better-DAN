open NoValue

type t = [ nv | `In | `Ex ]
val union : t -> t -> t
val inter : t -> t -> t
val subset : t -> t -> bool option
val is_empty : t -> bool
val string_of : t -> string
val compare : t -> t -> int

val make_eq_eq : t -> t -> bool * t
val make_eq_lower : t -> t -> bool * t
val make_eq_upper : t -> t -> bool * t
