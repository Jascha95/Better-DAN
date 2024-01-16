type 'a symclass
type 'a regexp

val symclass_range : 'a -> 'a -> 'a symclass
val symclass_union : 'a symclass -> 'a symclass -> 'a symclass
val symclass_symbol : 'a -> 'a symclass
val symclass_symbols : 'a list -> 'a symclass
val union_list : 'a symclass list -> 'a symclass

val epsilon : 'a regexp
val symclass_pos : 'a symclass -> 'a regexp
val symclass_neg : 'a symclass -> 'a regexp
val symbol : 'a -> 'a regexp
val concat : 'a regexp -> 'a regexp -> 'a regexp
val alternate : 'a regexp -> 'a regexp -> 'a regexp
val repeat : 'a regexp -> 'a regexp

val repeat_one : 'a regexp -> 'a regexp
val concat_list : 'a regexp list -> 'a regexp
val alternate_list : 'a regexp list -> 'a regexp

val is_null : 'a regexp -> bool

val accepts_empty : 'a regexp -> bool
val after_symbol : 'a -> 'a regexp -> 'a regexp

val matches : 'a regexp -> 'a list -> bool
