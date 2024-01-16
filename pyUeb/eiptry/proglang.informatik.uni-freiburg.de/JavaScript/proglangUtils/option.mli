(** Provides some help functions that deals with the option type. 
    @author: Phillip Heidegger
*)

(** Runs the given function on the two arguments, if both are 
    not None. There are the four cases:
    None,None -> None
    None,Some e -> Some (f2 e)
    Some e,None -> Some (f1 e)
    Some e1, Some e2 -> Some (f e1 e2)
*)
val maybe_tuppel : 
  f:('a -> 'b -> 'c) -> 
  fa:('a -> 'c) ->
  fb:('b -> 'c) ->
  'a option -> 'b option -> 'c option


(** Takes a compare function and returns a new one, that is
    able to deal with option values. *)
val maybe_cmp : ('a -> 'a -> int) -> 'a option -> 'a option -> int


(** Takes a compare function and returns a new one, that is
    able to deal with option values. *)
val maybe_equal : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool

(** Takes a [string_of] function and returns a function that
    converts 'a option values into a string. *)
val maybe_string : ('a -> string) -> 'a option -> string

val bind : 
  f1:('a -> bool option) -> 
  f2:('b -> bool option) ->
  'a -> 'b -> bool option

val doo : ('a -> 'b) -> 'a option -> 'b option

(** and operation on optional boolean values. *)
val oand : bool option -> bool option -> bool option

(** or operation on optional boolean values. *)
val oor : bool option -> bool option -> bool option

val oands : bool option list -> bool option
