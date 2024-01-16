(** Module that provides some additional string functions. 
    @author Stefan Wehr
    @author Phillip Heidegger
*)

(** The identity function *)
val identity : 'a -> 'a

(** Returns a string representation of a tuple *)
val string_of_pair : ('a -> string) -> ('b -> string) -> 'a * 'b -> string

(** Returns a string representation of a list *)
val string_of_list : ('a -> string) -> 'a list -> string

(** Returns a string representation of a list, using the 
    additional string parameters for adapt the output *)
val string_of_list_complex :
  ?sep:string ->
  ?start_char:string ->
  ?end_char:string -> ('a -> string) -> 'a list -> string


(** Returns a string representation of an array *)
val string_of_array : ('a -> string) -> 'a array -> string

(** Returns a string representation of an option *)
val string_of_option : ('a -> string) -> 'a option -> string

(** Some funny quoting ;) *g* *)
val quote : string -> string

(** Some other funny quoting ;) *g* *)
val quote' : ('a -> string) -> 'a -> string
