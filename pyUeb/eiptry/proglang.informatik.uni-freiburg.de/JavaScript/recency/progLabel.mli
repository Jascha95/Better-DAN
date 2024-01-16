(** Module that provides types, values and function
    for the program labels. *)

(** a program label *)
type t

(** create a new program label. *)
val create_new : unit -> t

(** Compares to program labels. *)
val compare : t -> t -> int

(** Returns a string representation of a program label *)
val string_of : t -> string

(** IMPORTENT: Do only use this function for tests.
    It resets the internal state. So the invariant, 
    that every program label created by [create_new]
    is unique is lost after calling this method. *)
val reset : unit -> unit
