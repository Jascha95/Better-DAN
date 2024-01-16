(** Extends the OCaml Map with a string_of function.
    @author: Phillip Heidegger
*)

(** Module type that extends the Map.OrderedType with some
    information needed to print the Map *)
module type OrderedType =
  sig
    (** @see <http://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.OrderedType.html> 
        Map.OrderedType from the standard libary *)
    include Map.OrderedType

    (** A function that prints the values of type t *)
    val string_of : t -> string

    (** String that is used to seperate the different mapping
        contents. Often you can use comma. *)
    val sep : string

    (** this is the string used to seperate the key from the imange,
        often you can use -> *)
    val mapto : string
  end

(** Output signature of the functor OwnMap.Make. *)
module type S =
  sig
    (** @see <http://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.S.html> 
        Map.S from the standard libary *)
    include Map.S

    (** [string_of f m] converts the map [m] into a string using [f] as
        a conversion function of the images. *)
    val string_of : ('a -> string) -> 'a t -> string
  end

(** Functor building an implementation of the map structure 
    given a totally ordered type as in the standard libary. 
    Supports additional string conversion.
*)
module Make : functor (Ord : OrderedType) -> S with type key = Ord.t
