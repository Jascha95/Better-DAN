(** This module provides some advanced List functions. 
    @author Stefan Wehr
    @author Phillip Heidegger
*)

module List : sig
  
  (** {1 Additional functions } *)

  (** returns [true] if the list is empty, otherwise returns [false]. *)
  val is_empty : 'a list -> bool

  (** like find, but does not throw an error. *)
  val find' : ('a -> bool) -> 'a list -> 'a option 
  
  (** Gets the first n-th arguments of an list *)
  val take : int -> 'a list -> 'a list

  (** [select pred l] returns a pair containing as the first component the longest
      prefix [p] of [l], such that all elements of [p] satisfies [pred]. 
      The second component [q] contains all forthcoming elements of [l], such that
      [p @ q = l]. So the first element of [q] does not satisfy the [pred].
  *)
  val select : ('a -> bool) -> 'a list -> 'a list * 'a list

  (** [mem_cmp cmp a l] checks if a is element of the list, and uses
      cmp as compare operator. 
  *)
  val mem_cmp : ('a -> 'a -> bool) -> 'a -> 'a list -> bool


  (** {2 Iterators}  *)

  (** [mapi f l] applies [f] to each element of the list and also 
      gives the position of the list element as first parameter. 
  *)
  val mapi: (int -> 'a -> 'b) -> 'a list -> 'b list


  (** {2 Comparators }  *)

  (** Compares two lists using a compare operator over
      the elements of the list. *)
  val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int

  (** [compare_ignore_order ~equal:eq l1 l2] returns [true] if the two lists [l1]
      and [l2] has the same elements. The order of the elements is ignored. As a
      comparison function is [eq] used if given. If the parameter is not set the
      default will be [Pervasives.compare].
  *)
  val compare_ignore_order : ?equal:('a -> 'a -> bool) -> 'a list -> 'a list -> bool

  (** {2 Maybe } *)

  (** [maybe_forall f l] returns [Some true] if [f e == Some true]
      for all elements [e] of [l]. If there exists one [e] in [l]
      such that [f e == Some false] then [Some false] is the result
      of [maybe_forall f l]. If no application of [f] to the elements
      results in [Some false], but some returns [None], then [None]
      is returned.

      If the element with number [i] returns [Some false] then
      [f (take j l)] for [i < j < length l] is not evaluated.  
  *)
  val maybe_forall : ('a -> bool option) -> 'a list -> bool option

  (** [maybe_exists f l] returns [Some true] if [f e == Some true]
      for at least one element [e] of [l]. If for all [e] in [l]
      it holds that [f e == Some false] then [Some false] is the result
      of [maybe_forall f l]. If no application of [f] to the elements
      results in [Some True], but some returns [None], then [None]
      is returned.

      If the element with number [i] returns [Some True] then
      [f (take j l)] for [i < j < length l] is not evaluated.  
  *)
  val maybe_exists : ('a -> bool option) -> 'a list -> bool option


  (** {2 Set functions } *)

  (** returns [true] if the first list is a subset of the second one. *)
  val subset : ?equal:('a -> 'a -> bool) -> 'a list -> 'a list -> bool

  (** adds the element to the list, if the list don't contains 
      it already. *)
  val add : 'a -> 'a list -> 'a list

  (** [remove elm set] returns the set that do not contain [elm]. *)
  val remove : 'a -> 'a list -> 'a list

  (** If these two lists are sets, the result is a set that
     contains all elements of the two given sets. *)
  val union : 'a list -> 'a list -> 'a list

  (** [inter l1 l2] contains the intersection of the two sets [l1]
      and [l2]. *)
  val inter : 'a list -> 'a list -> 'a list

  (** [diff l1 l2] contains all elements of [l1] that are
      not part of [l2] if [l1, l2] as sets. *)
  val diff : 'a list -> 'a list -> 'a list

  
  (** {2 Association lists } *)

  (** [assoc' a l == Some b] iff [assoc a l == b]. If
      [assoc a l -> throws Not_found], then [assoc' a l == None].
  *)
  val assoc' :  ?equal:('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b option

  (** {1 Functions from the standard libary } *)

  (** Same as original List module, see
      {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html}List}. 
  *)


  val length : 'a list -> int
  val hd : 'a list -> 'a
  val tl : 'a list -> 'a list
  val nth : 'a list -> int -> 'a
  val rev : 'a list -> 'a list
  val append : 'a list -> 'a list -> 'a list
  val rev_append : 'a list -> 'a list -> 'a list
  val concat : 'a list list -> 'a list
  val flatten : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val map : ('a -> 'b) -> 'a list -> 'b list
  val rev_map : ('a -> 'b) -> 'a list -> 'b list
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
  val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
  val fold_right2 :
    ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
  val for_all : ('a -> bool) -> 'a list -> bool
  val exists : ('a -> bool) -> 'a list -> bool
  val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val mem : 'a -> 'a list -> bool
  val memq : 'a -> 'a list -> bool
  val find : ('a -> bool) -> 'a list -> 'a
  val filter : ('a -> bool) -> 'a list -> 'a list
  val find_all : ('a -> bool) -> 'a list -> 'a list
  val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
  val assoc : 'a -> ('a * 'b) list -> 'b
  val assq : 'a -> ('a * 'b) list -> 'b
  val mem_assoc : 'a -> ('a * 'b) list -> bool
  val mem_assq : 'a -> ('a * 'b) list -> bool
  val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
  val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
  val split : ('a * 'b) list -> 'a list * 'b list
  val combine : 'a list -> 'b list -> ('a * 'b) list
  val sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
  val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
end
