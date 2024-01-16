(** Provides some utility functions 
    @author Stefan Wehr
    @author Phillip Heidegger
*)

module Utils : sig

  (** Type of the two supported operation systems. *)
  type os_type = Win32 | Unix | Cygwin
      
  (** Error type *)
  exception Error of string
    
  (** returns the operation system *)
  val get_os_type : os_type
    
  (** [replicate x a] returns [a;a;...;a], where the length 
      of the list is [x].
  *)
  val replicate : int -> 'a -> 'a list

  (** [apply f x] runs the function [f] and gives [x] as parameter. *)
  val apply : ('a -> 'b) -> 'a -> 'b

  (** [apply' x f] runs the function [f] and gives [x] as parameter. *)
  val apply' : 'a -> ('a -> 'b) -> 'b
  
  (** type of a total ordering function *)
  type 'a cfun = 'a -> 'a -> int

  (** compares two values *)
  val comp : 'a cfun 

  (** Returns a total ordering of tuppels with two components *)
  val compare_2_tup : 'a cfun * 'b cfun -> 'a * 'b -> 'a * 'b -> int 

  (** Returns a total ordering of tuppels with three components *)
  val compare_3_tup : 'a cfun * 'b cfun * 'c cfun 
      -> 'a * 'b * 'c -> 'a * 'b * 'c -> int

  (** bla *)
  val (<.<) : int -> (unit -> int) -> int

  (** [fixpoint f a] applies [f] to [a], then [f] to [f a], and so on, until
      a fixpoint is reached and [f x == x] for some [x = f ... f a]. 

      Waring: This could yield to an infinite loop. A possibility to
      ensure termination of this fixpoint computation is to use only monotone 
      functions [f] satisfying the ascending chain condition.
  *) 
  val fixpoint : ('a -> 'a) -> 'a -> 'a

  (** [compare_to_equal f] returns a function that returns [true]
      of f a b == 0, otherwise it returns [false]. *)
  val compare_to_equal : ('a -> 'a -> int) -> 'a -> 'a -> bool

  (** [flip f] turns the order of parameters of [f], if [f] is
      a function with two parameters. *)
  val flip : ('a -> 'b -> 'c) -> ('b -> 'a -> 'c)

end
