(** Unit for doing the tests. 

    @author Stefan Wehr, Phillip Heidegger
*)

(** {2 {!OUnit} functions } *)

(** *)
val assert_failure : string -> 'a
val assert_bool : string -> bool -> unit
val ( @? ) : string -> bool -> unit
val assert_string : string -> unit
val assert_equal : ?cmp:('a -> 'a -> bool) ->  ?printer:('a -> string) -> 
  ?msg:string -> 'a -> 'a -> unit
val assert_raises : ?msg:string -> exn -> (unit -> 'a) -> unit
val cmp_float : ?epsilon: float -> float -> float -> bool
val bracket : (unit -> 'a) -> ('a -> 'b) -> ('a -> 'c) -> unit -> 'c
  
(** {2 Functions for manipulating the global test case list } *)
  
(** type of a test function *)
type test_fun = unit -> unit
  
(** List of Unit tests for one module *)
type tests = (string * test_fun) list
    
(** [install_tests "module" f] adds for the module 
    give as first parameter all test into the test suite,
    if run tests is called. The function [f] returns the
    tests. It is not directly given to avoid computing the
    tests if they are not called. The tests are added into
    the global test case list.
*)
val install_tests : string -> (unit -> tests) -> unit
  
(** Runs all tests if [None] is given. If [Some l] is given,
    [l] is the list of modules witch tests should be executed. *)
val run_tests : string list option -> bool
  
val test_program : string -> test_fun
