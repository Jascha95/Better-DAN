module type S = sig
  val prefix : string
end

module type HI = sig
  type t
  val create_new : unit -> t
  val compare : t -> t -> int
  val string_of : t -> string
  val reset : unit -> unit
  val from_int : int -> t
end

module Make : functor (X : S ) -> HI 
  = functor ( X : S ) -> 
struct
    type t = int
    let i = ref 0
    let create_new () = 
      let j = !i in
        i := !i + 1;
      j        
    let compare = Pervasives.compare
    let string_of i = X.prefix ^ string_of_int i
    let reset () = i := 0
    let from_int x =
      i := max (x+1) (!i+1);
      x
end
