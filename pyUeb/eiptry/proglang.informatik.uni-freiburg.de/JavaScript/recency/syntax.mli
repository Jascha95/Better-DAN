(** This module contains the AST. *)
open ProglangUtils
open Location


(** {2 Syntax } *)

(** expressions *)
type e = private
    SExp of s 
  | Let of plab * LSet.t option * var * s * e (* Let x = e in e *)
  | Mask of plab * LSet.t option * e

(** serious expressions *)
and s = private
    Val of v
  | App of plab * v * v 
  | New of plab * Loc.t 
  | Read of p
  | Write of plab * p * v
  | MethodCall of plab * p * v

(** values *)
and v = private
    Var of plab * var
  | Lam of plab * var * var * e
  | Undef of plab
  | Pointer of plab * P.t
  | Int of plab * int

and p = private 
    Prop of plab * v * label

(** variables *)
and var 

(** labels *)
and label 

(** program labels *)
and plab




(** {3 Expressions } *)

(** Creates an expression from a serious expression *)
val e_sexp : s -> e

(** Creates a let expression *)
val e_let : var -> s -> e -> e

(** Creates a mask expression (Mask[e]) *)
val e_mask : LSet.t -> e -> e

(** Returns a string representation of an expression *)
val string_of_e : e -> string

(** Returns an expression together with it's program locations as a string *)
val string_of_e_with_plabs : e -> string




(** {3 Serious expressions } *)

(** Creates a serious expression from a value *)
val s_val : v -> s

(** Creates a serious expression (application) *)
val s_app : v -> v -> s

(** Creates a serious expression (new) *)
val s_new : unit -> s

(** Creates a serious expression (new) with the given 
    abstract location *)
val s_new_loc : Loc.t -> s

(** Creates a serious expression (read) *)
val s_read : v -> label -> s

(** Creates a serious expression (write) *)
val s_write : v -> label -> v -> s

(** Creates a serious expression (method call) *)
val s_mcall : v -> label -> v -> s




(** {3 Values } *)

(** Creates a value (var) *)
val v_var : var -> v

(** Creates a value (lambda expression) *)
val v_lam : var -> var -> e -> v

(** Creates a value (undefined) *)
val v_undef : unit -> v

(** Creates an integer as a value *)
val v_int : int -> v


(** Returns a string representation of a value. Depending on the global
    option with_label then the program labels are part of the result string.
*)
val string_of_v : v -> string

(** Returns a string represenation of a lambda expression *)
val string_of_lam : var -> var -> e -> string

(** Returns a value together with it's program locations as a string *)
val string_of_v_with_plabs : v -> string

(** Returns a lambda expression together with it's program locations as a string *)
val string_of_lam_with_plabs : var -> var -> e -> string

(** Returns a string representation of a value *)
val string_of_v_without_plabs : v -> string




(** {3 Variables } *)

(** creates a new variable from a string *)
val create_var : string -> var

(** compares to variables. *)
val compare_var : var -> var -> int

(** Returns a string representation of a variable *)
val string_of_var : var -> string




(** {3 Labels } *)

(** creates a new label *)
val create_label : string -> label

(** comapres to labels *)
val compare_label : label -> label -> int

(** Returns a string representation of a variable *)
val string_of_label : label -> string




(** {3 Program labels } *)

(** create a program label *)
val create_plab : unit -> plab

(** comapre to program labels *)
val compare_plab : plab -> plab -> int

(** Returns a string represenation of a program label *)
val string_of_plab : plab -> string

(** returns the program label *)
val e_get_plab : e -> plab

(** returns the program label *)
val s_get_plab : s -> plab

(** returns the program label *)
val v_get_plab : v -> plab

(** returns the program label *)
val p_get_plab : p -> plab

(** collects all locations from a program *)
val collect_locs : e -> LSet.t


(** {3 Free Variables  } *)

module VarSet : ExtSSet.S with type elt = var

(** [free_e e] computes the free variables of [e] and 
    returns it as a set. *)
val free_e : e -> VarSet.t

(** [free_s s] computes the free variables of [s] and 
    returns it as a set. *)
val free_s : s -> VarSet.t

(** [free_e v] computes the free variables of [v] and 
    returns it as a set. *)
val free_v : v -> VarSet.t

module LabelSet : ExtSSet.S
module LabelSetRef : sig
  include ExtSSetImp.S 
  val laset : t -> LabelSet.t
end
  with type elt = label


val set_print_with_label : unit -> unit
val set_print_without_label : unit -> unit
