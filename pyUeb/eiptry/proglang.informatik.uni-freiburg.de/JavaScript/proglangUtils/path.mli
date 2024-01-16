(** This module provides methods for handling path.
    It know absolute and relative path, and can
    concatenate these two kinds. 
    @author Phillip Heidegger
    @author Stefan Wehr
*)

(** relative paths *)
type rel

(** absolute paths *)
type abs

(** relative or absolute paths *)
type t = Abs of abs | Rel of rel

(** *)
exception Error of string

(** If this error is throw, it's not possible to concatenate the two paths.
    For example the two paths [/a/b] and [../../../x], the three [..]
    leads to a not existing position. *)
exception EConcat_path of abs * rel * string

(** Is was not possible to make a canonical representation of
    the path. *)
exception ECanonicalize of string

(** creates a path from a string *)
val make : string -> t

(** concatenate to paths *)
val concat : abs -> t -> abs

val to_abs : t -> abs

(** returns the string of a path *)
val string_of : t -> string

(** This module is for the relative Paths. *)
module R :
sig
  exception ERemove_prefix of rel * rel * string
  val make : string -> rel
  val make_from_components : string list -> rel
  val concat : rel -> rel -> rel
  val concat' : rel list -> rel
  val chop_extension : rel -> rel
  val basename : rel -> rel
  val dirname : rel -> rel
  val string_of : rel -> string
  val filter_dir_components : (string -> bool) -> rel -> rel
  val components : rel -> string list
  val compare : rel -> rel -> int
  val remove_prefix : rel -> rel -> rel
  val is_prefix : rel -> rel -> bool
end

(** This module provides the functions for absolute paths *)
module A :
sig
  exception ERemove_prefix of abs * abs * string
  val make : string -> abs
  val concat : abs -> rel -> abs
  val chop_extension : abs -> abs
  val basename : abs -> rel
  val dirname : abs -> abs
  val open_temp_file : ?mode:open_flag list -> string -> string 
                     -> abs * out_channel
  val executable_name : abs
  val getcwd : unit -> abs
  val is_prefix : abs -> abs -> bool
  val remove_prefix : abs -> abs -> rel
  val remove_prefix_list : abs -> abs list -> rel list 
  val canonicalize : abs -> abs
  val file_exists : abs -> bool
  val components : abs -> string list
  val string_of : abs -> string
  val compare : abs -> abs -> int
end
