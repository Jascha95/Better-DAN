(** Module for handle the arguments of the program. *)

open ProglangUtils 

type test_args = { t_log_level : Log.level;
                 }

type mode = 
  | LexOnly
  | ParseOnly
  | GenOnly
  | Solve

type normal_args = { log_file : Path.abs;
                     log_level : Log.level;
                     file: Path.abs option;
                     mode : mode;
                     with_loc : bool;
                   }
type t = 
    NormalMode of normal_args
  | TestMode of test_args

val parse : unit -> t
val string_of : t -> string

val align : (Arg.key * Arg.spec * Arg.doc list) list 
         -> (Arg.key * Arg.spec * Arg.doc) list

exception Error of string
