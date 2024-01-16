(** Module that provides methods for logging to 
    a file or to [stdout]. 

    @author Stefan Wehr, Phillip Heidegger
*)

(** type of log levels *)
type level

(** one log level *)
val level_full_debug : level

(** one log level *)
val level_debug : level 

(** one log level *)
val level_full_info  : level

(** one log level *)
val level_info : level

(** one log level *)
val level_warn  : level

(** one log level *)
val level_error : level

(** one log level *)
val level_fatal : level

(** Converts string into level. *)
val level_of_string : string -> level

(** Converts level into string. *)
val string_of_level : level -> string

(** enable the given level and all level with
    higher priority. If you enable e.g. [debug], 
    then [full_info], and [warn] are also enabled. *)
val enable_level : level -> unit

(** disable the level and all levels with lower
    priority. *)
val disable_level : level -> unit

(** *)
val set_level : level -> unit

(** Checks if level is enabled. *)
val is_enabled : level -> bool

(** *)
val toggle_level : level -> unit

(** run a function and sets for this function
    the log level to the given one. *)
val with_level : level -> (unit -> 'a) -> 'a

(** list of log level names *)
val level_names : string list

(** Disable the ansi escape sequence usage, if
    you log to [stdout]. *)
val disable_ansi_esc : unit -> unit

(** Disable the ansi color escape sequence usage, if
    you log to [stdout]. *)
val disable_ansi_color : unit -> unit

(** Change the log target *)
val change_log_target : out_channel -> unit

(** add another log target *)
val add_log_target : out_channel -> unit

(** Open a log file *)
val open_log_file : string -> out_channel

(** Full Debug log *)
val full_debug : string Lazy.t -> unit

(** Debug log *)
val debug : string Lazy.t -> unit

(** Full info log *)
val full_info : string Lazy.t -> unit

(** Info log *)
val info : string Lazy.t -> unit

(** Warning log *)
val warn : string Lazy.t -> unit

(** Error log *)
val error : string -> unit

(** Fatal log *)
val fatal : string -> 'a

(** Exception that is thrown when [fatal] is called. *)
exception Log_fatal of string
