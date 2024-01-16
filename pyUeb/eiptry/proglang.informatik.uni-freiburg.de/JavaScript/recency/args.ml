(*  - Add more test cases *)
(** This file checks the arguments given to the program
    throw the command line or throw environment variables. 
    @author: Phillip Heidegger 
    @author: Stefan Wehr *)
open ProglangUtils
open ExtString


type test_args = { t_log_level : Log.level; }
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

exception EMissingValue of string
exception Error of string

(* We use our own type for argument specifications to support
   default values coming from the environment. *)
type my_spec = FromArg of Arg.spec 
             | FromEnvAndArg of (string * (string -> unit))

let my_spec_to_spec : Arg.key list * my_spec * string list -> 
  (Arg.key * Arg.spec * string list) list 
  =
  fun (key_list, spec, doc) -> 
    let s,d = 
      ((match spec with
            FromArg s -> s
          | FromEnvAndArg(_, f) -> Arg.String f),
       doc)
    in
    let add_alias spec_list key =
      match spec_list with
          None -> Some ([(key,s,d)],key)
        | Some (sl,first_key) ->
            let d = "Shortcut for: " ^ first_key in
              Some ((key,s,[d]) :: sl, first_key)
    in
      match List.fold_left add_alias None key_list with
        | None -> []
        | Some (sl,_) -> List.rev sl
    
let align l =
  let max_key = 
    (List.fold_left 
      (fun cur (key,_,_) -> max cur (String.length key)) 
      0
      l) + 2 
  in
  let align (key, spec, doc_lines) =
    let n = max_key - String.length key in
    let n' = 3 + max_key in
    let indent_first = String.make n ' ' in
    let indent_rest = String.make n' ' ' in
    let doc = 
      match doc_lines with
        | [] -> ""
        | x::[] -> indent_first ^ x
        | x::xs -> 
            indent_first ^ x ^ "\n" ^ 
              (String.concat "\n" (List.map (fun y -> indent_rest ^ y) xs))
    in
      (key, spec, doc)
  in
    Arg.align (List.map align l)

let parse_with_defaults 
    getenv argv specs anon_fun help =
  let _ =
    List.iter
      (fun (_, spec, _) ->
          match spec with
            | FromArg _ -> ()
            | FromEnvAndArg(s,f) ->
                (try
                   let v = getenv s in
                     Log.full_debug (lazy ("found " ^ s ^ " = " ^ v
                                      ^ " in environment"));
                     f v
                 with
                     Not_found -> ()))
      specs
  in
  let arg_specs = 
    align (List.flatten (List.map my_spec_to_spec specs))
  in
    Arg.parse_argv
      ~current:(ref 0)
      argv
      arg_specs
      anon_fun
      help

let parse' getenv argv =
  (*
   * References needed to cooperate with the Arg module
   *)
  let log_file = ref (Path.to_abs (Path.make "m.log")) in
  let log_level = ref Defaults.log_level in
  let test = ref false in
(*  let terminal = ref "utf8" in *)
  let file = ref None in
  let mode = ref Solve in
  let loc = ref false in
  (* 
   * Set functions 
   *)
(*  let set_log_file s = log_file := Path.to_abs (Path.make s) in *)
  let set_log_level s = 
    try
      (* check if string is a valid log level *)
      let level = Log.level_of_string s in
        log_level := level
    with Invalid_argument _ ->
      Log.warn (lazy ("invalid log level " ^ s ^ 
                        ", proceeding with the default"))
  in
(*  let high_log_level_full_info _ =
    set_log_level "full-info"
  in *)
  let set_test () = test := true in
(*  let set_utf8 () = terminal := "utf8" in *)
(*  let set_latin1 () = terminal := "latin1" in *)
  let set_file f = file := Some (Path.to_abs (Path.make f)) in
  let set_mode m () = mode := m in
  let set_with_loc () = loc := true in
  let rec show_help () =
    raise (Arg.Bad "Use the program with") 
  (*
   * Specification for supported environment variables and
   * commandline arguments.
   *)
  and spec =
    [
     (["--log-level"; "-ll"],
      FromEnvAndArg("RUN_LOG_LEVEL", set_log_level),
      ["One of: "; "\"warn\""; "\"info\""; "\"full-info\""; 
       "\"debug\" or \"full-debug\""; "Default: info"])
    ;
     (["--test"; "-t"],
      FromArg (Arg.Unit set_test),
      ["Runs in testmode"])
     ;
     (["--help";"-help";"-h"],
      FromArg (Arg.Unit show_help),
      ["Shows the help"])
     ;
     (["--lex-only";"-l"],
      FromArg (Arg.Unit (set_mode LexOnly)),
      ["Set mode to lex only.";"Print the lexed token to stdout."])
     ;
     (["--parse-only";"-p"],
      FromArg (Arg.Unit (set_mode ParseOnly)),
      ["Set mode to parse only.";
       "The AST is printed to stdout with generated program locations."])
     ;
     (["--gen-only";"-g"],
      FromArg (Arg.Unit (set_mode GenOnly)),
      ["Set mode to generate only.";
       "The constraints are printed to stdout."])
     ;
     (["--solve";"-s"],
      FromArg (Arg.Unit (set_mode Solve)),
      ["Set mode to solve. This is the default mode."])
     ;
     (["--with-locations";"-wl"],
      FromArg (Arg.Unit (set_with_loc)),
      ["Program output is printed with the locations.";
       "New Locations are always printed!"
      ])
    ]
  and help = 
      "run [OPTIONS] file\n"
      ^" \n OPTIONS:"
  in
  (*
   * Parsing
   *)
  let _ = 
    try
      parse_with_defaults getenv argv spec set_file help
    with 
      | Arg.Help s | Arg.Bad s ->
          raise (Error s)
  in
  (* 
   * Get functions 
   *)
  let get_log_file () = !log_file in
  let get_log_level () = !log_level in
  let get_file () = !file in
  let get_mode () = !mode in
  let get_with_loc () = !loc in
(*  let get_terminal () = !terminal in *)
(*  let get_file () =
    match !file with
        None -> Path.to_abs (Path.make "newfile")
      | Some f -> f
  in *)
  (*
   * Compute the result
   *)
  let get_test () = !test in
    if get_test () then 
      TestMode 
        { t_log_level = get_log_level ();
        }
    else
      try
        NormalMode 
          { log_file = get_log_file ();
            log_level = get_log_level ();
            file = get_file ();
            mode = get_mode ();
            with_loc = get_with_loc ();
          }
      with EMissingValue s ->
          (* Error found in arguments or environment, 
             so show help page and exit *)
        raise (Error s)

let parse _ = parse' Sys.getenv Sys.argv

open String_of 

let string_of_mode = function
  | LexOnly -> "LexOnly"
  | ParseOnly -> "ParseOnly"
  | GenOnly -> "GenOnly"
  | Solve -> "Solve"

let string_of = function
    NormalMode p ->
      "NormalMode {"
      ^"\n logfile = '"^ Path.A.string_of p.log_file ^"'"
      ^"\n loglevel = '"^ (Log.string_of_level p.log_level) ^"'"
      ^"\n file = '"^(match  p.file with None -> "" | Some p -> Path.A.string_of p) ^"'"
      ^"\n mode = '"^(string_of_mode p.mode)^"'"
      ^"\n}"
  | TestMode p ->
      "TestMode  {"
      ^"\n t_loglevel = " ^ (Log.string_of_level p.t_log_level) 
      ^"\n}"

(* Test code *)
(* TODO: add some more tests *)
module PropTest = 
struct
  open Test
    
  let compare_prop_strings sout sexp =
    ("\nGiven:\n" ^ sout ^ "" 
      ^ "\nExpected:\n" ^ sexp ^ "\n"
      ^ "Length: g=" ^ (string_of_int (String.length sout)) ^ "\n"
      ^ "Length: e=" ^ (string_of_int (String.length sexp)) ^ "\n"
      ^ (string_of_int (String.compare sout sexp) )
    )
      
  let make_getenv l s = List.assoc s l
      
  (* Test 1 *)
  let t1 () = 
    let p_exp = 
      NormalMode 
        { log_file = Path.to_abs (Path.make "m.log");
          log_level = Log.level_of_string "warn";
          file = None;
          mode = Solve;
          with_loc = false;
        }
    in
    let envlist = [] in
    let argv = [| "run"; "--log-level"; "warn" |] in
    let p = parse' (make_getenv envlist) argv in
      assert_equal
        ~cmp:(=)
        ~printer:string_of
        p_exp
        p

  (* Test 2 *)
  let t2 () =
    let p_exp = 
      NormalMode 
        { log_file = Path.to_abs (Path.make "m.log");
          log_level = Log.level_of_string "info";
          file = Some (Path.to_abs (Path.make "m.mny"));
          mode = Solve;
          with_loc = false;
        } 
    in
    let envlist = [] in
    let argv = [| "run"; "m.mny" |]
    in
    let p = parse' (make_getenv envlist) argv in
      assert_equal
        ~cmp:(=)
        ~printer:string_of
        p_exp
        p
        
  let _ =
    install_tests "ParseArgs"
      (fun _ -> [("Test 1", t1);
                 ("Test 2", t2)])
end
