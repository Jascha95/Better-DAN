open ProglangUtils
open String_of
open Syntax
open Inf
open GenVars
open Lexer
open Parser
open Gen
open PrioQueue
open LowerUpper
open LowerUpperNeg
open LUNVar
open LUVar
open MapVar
open NoValue
open MapMod
open Driver
open Gtest


let run_normal mode plabs = function
  | None -> 
      Log.error 
        ("You have to give a file as an argument. For more details pass --help.")      
  | Some p ->
      if plabs then Syntax.set_print_with_label ();
      if (Path.A.file_exists p) then begin
        let f = open_in (Path.A.string_of p) in
        let l = Lexing.from_channel f in
          match mode with
            | Args.LexOnly -> 
                let rec loop () =
                  let t = Lexer.token l in
                    match t with
                      | LEof -> ()
                      | t ->
                          print_endline (token_to_string t);
                          loop () 
                in
                  loop ()
            | Args.ParseOnly -> parse (fun e s -> print_endline s) l
            | Args.GenOnly -> gen (fun p -> print_endline p.sc) l
            | Args.Solve -> solve (fun p -> ()) l
      end else begin
        Log.error 
          ("The file you pass as parameter does not exists:"
           ^(Path.A.string_of p))
      end


let _ = 
  (* here starts the program *)
  (* first set the initial log level, then parse the
     command line arguments. *)
  let _ = Log.enable_level Defaults.log_level in
  let p = 
    try
      Args.parse ()
    with
      | Args.Error s ->
          prerr_endline s;
          exit 1
  in
  let log_args _ = 
    Log.full_debug (lazy ("Raw commandline arguments: " ^ 
                            string_of_array (quote' identity) Sys.argv));
    Log.debug (lazy ("Finished computing properties, result: " ^
                       Args.string_of p))
  in

    (* run tests, if the args say we should run in test mode,
       otherwise start the normal mode. *)       
    match p with
        Args.TestMode ta ->
          let _ = Log.enable_level ta.Args.t_log_level in
            ignore (Test.run_tests None)
      | Args.NormalMode na ->
          let _ = Log.enable_level na.Args.log_level in
          let lf = Log.open_log_file (Path.A.string_of na.Args.log_file) in
          let _ = Log.add_log_target lf in 
              ignore (run_normal na.Args.mode na.Args.with_loc na.Args.file)
