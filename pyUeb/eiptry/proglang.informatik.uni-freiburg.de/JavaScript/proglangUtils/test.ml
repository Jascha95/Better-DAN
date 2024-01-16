include OUnit

type test_fun = unit -> unit
type tests = (string * test_fun) list
    

let g_all_tests : (string * (unit -> tests)) list ref = ref []
  
let install_tests name tests =
  g_all_tests := !g_all_tests @ [(name,tests)]

let is_good l = 
  match l with
    | None -> (fun _ -> true)
    | Some l -> (fun (name,_) -> List.mem name l)
  
let run_tests l =
  (* adds the test of a module into the the tests *)
  let add_tests tests (name,t_f) =
    if (is_good l (name,t_f)) then
      (* init tests *)
      let t_tests = t_f () in
      (* convert for OUnit *)
      let t = 
        TestLabel(name,
                  TestList (List.map 
                              (fun (n,f) -> TestLabel(n, TestCase f))
                              t_tests))
      in 
        tests @ [t]
    else
      tests
  in
  (* collect all tests that should be run, inizialize them *)
  let all_tests = 
    List.fold_left 
      add_tests 
      []
      (!g_all_tests)
  in
  let res = run_test_tt (TestList all_tests) in
  let is_success = function
    | RSuccess _ -> true
    | _ -> false
  in
    List.for_all is_success res
      
let test_program prog () =
  let log_file = "homegen_tests.log" in
  let _ = output_string stderr ("running test program \"" ^ prog ^ "\"\n") in
  let _ = flush stderr in
  let ecode = Sys.command (prog ^ " 1>> " ^ log_file ^ " 2>> " ^ log_file) in
  let _ = output_string stderr ("test program \"" ^ prog ^ "\" finished\n") in
    if ecode <> 0 then
      OUnit.assert_failure ("test program " ^ String_of.quote prog 
                            ^ " failed, output can be found in " ^ log_file)
    else ()
