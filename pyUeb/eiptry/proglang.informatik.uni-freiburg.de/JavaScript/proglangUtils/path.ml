(* 
   A relative path is represented as a pair (i,l) where l is a list
   of path components (not including "." and upward references "..")
   and i is a positive integer specifying the number of unresolved upward
   references at front of the path.
   
   Example: The path "a/../../b/c/.././d" is represented as (1, ["b"; "d"]).
*)
open String_of
open ExtList
open ExtUtils

type components = string list

type ends_with_slash = bool
type abs = components * ends_with_slash
type rel = int * components * ends_with_slash
type t = Abs of abs | Rel of rel


exception Error of string
exception ERemove_prefix of string
exception EConcat_path of abs * rel * string
exception ECanonicalize of string
exception EBadExtensions of int

let path_sep =
  match Utils.get_os_type with
    | Utils.Win32 -> "\\"
    | _ -> "/"

module R =
struct
  open ExtString

  exception ERemove_prefix of rel * rel * string

  let gen_make_from_components l last_slash =
    let rec norm = function
      | [] -> (0, [])
      | x::xs ->
          let (i,l) = norm xs in
            match x with
              | "." -> (i,l)
              | ".." -> (i+1,l)
              | y -> if i > 0 then (i-1,l) else (i,y::l)
    in
    let l = List.filter (fun s -> String.length s > 0) l in
    let (i, comps) = norm l in
      (i, comps, last_slash)

  let make_from_components l = gen_make_from_components l false

  let unsafe_make s =
    let l = String.nsplit s path_sep in
    let last_slash = String.ends_with s path_sep in
      gen_make_from_components l last_slash

  let make s =
    if not (Filename.is_relative s) then
      raise (Error ("Cannot treat " ^ s ^ " as a relative path."))
    else
      unsafe_make s

  let concat (i,l,_) (i',l',b) =
    let n = List.length l - i' in
      if n < 0 then (i-n,l',b)
      else
        let new_l = List.take n l in
          (i, new_l @ l', b)    

  let concat' = function
      [] -> failwith "File.concat': cannot concatenate empty list"
    | x::xs -> List.fold_left concat x xs

  let string_of (i,l,b) =
    let l' = (Utils.replicate i ".." @ l) in
    let s = 
      match l' with
        | [] -> "."
        | _ -> String.concat path_sep l'
    in if b then s ^ path_sep else s

  let chop_extension (i,l,_) =
    match List.rev l with
      | [] -> (i,[],false)
      | (x::xs) ->
          let x' = Filename.chop_extension x in
            (i, List.rev (x'::xs),false)

  let basename (i,l,_) =
    match List.rev l with
      | [] -> (0,[],false)
      | (x::_) -> (0,[x],false)

  let dirname (i,l,_) =
      match List.rev l with
        | [] -> (max 0 (i-1), [], true)
        | (_::xs) -> (i, List.rev xs, true)
      
  let ends_with_slash (_,_,b) = b

  let set_ends_with_slash (i,c,_) b = (i,c,b)

  let filter_dir_components f p =
    let (i,l,_) = dirname p in
    let base = basename p in
    let b = ends_with_slash p in
      set_ends_with_slash (concat (i, List.filter f l, true) base) b

  let components (i,l,_) =
    Utils.replicate i ".." @ l

  let compare = Pervasives.compare

  let remove_prefix' (i,l,b) (i',l',b') e =
    let rec loop = function
      | ([], ys) -> ys
      | (x::xs, y::ys) ->
          if x = y then loop (xs,ys)
          else raise e
      | (x::_, []) -> raise e
    in 
    let is_empty = function
        [] -> true
      | _ -> false
    in
      if i < i' && is_empty l then (i'-i,l',b')
      else if i > i' then raise e
      else
        (0, loop (l,l'), b')

  let remove_prefix p p' = 
    remove_prefix' p p' 
      (ERemove_prefix 
         (p, p', string_of p ^ " is not a prefix of " ^ string_of p'))

  let is_prefix (i,l,_) (i',l',_) =
    let rec loop = function
      | ([], ys) -> true
      | (x::xs, y::ys) ->
          if x = y then loop (xs,ys)
          else false
      | (x::_, []) ->
          false
    in (i=i') && loop (l,l')

end

module A =
struct

  exception ERemove_prefix of abs * abs * string

  let string_of (l,b) = path_sep ^ R.string_of (0,l,b)

  let concat ((l,b) as abs) rel =
    let (j,new_l,new_b) = R.concat (0,l,b) rel in
      if j > 0 then
        raise (EConcat_path (abs,
                             rel,
                             "Cannot concatenate " ^ string_of abs
                             ^ " and " ^ R.string_of rel 
                             ^ ": the result would have unresolved " 
                             ^ "upward references."))
      else
        (new_l,new_b)

  (* we read the current working directory at startup, assuming
     no other module has already changed to working directory. *)
  let cwd = 
    let x = Sys.getcwd () in
      match R.unsafe_make x with
        | (0,l,b) -> (l,b)
        | (i,_,_) -> Log.fatal ("Sys.getcwd return " ^ x ^ " which has "
                              ^ string_of_int i 
                              ^ " unresolved upward references.")

  let make s = 
    if Filename.is_relative s 
    then concat cwd (R.make s)
    else 
      match R.unsafe_make s with
        | (0,l,b) -> (l,b)
        | (i,_,_) -> 
            raise (Error (s ^ " does not denote an absolute path: it has "
                          ^ string_of_int i 
                          ^ " unresolved upward references."))

  let to_rel (l,b) = (0,l,b)

  let basename abs = R.basename (to_rel abs)

  let dirname abs =
    let (_,l,b) = (R.dirname (to_rel abs)) in
      (l,b)

  let chop_extension abs = 
    let (_,l,b) = (R.chop_extension (to_rel abs)) in
      (l,b)

  let open_temp_file ?mode:(m=[Open_text]) pref suf =
    let (s,ch) = Filename.open_temp_file ~mode:m pref suf in
      (make s, ch)

  let executable_name = make Sys.executable_name

  let getcwd _ = make (Sys.getcwd ())

  let file_exists p = Sys.file_exists (string_of p)

  let gen_canonicalize is_symlink read_symlink path =
    let max_symlinks = 10 in
    let rec loop : components -> components -> int -> components = fun l acc n ->
      if n > max_symlinks then
        raise (Error ("Too many symbolic links encountered while "
                       ^ "canonicalizing " ^ string_of path))
      else
        match l with
          | [] -> acc
          | x::xs ->
              let acc_x = acc @ [x] in
              let p = string_of (acc_x,false) in
                if not (is_symlink p)
                then
                  loop xs (acc_x) n
                else
                  let real = read_symlink p in
                  let (new_acc,_) = 
                    if Filename.is_relative real then
                      concat (acc,false) (R.make real)
                    else
                      make real
                  in
                    loop (new_acc @ xs) [] (n+1)
    in
    let l = loop (fst path) [] 0 in
      (l, snd path)

  let canonicalize path =
    let is_symlink file =
      let stat = 
        try Unix.lstat file 
        with _ -> raise (ECanonicalize file)
      in
        match stat.Unix.st_kind with
            Unix.S_LNK -> true
          | _ -> false
    in
    let readlink file = 
      try Unix.readlink file
      with _ -> raise (ECanonicalize file)
    in
      gen_canonicalize is_symlink readlink path

  let is_prefix (l,_) (l',_) =
    let rec loop = function
      | ([], ys) -> true
      | (x::xs, y::ys) ->
          if x = y then loop (xs,ys)
          else false
      | (x::_, []) ->
          false
    in loop (l,l')
        
  let remove_prefix abs abs'= 
    R.remove_prefix' (to_rel abs) (to_rel abs')
      (ERemove_prefix 
         (abs, abs', string_of abs ^ " is not a prefix of " ^ string_of abs'))


  let components (l,_) = l

  let compare = Pervasives.compare    

  let remove_prefix_list dir fl =
    List.map 
      (fun f -> remove_prefix dir (canonicalize f))
      fl

end

let make x =
  if Filename.is_relative x then Rel (R.make x) else Abs (A.make x)

let concat abs = function
    Abs x -> x
  | Rel x -> A.concat abs x

let string_of = function
    Abs x -> A.string_of x
  | Rel x -> R.string_of x


let to_abs = function
    Abs a -> a
  | Rel r -> A.concat (A.getcwd ()) r
        
(* Test code *)
module PathTest = 
struct
  open Test

  let test_make_rel () =
    let spec = [("a//b", 0, ["a";"b"], false);
                ("a", 0 ,["a"], false);
                ("a/", 0 ,["a"], true);
                ("", 0, [], false);
                ("./././", 0, [], true);
                (".", 0, [], false);
                ("../a/.", 1, ["a"], false);
                ("a/../../b/c/.././d", 1, ["b"; "d"], false);
                ("../../", 2, [], true)
               ]
    in
      List.iter (fun (s,u,l,b) -> assert_equal (u,l,b) (R.make s) ) spec

  let test_rel_string_of () =
    let spec = [(0, ["a";"b";"c"], false, "a/b/c");
                (2, ["a";"b";"c"], true, "../../a/b/c/");
                (1, [], false, "..")
               ]
    in
      List.iter (fun (u,l,b,s) -> assert_equal s (R.string_of (u,l,b))) spec
    
  let test_concat_rel () =
    let spec = [("a/b", "c", "a/b/c");
                ("../../a/b", "c/", "../../a/b/c/");
                ("a/b", "../c", "a/c");
                ("a/b", "../../c", "c");
                ("a/b", "../../../c", "../c")
               ]
    in
      List.iter 
        (fun (s1,s2,s3) -> 
           let r1 = R.make s1 in
           let r2 = R.make s2 in
           let r = R.concat r1 r2 in
             assert_equal ~printer:(fun x -> x) s3 (R.string_of r))
        spec

  let test_misc_rel () =
    assert_equal "a/b/c" (R.string_of (R.chop_extension (R.make "a/b/c.txt")));
    assert_equal "c.txt" (R.string_of (R.basename (R.make "a/b/c.txt")));
    assert_equal "a/b/" (R.string_of (R.dirname (R.make "a/b/c.txt")))

  let test_matches_rel () =
    assert_equal 1 2

  let test_abs_string_of () =
    assert_equal "/a/b/c" (A.string_of (["a";"b";"c"], false));
    assert_equal "/a/b/c/" (A.string_of (["a";"b";"c"], true))

  let test_concat_abs () =
    let spec = [("/a/b", "c", "/a/b/c");
                ("/a/b", "../c/", "/a/c/");
                ("/a/b", "../../c", "/c");
                
               ]
    in
    let conc s1 s2 =
      let a = A.make s1 in
      let r = R.make s2 in
        A.concat a r
    in
      List.iter 
        (fun (s1,s2,s3) -> 
           let a = conc s1 s2 in
             assert_equal ~printer:(fun x -> x) s3 (A.string_of a))
        spec;
      try
        let _ = conc "/a/b" "../../../../../c" in
          assert_failure "exception expected"
      with
          EConcat_path _ -> ()

  let test_filter_dir_components () =
    let p = R.make "../../a/www/b/www" in
    let exp = R.make "../../a/b/www" in
      assert_equal exp (R.filter_dir_components (fun s -> s <> "www") p)

  let test_canonicalize () =
    let symlink_map =
      [("/a/b/c", "d");
       ("/a/b/d", "../../d");
       ("/d/abs", "/x/y/z");
       ("/foo/bar", "/bar/foo");
       ("/bar/foo", "/foo/bar");
       ("/entry", "foo/bar")]
    in
    let is_symlink s = List.mem_assoc s symlink_map in
    let read_symlink s = List.assoc s symlink_map in
    let canonicalize s = 
      let p = A.gen_canonicalize is_symlink read_symlink (A.make s) in
        A.string_of p
    in
      assert_equal ~printer:(fun s -> s) "/stefan/phillip" 
        (canonicalize "/stefan/phillip");
      assert_equal ~printer:(fun s -> s) "/d/x" (canonicalize "/a/b/c/x");
      assert_equal ~printer:(fun s -> s) "/x/y/z" (canonicalize "/a/b/c/abs");
      (try
          ignore (canonicalize "/entry");
          assert_failure "exception expected because of loop in symlinks"
        with Error _ -> ())

  let _ =
    install_tests "path"
      (fun _ -> [("test_make_rel", test_make_rel);
                 ("test_rel_string_of", test_rel_string_of);
                 ("test_concat_rel", test_concat_rel);
                 ("test_misc_rel", test_misc_rel);
                 ("test_abs_string_of", test_abs_string_of);
                 ("test_concat_abs", test_concat_abs);
                 ("test_filter_dir_components", test_filter_dir_components);
                 ("test_canonicalize", test_canonicalize)
                ])
end
