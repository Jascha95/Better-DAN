open Location
open ProgLabel
open ProglangUtils

type lt = WithoutLabel | WithLabel

let print_mode = ref WithoutLabel
let set_print_with_label () = print_mode := WithLabel
let set_print_without_label () = print_mode := WithoutLabel
let do_on_pl f1 f2 =
  match !print_mode with
    | WithLabel -> f1 ()
    | WithoutLabel -> f2 ()

type e = 
    SExp of s 
  | Let of plab * LSet.t option * var * s * e (* Let x = e in e *)
  | Mask of plab * LSet.t option * e
and s =
    Val of v
  | App of plab * v * v 
  | New of plab * Loc.t 
  | Read of p
  | Write of plab * p * v
  | MethodCall of plab * p * v 
and v = 
    Var of plab * var
  | Lam of plab * var * var * e
  | Undef of plab
  | Pointer of plab * P.t
  | Int of plab * int
and p = Prop of plab * v * label
and plab = ProgLabel.t

and var = string
and label = string

let id x = x
let create_var = id
let create_label = id
let create_plab = ProgLabel.create_new
let compare_plab = ProgLabel.compare
let string_of_plab = ProgLabel.string_of


let c_new = ProgLabel.create_new

let rec prop v a = Prop (c_new (), v, a)

and v_var var = Var (c_new (), var)
and v_lam x y e = Lam (c_new (),x,y,e)
and v_undef () = Undef (c_new ())
and v_int i = Int (c_new (),i)

and s_val v = Val v
and s_var var = s_val (v_var var)

and s_app v1 v2 = App (c_new (),v1,v2)
and s_new () = New (c_new (), Loc.create ())
and s_new_loc l = New (c_new (), l)
and s_read v a = Read (prop v a)
and s_write v1 a v2 = Write (c_new (), prop v1 a,v2)
and s_mcall v1 a v2 = MethodCall (c_new (),prop v1 a,v2)
and s_lam x y e = Val (v_lam x y e)

and e_sexp s = SExp s
and e_new () = e_sexp (s_new ())
and e_write v1 a v2 = e_sexp (s_write v1 a v2)
and e_lam x y e = e_sexp (s_lam x y e)
and e_var var = e_sexp (s_var var)
and e_mcall v1 a v2 = 
  e_sexp (s_mcall v1 a v2)
and e_mask ls e = Mask (c_new (), Some ls, e)

and e_get_plab = function
    SExp s -> s_get_plab s
  | Let (plab,_,_,_,_) -> plab
  | Mask (plab,_,_) -> plab
and s_get_plab : s -> plab = function
  | Val v -> v_get_plab v
  | App (plab,_,_) -> plab
  | New (plab,_) -> plab
  | Read p -> p_get_plab p
  | Write (plab,_,_) -> plab
  | MethodCall (plab,_,_) -> plab
and v_get_plab : v -> plab = function
  | Var (plab,_) -> plab
  | Lam (plab, _,_,_) -> plab
  | Undef plab -> plab
  | Pointer (plab,_) -> plab
  | Int (plab, _) -> plab
and p_get_plab : p -> plab = function
    Prop (plab,_,_) -> plab


let e_let x s e = Let (ProgLabel.create_new (),None,x,s,e)
let rec e_lets xel e =
  match xel with
      [] -> e
    | (var,er) :: xeb -> 
        let eb = e_lets xeb e in
          e_let var er eb




let ind = 2
let ind_str i = 
  if i < 1 then 
    "" 
  else 
    String.make (ind * i) ' '
let lset_opt_to_string t1 t2 = function 
    None -> ""
  | Some ls -> t1 ^ (LSet.string_of ls) ^ t2

let rec string_of_e_wp' : int -> e -> string = fun i -> function
  | SExp s -> string_of_s_wp' i s
  | Let (l,lso,v,s,e) -> 
      let ip1 = match e with 
          Let _ -> 0
        | _ -> 1
      in
        ind_str i ^ 
          "let_"^(string_of_plab l) ^ " " 
        ^ (lset_opt_to_string "[" "]" lso) ^ v
        ^ " = " ^ (string_of_s_wp' 0 s) ^ " in\n"
        ^ (string_of_e_wp' ip1 e)
  | Mask (l,lso,e) ->
      begin
        match lso with
          | Some ls -> 
              "M_" ^  (string_of_plab l) 
              ^"[" ^ (LSet.string_of ls) ^ "](" ^ (string_of_e_wp' 0 e) ^ ")"
          | None -> 
              "M_" ^  (string_of_plab l) ^ "(" ^ (string_of_e_wp' 0 e) ^ ")"
      end
and string_of_p_wp' i : p -> string = function
    Prop (l,v,a) -> ind_str i ^ "[" ^ string_of_v_wp' 0 v ^ "." ^ a ^ "]_"
      ^(string_of_plab l) 
and string_of_s_wp' i : s -> string = function
    Val v -> string_of_v_wp' i v
  | App (_,v1,v2) -> 
      string_of_v_wp' i v1 
      ^ "(" ^ string_of_v_wp' i v2 ^ ")" 
  | New (pl,l) -> "new_" ^ Loc.string_of l ^ (string_of_plab pl)
  | Read p -> string_of_p_wp' i p
  | Write (l,p,v2) -> ind_str i ^
      string_of_p_wp' i p ^ " =_" ^ (string_of_plab l) ^ " "
      ^ string_of_v_wp' i v2
  | MethodCall (l,p,v2) -> ind_str i ^
      "[" ^ string_of_p_wp' 0 p ^ "(" ^ string_of_v_wp' 0 v2 ^ ")" 
      ^ "]_" ^ (string_of_plab l)
and string_of_v_wp' i : v -> string = function
    Var (l,x) -> ind_str i ^ x ^ "_" ^ (string_of_plab l)
  | Lam (l,v1,v2,e) -> string_of_lam_wp' i v1 v2 e (Some l)
  | Undef l -> ind_str i ^ "undefined_"^(string_of_plab l)
  | Pointer (_,p) -> ind_str i ^ P.string_of p
  | Int (_,v) -> ind_str i ^ (string_of_int v)
and string_of_lam_wp' : int -> var -> var -> e -> plab option -> string = 
  fun i v1 v2 e -> function
    | None -> ind_str i^"(\\" ^ v1 ^ "," ^ v2 ^ "." ^ string_of_e_wp' i e ^ ")"
    | Some l ->
        ind_str i ^ 
          "(\\" ^ v1 ^ "," ^ v2 ^"." ^ string_of_e_wp' i e 
        ^ ")" ^ "_"^(string_of_plab l)
          
let rec string_of_e' : int -> e -> string = fun i -> function
  | SExp s -> string_of_s' i s
  | Let (_,lso,v,s,e) -> 
      let ip1 = match e with 
          Let _ -> 0
        | _ -> 1
      in
        ind_str i ^ 
          "let "^ (lset_opt_to_string "[" "]" lso) ^ v
        ^ " = " ^ (string_of_s' 0 s) ^ " in\n"
        ^ (string_of_e' ip1 e)
  | Mask (l,lso,e) ->
      begin
        match lso with
          | Some ls -> 
              "M[" ^ (LSet.string_of ls) ^ "](" ^ (string_of_e' 0 e) ^ ")"
          | None -> 
              "M(" ^ (string_of_e' 0 e) ^ ")"
      end
and string_of_p' i : p -> string = function
    Prop (_,v,a) -> string_of_v' i v ^ "." ^ a
and string_of_s' i : s -> string = function
    Val v -> string_of_v' i v
  | App (_,v1,v2) -> 
      string_of_v' i v1 
      ^ "(" ^ string_of_v' 0 v2 ^ ")" 
  | New (_,l) -> "new_" ^ Loc.string_of l 
  | Read p -> string_of_p' i p
  | Write (_,p,v2) -> ind_str i ^
      string_of_p' 0 p ^ " = " ^ string_of_v' 0 v2
  | MethodCall (_,p,v2) -> ind_str i ^
      "[" ^ string_of_p' 0 p ^ "(" ^ string_of_v' 0 v2 ^ ")" 
      ^ "]"
and string_of_v' i : v -> string = function
    Var (_,x) -> ind_str i ^ x 
  | Lam (_,v1,v2,e) -> string_of_lam' i v1 v2 e
  | Undef _ -> ind_str i ^ "undefined"
  | Pointer (_,p) -> ind_str i ^ P.string_of p
  | Int (_,v) -> ind_str i ^ (string_of_int v)
and string_of_lam' : int -> var -> var -> e -> string = fun i v1 v2 e ->
  ind_str i ^ "(\\" ^ v1 ^ "," ^ v2 ^ "." ^ string_of_e' i e ^ ")"


let string_of_var = id
let string_of_label = id
let compare_var = String.compare
let compare_label = String.compare


let string_of_e_with_plabs = string_of_e_wp' 0        
let string_of_s_with_plabs = string_of_s_wp' 0        
let string_of_v_with_plabs = string_of_v_wp' 0        
let string_of_lam_with_plabs v1 v2 e = string_of_lam_wp' 0 v1 v2 e None

let string_of_e_without_plabs = string_of_e' 0        
let string_of_s_without_plabs = string_of_s' 0        
let string_of_v_without_plabs = string_of_v' 0        
let string_of_lam_without_plabs v1 v2 e = string_of_lam' 0 v1 v2 e

let string_of_e e = do_on_pl
  (fun () -> string_of_e_with_plabs e)
  (fun () -> string_of_e_without_plabs e)
let string_of_s s = do_on_pl
  (fun () -> string_of_s_with_plabs s)
  (fun () -> string_of_s_without_plabs s)
let string_of_v v = do_on_pl 
  (fun () -> string_of_v_with_plabs v)
  (fun () -> string_of_v_without_plabs v)
let string_of_lam v1 v2 e = do_on_pl 
  (fun () -> string_of_lam_with_plabs v1 v2 e)
  (fun () -> string_of_lam_without_plabs v1 v2 e)

module VarSet : ExtSSet.S with type elt = var
= ExtSSet.Make(
  struct 
    type t = var 
    let sep = "," 
    let string_of = string_of_var
    let compare = compare_var
  end)

let rec free_s = function
  | Val v -> free_v v
  | App (_,v1,v2) -> VarSet.union (free_v v1) (free_v v2)
  | New _ -> VarSet.empty
  | Read p -> free_p p
  | Write (_,p,v) | MethodCall (_,p,v) ->
      VarSet.union (free_p p) (free_v v)
and free_e = function
  | SExp s -> free_s s
  | Let (_,_,var,s,e) ->
      VarSet.union (free_s s) (VarSet.remove var (free_e e))
  | Mask (_,_,e) -> free_e e
and free_v = function
  | Var (_,var) -> VarSet.singleton var
  | Lam (_,y,x,e) ->
      VarSet.remove y (VarSet.remove x (free_e e))
  | Undef _ | Pointer (_,_) | Int _ -> VarSet.empty
and free_p = function
  | Prop (_,v,_) -> free_v v

let rec collect_s = function
  | Val v -> collect_v v
  | App (_,v1,v2) -> LSet.union (collect_v v1) (collect_v v2)
  | New (_,l) -> LSet.singleton l
  | Read p -> collect_p p
  | Write (_,p,v) | MethodCall (_,p,v) ->
      LSet.union (collect_p p) (collect_v v)
and collect_e = function
  | SExp s -> collect_s s
  | Let (_,_,var,s,e) ->
      LSet.union (collect_s s) (collect_e e)
  | Mask (_,_,e) -> collect_e e
and collect_v = function
  | Var _ | Undef _ | Pointer _ | Int _ -> LSet.empty
  | Lam (_,_,_,e) -> collect_e e
and collect_p = function
  | Prop(_,v,_) -> collect_v v
let collect_locs = collect_e


module Labels = struct
  type t = label
  let compare = compare_label
  let sep = ","
  let string_of = string_of_label
end
module LabelSet = ExtSSet.Make(Labels)
module LabelSetRef = struct
  include ExtSSetImp.Make(Labels)
  let laset t = !t
end
    
module Test = struct
  open ProglangUtils
  open Test

  let t1 () =
    let _ = ProgLabel.reset () in
    let _ = Loc.reset () in
    let e = e_let 
      "x" (s_new ()) 
      (e_var "x") 
    in
    let s_exp = "let_p2 x = new_l0p1 in\n  x_p0" in
    let s = string_of_e_with_plabs e in
      assert_equal 
        ~cmp:(=)
        ~printer:(fun x -> x)
        s_exp
        s

  let t2 () =
    let _ = ProgLabel.reset () in
    let _ = Loc.reset () in
    let e = e_lets
      ["x", New (c_new (), Loc.create ());
       "y", s_write (Var (c_new (),"x")) "f" (v_lam "y" "x" (e_var "x"))]
      (e_mcall (Var (c_new (), "x")) "f" (Var (c_new (),"x")))
    in      
    let s_exp = "\n'let_p11 x = new_l0p9 in\nlet_p10 "
      ^"y = [x_p6.f]_p7 =_p8 (\\y,x.x_p4)_p5 in\n  [[x_p1.f]_p2(x_p0)]_p3'\n" 
    in
    let s = "\n'" ^ string_of_e_with_plabs e ^ "'\n" in
      assert_equal 
        ~cmp:(=)
        ~printer:(fun x -> x)
        s_exp
        s


      
  let _ =
    install_tests "Syntax"
      (fun _ -> [("constructor and string_of test 1", t1);
                 ("constructor and string_of test 2", t2)])

end

module FreeVarTest = struct
  open ProglangUtils
  open Test
  open ExtUtils

  let init_tests () =
    let assert_string s1 s2 =
      assert_equal 
        ~cmp:(=)
        ~printer:(fun x -> x)
        s1
        s2
    in

    let t1 () =
      let _ = ProgLabel.reset () in
      let _ = Loc.reset () in
      let e = e_let 
        "x" (s_new ()) 
        (e_sexp (s_app (v_var "x") (v_var "y")))
      in
      let e_free = free_e e in
        assert_string (string_of_e_without_plabs e) "let x = new_l0 in\n  x(y)";
        assert_equal
          ~cmp:(Utils.compare_to_equal VarSet.compare)
          ~printer:VarSet.string_of
          e_free
          (VarSet.singleton (create_var "y"))
    in
    let t2 () =
      let _ = ProgLabel.reset () in
      let _ = Loc.reset () in
      let e = e_let 
        "x" (s_var "x") 
        (e_sexp (s_app (v_var "x") (v_var "y")))
      in
      let e_free = free_e e in
        assert_string (string_of_e_without_plabs e) "let x = x in\n  x(y)";
        assert_equal
          ~cmp:(Utils.compare_to_equal VarSet.compare)
          ~printer:VarSet.string_of
          e_free
          (VarSet.add (create_var "x") (VarSet.singleton (create_var "y")))
      

    in
      ["free variable test, Let, test 1", t1;
       "free variable test, Letm test 2", t2]

  let _ = install_tests "Syntax.FreeVars" init_tests

end
