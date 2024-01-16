open ProglangUtils
open HiddenInts

(* TODO: Maby we need a union find structure here to allow 
   add_alias on locations! 
   Code this only if this is necessary, so first find
   examples that will not work without the add_alias
   function.
*)
module Loc = struct
  include HiddenInts.Make (struct let prefix = "l" end)
  let create = create_new
  let sep = ","
end
module Pointers = struct
  include HiddenInts.Make (struct let prefix = "" end)
  let create = create_new
  let sep = ","
end

module type LSET = ExtSSet.S
  with type elt = Loc.t
 
module LSet = ExtSSet.Make(Loc)
module LSetRef : sig
  include ExtSSetImp.S
  val lset : t -> LSet.t
end
  with type elt = Loc.t 
  = struct
    include ExtSSetImp.Make(Loc)
    let lset t = !t
  end

module P = struct
  type t = 
      Exact of Loc.t * Pointers.t
    | Inexact of Loc.t * Pointers.t
  
  let create () = Loc.create_new (), Pointers.create_new ()
  let create_exact () = 
    let l,p = create () in Exact (l, p)
  let create_inexact () = 
    let l,p = create () in Inexact (l, p)

  let create_string s l p =
        "(" ^ s ^ Loc.string_of l ^ "," ^ Pointers.string_of p ^ ")"
  let string_of = function
      Exact (l,p) -> create_string "@" l p
    | Inexact (l,p) -> create_string "?" l p

  let get_location = function
    | Exact (l,_) | Inexact (l,_) -> l

  let is_precise = function
    | Exact _ -> true
    | _ -> false

  let reset () = 
    Pointers.reset ();
end

let reset_all () =
  Loc.reset ();
  P.reset ()

(* module LVar = HiddenInts.Make(struct let prefix = "d" end) *)


module LSetInEx = LowerUpperNeg.Make(
  struct
    module LS = LSet
    type t = LS.t
    let is_empty = LS.is_empty
    let subset ls1 ls2 = Some (LS.subset ls1 ls2)

    let union = LS.union
    let inter = LS.inter
    let diff = LS.diff

    let normalize x = x
    let string_of = LS.string_of
    let compare = LS.compare
  end
)

module TetsLSetInEx = struct
  open Test
  open ExtUtils
  open LSetInEx

  let init_tests () = 
    let assert_string s1 s2 = 
      assert_equal
        ~cmp:(Utils.compare_to_equal String.compare)
        ~printer:(fun x -> x)
        s1
        s2
    in
(*    let assert_string_n s1 s2 = 
      assert_string ("\n" ^ s1 ^ "\n") ("\n" ^ s2 ^ "\n")
    in *)
    let assert_string_list s =
      List.map
        (fun (fls,nr) -> 
           ((fun () -> 
             let ls = fls () in
             let si = string_of ls in
               assert_string s si),nr))
    in

    let t1 () =
      let _ = reset_all () in
      let l1 = Loc.create_new () in
      let l2 = Loc.create_new () in
      let l3 = Loc.create_new () in
      let l4 = Loc.create_new () in
      let ls1 = create_lower (LSet.from_list [l1;l2]) in
      let ls2 = create_upper (LSet.from_list [l1;l2;l3]) in
      let ls3 = create_neg (LSet.singleton l3) in

      let ls_l = create_lower (LSet.singleton l4) in
      let ls_u = create_upper (LSet.from_list [l1;l2;l3;l4]) in
      let ls_n = create_neg (LSet.singleton l4) in

      let ls_u12 () = merge ls1 ls2 in
      let ls_u21 () = merge ls2 ls1 in
      let ls_u13 () = merge ls1 ls3 in
      let ls_u31 () = merge ls3 ls1 in
      let ls_u23 () = merge ls2 ls3 in
      let ls_u32 () = merge ls3 ls2 in

      let ls_u123 () = merge (ls_u12 ()) ls3 in
      let ls_u132 () = merge (ls_u13 ()) ls2 in
      let ls_u213 () = merge (ls_u21 ()) ls3 in
      let ls_u231 () = merge (ls_u23 ()) ls1 in
      let ls_u312 () = merge (ls_u31 ()) ls2 in
      let ls_u321 () = merge (ls_u32 ()) ls1 in

      let ls_u123' () = merge ls1 (ls_u23 ()) in
      let ls_u132' () = merge ls1 (ls_u32 ()) in
      let ls_u213' () = merge ls2 (ls_u13 ()) in
      let ls_u231' () = merge ls2 (ls_u31 ()) in
      let ls_u312' () = merge ls3 (ls_u12 ()) in
      let ls_u321' () = merge ls3 (ls_u21 ()) in


      let ls_ll () = merge ls1 ls_l in
      let ls_uu () = merge ls2 ls_u in
      let ls_nn () = merge ls3 ls_n in
      let ls_ln_ln () = merge (ls_u13 ()) (merge ls1 ls_n) in
      let ls_lu_ln () = merge (merge ls1 ls_u) (ls_u13 ()) in
      let ls_ln_lu () = merge (ls_u13 ()) (merge ls1 ls_u) in
      let ls_lu_lu () = merge (merge ls1 ls_u) (ls_u12 ()) in

      let tests =
        assert_string_list
          "l0,l1 < l0,l1,l2"
          [ls_u12,"1";ls_u21,"2"] @
        assert_string_list
          "l0,l1 < -l2"
          [ls_u13,"3";ls_u31,"4"] @
        assert_string_list
          "< l0,l1"
          [ls_u23,"5";ls_u32,"6"] @
        assert_string_list
          "l0,l1 < l0,l1"
          [ls_u123,"7";ls_u132,"8";ls_u213,"9";
           ls_u231,"10";ls_u312,"11";ls_u321,"12";
           ls_u123',"13";ls_u132',"14";ls_u213',"15";
           ls_u231',"16";ls_u312',"17";ls_u321',"18";] @
        assert_string_list
          "l0,l1,l3 <"
          [ls_ll,"_ll"] @
        assert_string_list
          "< l0,l1,l2"
          [ls_uu,"_uu"] @
        assert_string_list
          "- l2,l3"
          [ls_nn,"_nn"] @
        assert_string_list
          "l0,l1 < -l2,l3"
          [ls_ln_ln,"_ln_ln"] @
        assert_string_list
          "l0,l1 < l0,l1,l3"
          [ls_lu_ln,"_lu_ln";ls_ln_lu,"_ln_lu"] @
        assert_string_list
          "l0,l1 < l0,l1,l2"
          [ls_lu_lu,"_lu_lu"]

      in
        tests
    in
      (List.map (fun (t,s) -> ("Merge test"^s, t)) (t1 ())) @
        []
    
  let _ =
    install_tests "Location.LSetInEx" init_tests


end
