open Driver
open Inf
open OUnit
open ProglangUtils
open ExtUtils
open Vars
open Location
open ExtString
open ExtList
open Test

include TestUtils.Make(
  struct 
    let reset () = 
      Location.reset_all ();
      Vars.reset ();
      Const.reset ();
      GEnv.reset ();
  end)

module Tests = struct
  open Gen

    let init_tests () = 
      let varss () = 
        let tvarss = List.map 
          Vars.TVar.string_of_with_img
          (Vars.TVar.get_all_ts ())
        in
        let pvars = List.map
          Vars.PrVar.string_of_with_img
          (Vars.PrVar.get_all_ts ())
        in
        let lsvars = List.map
          Vars.LSVar.string_of_with_img
          (Vars.LSVar.get_all_ts ())
        in
        let levs = List.map
          Vars.LEVar.string_of_with_img
          (Vars.LEVar.get_all_ts ())
        in
        let otvs = List.map
          Vars.OTVar.string_of_with_img
          (Vars.OTVar.get_all_ts ())
        in
          pvars @ tvarss @ lsvars @ levs @ otvs
      in
        
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
        
      let create_new_lset anz = 
        let ll = List.map 
          (Utils.apply' ())
          (Utils.replicate anz Loc.create) 
        in
          ll
      in
      let assert_const c1 c2 =
        let c1l = String.nsplit c1 "; " in
        let c2l = String.nsplit c2 "; " in
        let cmp = List.compare_ignore_order 
          ~equal:(Utils.compare_to_equal String.compare)
        in
          assert_equal
            ~cmp:cmp
            ~printer:(fun sl -> String.concat "; " 
                        (List.map (fun s -> "'"^s^"'") sl))
            c1l
            c2l
      in

      let t1 () =
        let s = "let x = new in let y = x in let z = y.a := 5 in x.a" in
        let l = Lexing.from_string s in
        let {Gen.typevar = tv},cs = solve (fun p -> p.r, p.sc) l in
          assert_string
            "tv_9:int <"
            (TVar.string_of_with_img tv);
          assert_string
            ("le_1 = le_0[l0 -> {}]; {} s mu_5; mu_3 s mu_1; mu_5 s mu_1; mu_8 s mu_5; "
             ^"mu_9 s {}; mu_9 s mu_8; mu_11 s {}; mu_11 s mu_8; int = tv_7; "
             ^"tv_1 = obj(@{l0}); tv_5 = undefined; tv_6 = obj(pv_0mu_10); "
             ^"tv_10 = obj(pv_1mu_12); le_1 |-w tv_6.a = tv_8 => le_2; "
             ^"le_0 :=#{l0} le_0; le_0 :=#mu_0 le_0; le_1 :=#mu_4 le_1; "
             ^"le_1 :=#mu_7 le_1; tv_0 :=#mu_4 tv_0; tv_0 :=#mu_7 tv_0; "
             ^"tv_2 :=#mu_7 tv_2; tv_0 <: tv_3; tv_0 <: tv_10; tv_1 <: tv_0; "
             ^"tv_2 <: tv_6; tv_3 <: tv_2; tv_5 <: tv_4; tv_7 <: tv_8; "
             ^"l0 EX mu_2; mu_0 d mu_2; mu_0 d mu_3; mu_4 d {}; mu_4 d mu_6; "
             ^"mu_7 d mu_6; mu_7 d mu_9; l0 IN mu_3; "
             ^"le_2 |-r pv_1 mu_12.a : tv_9")
            cs


      in
      let t2 () =
        let s = "let x = new in let y = x in let z = y.a := 5 in let w = x.a := undefined in y.a" in
          assert_string
            ""
            (String.concat "; " (varss ()));
        let l = Lexing.from_string s in
        let {Gen.typevar = tv},cs = solve (fun p -> p.r, p.sc) l in
          assert_string
            ("pv_3:= @; pv_2:= @; pv_1:= @; pv_0:= @; "
             ^"tv_15:= obj(@{l0}); tv_14:undefined <; tv_13:undefined <; "
             ^"tv_12:= undefined; tv_11:= obj(@{l0}); tv_10:= undefined; "
             ^"tv_9:undefined <; tv_8:int <; tv_7:= int; tv_6:= obj(@{l0}); "
             ^"tv_5:= undefined; tv_4:undefined <; "
             ^"tv_3:= obj(@{l0}); tv_2:= obj(@{l0}); tv_1:= obj(@{l0}); "
             ^"tv_0:= obj(@{l0}); "
             ^"mu_19:l0 < l0; mu_18:l0 < l0; mu_17:l0 < l0; mu_16:< ; "
             ^"mu_15:l0 < l0; mu_14:< ; mu_13:l0 < l0; mu_12:< l0; mu_11:< ; "
             ^"mu_10:l0 < l0; mu_9:< ; mu_8:< l0; mu_7:< ; mu_6:l0 < l0; "
             ^"mu_5: < l0; mu_4:< ; mu_3:l0 < l0; mu_2:< ; mu_1:l0 < l0; "
             ^"mu_0:< ; le_3: l0 : ov_2 (mu_19:l0 < l0); "
             ^"le_2: l0 : ov_1 (mu_13:l0 < l0); "
             ^"le_1: l0 : ov_0 (mu_6:l0 < l0); "
             ^"le_0:  (mu_2:< ); ov_2: a : tv_13; ov_1: a : tv_8; ov_0: ")
            (String.concat "; " (varss ()));
          assert_string
            "tv_14:undefined <"
            (TVar.string_of_with_img tv)

      in
      let t3 () =
        let s = 
          ( "M_[l0] let x = new_l0 in let f = x.a := 42 in "
           ^"M_[l0] let y = new_l0 in let f = y.a := undefined in "
           ^"M_[l0] let z = new_l0 in let f = z.a := 20 in x.a")
        in
        let l = Lexing.from_string s in
        let s = parse (fun e s -> s) l in
          assert_string
            ("M[l0](let x = new_l0 in\n"
             ^"let f = x.a = 42 in\n"
             ^"M[l0](let y = new_l0 in\n"
             ^"let f = y.a = undefined in\n"
             ^"M[l0](let z = new_l0 in\n"
             ^"let f = z.a = 20 in\n"
             ^"  x.a)))")
            s
      in

      let t4 () =
(*         let s = "M_[l0] let x = new_l0 in let f = x.a := 42 in x.a" in *)
        let s = "M_[l0] let x = new_l0 in undef" in
        let l = Lexing.from_string s in
        let sc, {Gen.typevar = tv } = solve (fun p -> p.sc, p.r) l in
          assert_string
            ("le_2 = le_1[l0 -> {}]; le_0, obj(@{l0}) <| obj(~{l0}); "
             ^"{} s mu_2; mu_2 s {l0}; mu_3 s mu_2; "
             ^"undefined = tv_2; tv_1 = obj(@{l0}); "
             ^"le_1 :=#{l0} le_1; le_1 :=#{l0}{l0} le_0; le_1 :=#mu_1 le_1; "
             ^"tv_1 <: tv_0; tv_2 <: tv_3; l0 EX mu_0; {l0} d mu_0; "
             ^"mu_1 d mu_0; mu_1 d mu_3; l0 IN mu_3")
            sc;
          assert_string
            "tv_3:undefined <"
            (TVar.string_of_with_img tv);
          assert_string
            ("tv_3:undefined <; tv_2:= undefined; tv_1:= obj(@{l0}); "
             ^"tv_0:obj(@{l0}) <; mu_5:< l0; "
             ^"mu_4:l0 < l0; mu_3:l0 < l0; mu_2:l0 < l0; mu_1:< ; "
             ^"mu_0:< ; le_2: l0 : ov_0 (mu_4:l0 < l0); "
             ^"le_1:  (mu_0:< ); le_0:  (mu_5:< l0); ov_0: ")
            (String.concat "; " (varss ()));
          ()
      in
      let t4_a () =
        let s = "M_[l0] let x = new_l0 in let f = x.a := 42 in x.a" in
        let l = Lexing.from_string s in
        let sc, {Gen.typevar = tv } = solve (fun p -> p.sc, p.r) l in
          assert_string
            ("le_2 = le_1[l0 -> {}]; le_0, obj(@{l0}) <| obj(~{l0}); "
             ^"mu_2 s {l0}; mu_3 s mu_2; mu_5 s mu_2; mu_7 s {}; mu_7 s mu_5; "
             ^"mu_9 s {}; mu_9 s mu_5; int = tv_5; tv_1 = obj(@{l0}); "
             ^"tv_3 = undefined; tv_4 = obj(pv_0mu_8); tv_8 = obj(pv_1mu_10); "
             ^"le_2 |-w tv_4.a = tv_6 => le_3; le_1 :=#{l0} le_1; "
             ^"le_1 :=#{l0}{l0} le_0; le_1 :=#mu_1 le_1; le_2 :=#mu_4 le_2; "
             ^"tv_0 :=#mu_4 tv_0; tv_0 <: tv_4; tv_0 <: tv_8; tv_1 <: tv_0; "
             ^"tv_3 <: tv_2; tv_5 <: tv_6; l0 EX mu_0; {l0} d mu_0; "
             ^"mu_1 d mu_0; mu_1 d mu_3; mu_4 d mu_6; mu_4 d mu_7; l0 IN mu_3; "
             ^"le_3 |-r pv_1 mu_10.a : tv_7")
            sc;
          assert_string
            "tv_7:int <"
            (TVar.string_of_with_img tv);
          assert_string
            ("pv_2:= @; pv_1:= @; pv_0:= @; "
             ^"tv_8:= obj(@{l0}); tv_7:int <; tv_6:int <; tv_5:= int; "
             ^"tv_4:= obj(@{l0}); tv_3:= undefined; tv_2:undefined <; "
             ^"tv_1:= obj(@{l0}); tv_0:= obj(@{l0}); "
             ^"mu_13:l0 < l0; mu_12:< l0; "
             ^"mu_11:l0 < l0; mu_10:l0 < l0; mu_9:< ; "
             ^"mu_8:l0 < l0; mu_7:< ; mu_6:l0 < l0; mu_5:< l0; mu_4:< ; "
             ^"mu_3:l0 < l0; mu_2:l0 < l0; mu_1:< ; mu_0:< ; "
             ^"le_3: l0 : ov_1 (mu_13:l0 < l0); "
             ^"le_2: l0 : ov_0 (mu_6:l0 < l0); le_1:  (mu_0:< ); "
             ^"le_0:  (mu_12:< l0); ov_1: a : tv_6; ov_0: ")
            (String.concat "; " (varss ()));
          ()
      in
          
        
      let t4_b () =
        let s = "let x = new_l0 in let f = x.a := 42 in x.a" in
        let l = Lexing.from_string s in
        let sc, {Gen.typevar = tv } = solve (fun p -> p.sc, p.r) l in
          assert_string
            "tv_7:int <"
            (TVar.string_of_with_img tv);
          assert_string
            ("le_1 = le_0[l0 -> {}]; pv_2 <: pv_0; pv_2 <: pv_1; "
             ^"mu_3 s mu_1; mu_5 s mu_1; mu_7 s mu_5; mu_9 s mu_5; "
             ^"mu_11 s mu_8; mu_11 s mu_10; le_1 |-w tv_4.a = tv_6 => le_2; "
             ^"le_0 :=#{l0} le_0; le_0 :=#mu_0 le_0; le_1 :=#mu_4 le_1; "
             ^"ov_0 :=#mu_4 ov_0; tv_0 :=#mu_4 tv_0; tv_0 <: tv_4; tv_0 <: tv_8; "
             ^"tv_1 <: tv_0; tv_3 <: tv_2; tv_5 <: tv_6; tv_6 <: tv_7; mu_0 d mu_2; "
             ^"mu_0 d mu_3; mu_4 d mu_6; mu_4 d mu_7; "
             ^"le_2 |-r pv_1 mu_10.a : tv_7")
            (Const.string_of ());
          assert_string
             ("le_1 = le_0[l0 -> {}]; mu_3 s mu_1; mu_5 s mu_1; mu_7 s {}; "
              ^"mu_7 s mu_5; mu_9 s {}; mu_9 s mu_5; int = tv_5; "
              ^"tv_1 = obj(@{l0}); tv_3 = undefined; tv_4 = obj(pv_0mu_8); "
              ^"tv_8 = obj(pv_1mu_10); le_1 |-w tv_4.a = tv_6 => le_2; "
              ^"le_0 :=#{l0} le_0; le_0 :=#mu_0 le_0; le_1 :=#mu_4 le_1; "
              ^"tv_0 :=#mu_4 tv_0; tv_0 <: tv_4; tv_0 <: tv_8; tv_1 <: tv_0; "
              ^"tv_3 <: tv_2; tv_5 <: tv_6; l0 EX mu_2; mu_0 d mu_2; "
              ^"mu_0 d mu_3; mu_4 d mu_6; mu_4 d mu_7; l0 IN mu_3; "
              ^"le_2 |-r pv_1 mu_10.a : tv_7")
            sc;
          assert_string
            ("pv_2:= @; pv_1:= @; pv_0:= @; tv_8:= obj(@{l0}); "
             ^"tv_7:int <; tv_6:int <; tv_5:= int; tv_4:= obj(@{l0}); "
             ^"tv_3:= undefined; tv_2:undefined <; tv_1:= obj(@{l0}); "
             ^"tv_0:= obj(@{l0}); "
             ^"mu_12:l0 < l0; mu_11:l0 < l0; mu_10:l0 < l0; "
             ^"mu_9:< ; mu_8:l0 < l0; mu_7:< ; mu_6:l0 < l0; mu_5:< l0; "
             ^"mu_4:< ; mu_3:l0 < l0; mu_2:< ; mu_1:l0 < l0; mu_0:< ; "
             ^"le_2: l0 : ov_1 (mu_12:l0 < l0); "
             ^"le_1: l0 : ov_0 (mu_6:l0 < l0); le_0:  (mu_2:< ); "
             ^"ov_1: a : tv_6; ov_0: ")
            (String.concat "; " (varss ()));
          ()
      in

      let t5 () =
        let s = 
          ( "M_[l0] let x = new_l0 in let f = x.a := 42 in "
           ^"M_[l0] let y = new_l0 in let f = y.a := undefined in "
           ^"M_[l0] let z = new_l0 in let f = z.a := 20 in z.a")
        in
        let l = Lexing.from_string s in
        let sc, {Gen.typevar = tv } = solve (fun p -> p.sc, p.r) l in
          assert_string
            "tv_26:int <"
            (TVar.string_of_with_img tv);
(*           assert_string *)
(*             "" *)
(*             (String.concat "; " (varss ())); *)
(*           assert_string *)
(*             "" *)
(*             sc; *)
(*           assert_string *)
(*             "" *)
(*             (Const.string_of ()); *)
          ()          
      in
      let t6 () =
        let s = 
          ( "M_[l0] let x = new_l0 in let f = x.a := 42 in "
           ^"M_[l0] let y = new_l0 in let f = y.a := undefined in "
           ^"M_[l0] let z = new_l0 in let f = z.a := 20 in x.a")
        in
        let l = Lexing.from_string s in
        let sc,{Gen.typevar = tv } = solve (fun p -> p.sc,p.r) l in
(*           assert_string  *)
(*             ""  *)
(*             (String.concat "; " (varss ())); *)
(*           assert_string  *)
(*             ""  *)
(*             sc;  *)
(*           assert_string  *)
(*             ""  *)
(*             (Const.string_of ()); *)
          assert_string
            "l0->ov_6: a : tv_26"
            (GEnv.string_of ());
          assert_string
            "tv_26:T <"
            (TVar.string_of_with_img tv);
          ()
          
      in

      let t7 () = 
        let s = 
          ("M_[l0] let x = new_l0 in let z = x.a := 42 in "
           ^"let f = \\(u1,u2).x.a in "
           ^"M_[l0] let y = new_l0 in "
           ^"M_[l0] let z = f (0) in z")
        in
        let l = Lexing.from_string s in
        let s = parse (fun e s -> s) l in
          assert_string 
            ("M[l0](let x = new_l0 in\n"
             ^"let z = x.a = 42 in\n"
             ^"let f = (\u1,u2.x.a) in\n"
             ^"M[l0](let y = new_l0 in\n"
             ^"M[l0](let z = f(0) in\n"
             ^"  z)))")
            s;
          ()
      in

      let t8 () = 
        let s = 
          ("M_[l0] let x = new_l0 in let z = x.a := 42 in "
           ^"let f = \\(u1,u2).x.a in f(0)")
        in
        let l = Lexing.from_string s in
        let sc, {Gen.typevar = tv } = solve (fun p -> p.sc, p.r) l in
          assert_string
            "false"
            (Const.string_of ());
          ()
      in
      let t8_a () = 
        let s = 
          ("M_[l0] let x = new_l0 in let z = x.a := 42 in "
           ^"let f = \\(u1,u2).x.a in M_[l0] f(0)")
        in
        let l = Lexing.from_string s in
        let sc, {Gen.typevar = tv } = solve (fun p -> p.sc, p.r) l in
          assert_string
            "tv_18[32]:int <"
            (TVar.string_of_with_img tv);
(*           assert_string *)
(*             "" *)
(*             (String.concat "; " (varss ())); *)
(*           assert_string *)
(*             "" *)
(*             sc; *)
(*           assert_string *)
(*             "true" *)
(*             (Const.string_of ()); *)
(*           assert_string *)
(*             "l0->ov_2: a : tv_26" *)
(*             (GEnv.string_of ()); *)
          ()

      in
        wrap_list
          ["complete solve test, alias", t1;
           "complete solve test, alias, second", t2;
           "parse test, mask and new with abstract labels", t3;
           "complete solve test, mask and new with abstract labels", t4;
           "complete solve test, mask and new with abstract labels, a", t4_a;
           "complete solve test, mask and new with abstract labels, b", t4_b;
           "complete solve test, mask and new with abstract labels, c", t5;
           "complete solve test, mask and new with abstract labels, d", t6;
           "parse test, lambda test", t7;
           "complete solve test, lambda test, reject", t8;
           "complete solve test, lambda test, success", t8_a;
          ]



  let _ =
    install_tests "GTest" (wrap_reset init_tests)

end
