open ProglangUtils
open Location
open Inf
open Type

module TEnv = HistoryMap.AddS
  (struct type t = Syntax.var let string_of = Syntax.string_of_var end)
  (struct type t = Vars.TVar.t let string_of = Vars.TVar.string_of end)
  (HistoryMap.Make(
     struct
       type t = Syntax.var
       let compare = Syntax.compare_var
     end)(Vars.TVar))
(* module GEnv = OwnMap.AddS *)
(*   (struct type t = Loc.t let string_of = Loc.string_of end) *)
(*   (struct type t = Vars.OTVar.t let string_of = Vars.OTVar.string_of end) *)
(*   (OwnMap.Make(Loc)(Vars.OTVar)) *)


module type GEN = sig
  exception NoLetAnnotation of string

  type result = {
    typevar : Vars.TVar.t;
  }
  type changed = {
    location: Type.phi;
    lenv: Vars.LEVar.t;
    tenv: TEnv.t;
  }
  val create_e : TEnv.t -> Vars.LEVar.t -> Syntax.e 
    -> result * changed
  val create_s : TEnv.t -> Vars.LEVar.t -> Syntax.s 
    -> result * Type.phi * Vars.LEVar.t
  val create_v : TEnv.t -> Vars.LEVar.t -> Syntax.v -> result
end

module Gen = struct
  exception NoLetAnnotation of string

  open ConstBasic
  open Syntax

  type result = { typevar : Vars.TVar.t; }
  type changed = {
    location: Type.phi;
    lenv: Vars.LEVar.t;
    tenv: TEnv.t;
  }

  let new_const bc = Const.add bc; ()
  let new_const' bc = Const.add bc; bc

  let t_tv tv = Type.create_t_tv tv
  let new_tv = Vars.TVar.create
  let new_lv () = Type.create_phi_lsvar (Vars.LSVar.create ())
  let new_r () = { typevar = new_tv () } 
  let re tv = { typevar = tv}
  let new_c lenv tenv = { location = new_lv (); lenv = lenv; tenv = tenv }
  let ch phi lenv tenv = { location = phi; lenv = lenv; tenv = tenv }
  let create_demotation tenv mu =
    TEnv.iter  
      (fun _ tv -> 
         let t = t_tv tv in
           new_const (create_c_demotation_type t mu t))
      tenv

  let rec create_fm_call tenv lenv v1 v2 =
    (* fresh variables *)
    let mu = new_lv () in 
    let mus = Type.create_phi_lsvar 
      (Vars.LEVar.get_domain_var lenv) 
    in 
    let alpha = new_tv () in
    let alpha0 = new_tv () in 
    let lenv1 = Vars.LEVar.create () in
      
    (* constraints *)
      create_demotation tenv mu;
      new_const (create_c_demotation_lenv lenv mu lenv);
      new_const (create_c_disjoint mus mu);
      let { typevar = alpha1 } = create_v tenv lenv v1 in
      let { typevar = alpha2 } = create_v tenv lenv v2 in
        (mu,alpha,alpha0,alpha1,alpha2,lenv1)
          
  and create_e tenv lenv = function
    | SExp s -> 
        let re, mu, lev = create_s tenv lenv s in
          re, ch mu lev tenv
    | Let (plab,l1o,v,s,e) -> begin
        let l1 = 
          match l1o with
            | None -> 
                Type.create_phi_lsvar (Vars.LSVar.create ()) 
            | Some l1_set ->
                Type.create_phi_ls l1_set
        in
               
        (* create new fresh variables *)
        let mu = new_lv () in
        let mus = Type.create_phi_lsvar 
          (Vars.LEVar.get_domain_var lenv)
        in
        let alpha_v = Vars.TVar.create () in
          
        (* create constraints, c5 and c8 with a recursive call *)
          create_demotation tenv l1;
          new_const (create_c_disjoint l1 mus);
          new_const (create_c_demotation_lenv lenv l1 lenv);
          let { typevar = alpha_1 }, my1, lenv1 =
            create_s tenv lenv s 
          in
            new_const (create_c_subset my1 mu);
            new_const (create_c_disjoint l1 my1);
            let tenv' = TEnv.add v alpha_v tenv in
            new_const (create_c_subtype (t_tv alpha_1) (t_tv alpha_v));
              let { typevar = alphae }, 
                { location = mu2; lenv = lenv2; tenv = tenv2' } = 
                create_e tenv' lenv1 e 
              in
              let tenv2 = TEnv.remove v tenv2' in
                new_const (create_c_subset mu2 mu);
                re alphae, ch mu lenv2 tenv2
      end
    | Mask (plab,lseto,e) ->
        (* create new fresh variables *)
        
        let lenv' = Vars.LEVar.create () in
        let mus = Type.create_phi_lsvar
          (Vars.LEVar.get_domain_var lenv')
        in
        let lphi = 
          match lseto with
            | Some lset -> 
                Type.create_phi_ls lset
            | None ->
                Type.create_phi_lsvar (Vars.LSVar.create ()) 
        in
        let _ = (* c1 *)
          match lseto with
            | Some lset -> 
                LSet.iter 
                  (fun l -> new_const 
                     (create_c_flow 
                        lenv 
                        (Type.create_t_obj Type.q_ex 
                           (Type.create_phi_ls (LSet.singleton l)) :> Type.t)
                        (Type.create_t_obj Type.q_in 
                           (Type.create_phi_ls (LSet.singleton l)))))
                  lset
            | None ->
                ()
        in
        let tenv' = (* c2 *)
          TEnv.mapi
            (fun key img ->
               let tv = Vars.TVar.create () in
               let tvt = t_tv tv in
               let t_img = t_tv img in
                 new_const (create_c_demotation_type tvt lphi t_img);
                 tv 
            )
            tenv
        in
        let { typevar = alpha }, 
          { location = mu; lenv = lenv''; tenv = tenv''} = 
          create_e tenv' lenv' e
        in
          new_const (create_c_subset mu lphi); (* c4 *)
          new_const (create_c_demotation_lenv_phi lenv' lphi lphi lenv); (* c5 *)
          new_const (create_c_disjoint lphi mus); (* c7 *)
          re alpha, ch lphi lenv'' tenv''
            

  and create_s tenv lenv = function
    | Val v ->
        let {typevar = tv} = create_v tenv lenv v in
        (* return the constraints and the typevariable. The
           location is the emptyset, the type end local environment
           does not change 
        *)
          re tv, (Type.create_phi_ls LSet.empty), lenv
    | App (plab,v1,v2) ->
        let mu,alpha,alpha0,alpha1,alpha2,lenv1 =
          create_fm_call tenv lenv v1 v2 
        in
          new_const (create_c_subtype Type.t_undef (t_tv alpha0));
          new_const 
            (create_c_eqtype 
               (t_tv alpha1)
               (Type.create_t_fun 
                  lenv 
                  (t_tv alpha0)
                  (t_tv alpha2)
                  mu 
                  lenv1 
                  (t_tv alpha)));
        (* return alpha as typevariable, c as constraint, lenv1 as 
           locale environment and mu as location set. tenv stays unchanged. 
        *)
          re alpha, mu, lenv1
    | MethodCall (plab,Prop (_,v1,a),v2) ->
        let alphaf = Vars.TVar.create () in

        let mu1 = Vars.LSVar.create () in
        let mu1_as_phi = Type.create_phi_lsvar mu1 in
        let xi = Vars.PrVar.create () in
        let xi_as_q = Type.create_q_pv (xi) in

        let mu,alpha,alpha0,alpha1,alpha2,lenv1 =
          create_fm_call tenv lenv v1 v2 
        in

          new_const (create_c_eqtype (t_tv alpha1) (Type.create_t_obj xi_as_q mu1_as_phi));
          new_const (create_c_read lenv xi mu1 a alphaf);
          new_const (
            create_c_eqtype 
              (t_tv alphaf)
              (Type.create_t_fun 
                 lenv 
                 (t_tv alpha0) 
                 (t_tv alpha2) 
                 mu 
                 lenv1 
                 (t_tv alpha0)));
          re alpha0, mu, lenv1

    | New (plab,l) ->
        (* fesh variables *)
        let alpha = Vars.TVar.create () in
        let mus = Type.create_phi_lsvar 
          (Vars.LEVar.get_domain_var lenv)
        in
        let mu = new_lv () in 
        let lenv' = Vars.LEVar.create () in

        let lset = (Type.create_phi_ls (LSet.singleton l)) in
          create_demotation tenv lset;
          new_const (create_c_exclude l mus);
          new_const (create_c_demotation_lenv lenv lset lenv);
          new_const (create_c_include l mu);
          new_const (create_c_lev_empty_obj lenv' lenv l);
          new_const (
            create_c_eqtype 
              (t_tv alpha) 
              (Type.create_t_obj Type.q_ex (Type.create_phi_ls (LSet.singleton l))));
          re alpha, mu, lenv'
    | Read (Prop (_,v,a)) ->
        let alpha = new_tv () in
        let mu = new_lv () in
        let mu1 = Vars.LSVar.create () in
        let mu1_as_phi = Type.create_phi_lsvar mu1 in
        let xi = Vars.PrVar.create () in
        let xi_as_q = Type.create_q_pv (xi) in
        let { typevar = alpha1 } = create_v tenv lenv v in
          new_const (create_c_eqtype (t_tv alpha1) (Type.create_t_obj xi_as_q mu1_as_phi));
          new_const (create_c_read lenv xi mu1 a alpha);
          new_const (create_c_subset mu (Type.create_phi_ls LSet.empty));
          re alpha, mu, lenv
    | Write (_, Prop (_,v1,a), v2) ->
        let alpha = new_tv () in
        let mu = new_lv () in
        let mu1 = Vars.LSVar.create () in
        let mu1_as_phi = Type.create_phi_lsvar mu1 in
        let xi = Vars.PrVar.create () in
        let xi_as_q = Type.create_q_pv  xi in
        let lenv' = Vars.LEVar.create () in

        let { typevar = alpha1 } = create_v tenv lenv v1 in
          new_const (create_c_eqtype (t_tv alpha1) (Type.create_t_obj xi_as_q mu1_as_phi));
          let { typevar = alpha2 } = create_v tenv lenv v2 in
            new_const (create_c_write lenv alpha1 a alpha2 lenv');
            new_const (create_c_eqtype (t_tv alpha) Type.t_undef);
            new_const (create_c_subset mu (Type.create_phi_ls LSet.empty));
            re alpha, mu, lenv'

  and create_v tenv lenv v =
    let eq_tv t =
      let tv = new_tv () in
        new_const (ConstBasic.create_c_eqtype t (t_tv tv));
        { typevar = tv }
    in
        
    let create_v' = function
      | Var (_,var) -> 
          { typevar = 
              if (TEnv.mem var tenv) then
                TEnv.find var tenv
              else 
                failwith ("Variable " ^ (string_of_var var) 
                          ^ " not in type environment. " 
                          ^"Typing not possible.")
          }
      | Pointer (_,p) ->
          let l = Location.P.get_location p in
          let q = if Location.P.is_precise p then Type.q_ex else Type.q_in in
            eq_tv (Type.create_t_obj q (Type.create_phi_ls (LSet.singleton l)))
      | Undef _ -> eq_tv Type.t_undef
      | Int _ -> eq_tv Type.t_int
      | Lam (_,y,x,e) as v ->
          let alpha0,alpha2 = Vars.TVar.create (), Vars.TVar.create () in
          let mu, mu' = new_lv (), new_lv () in
          let lenv2 = Vars.LEVar.create () in
          let mus = Type.create_phi_lsvar 
            (Vars.LEVar.get_domain_var lenv2)
          in
            new_const (create_c_disjoint mu mus);
            new_const (create_c_subset mu' mu);
            let kl = VarSet.elements (Syntax.free_v v) in
            let tenv' = TEnv.restrict kl tenv in
            let c4 = 
              (TEnv.iter
                 (fun _ t -> 
                    new_const (create_c_locs mu' (t_tv t))) 
               tenv'
            )
          in
          let tenv'' = 
            TEnv.fold
              (fun key img tenv'' -> 
                 let tv = Vars.TVar.create () in
                 let tenv'' = TEnv.add key tv tenv'' in
                 let tenv''x = t_tv tv in
                 let tenv'x = Type.create_t_tv img in
                   new_const (create_c_demotation_type tenv''x mu' tenv'x);
                   new_const (create_c_flow lenv2 tenv'x tenv''x);
                   tenv''
              )
              tenv'
              TEnv.empty
          in
          let { typevar = alpha1 }, 
            { location = mu''; lenv = lenv1; tenv = tenv'''} 
            =
            create_e
              (TEnv.add y alpha0 (TEnv.add x alpha2 tenv''))
              lenv2 
              e 
          in
            new_const (create_c_subset mu'' mu);
            eq_tv 
              (Type.create_t_fun lenv2 (t_tv alpha0) (t_tv alpha2) mu 
                 lenv1 (t_tv alpha1))
    in
    let { typevar = tv } = create_v' v in
    (* Subsumation *)
    let alpha = new_tv () in
      new_const (create_c_subtype (t_tv tv) (t_tv alpha));
      { typevar = alpha }

end

module TestUtils = struct
  let wrap_reset f x =
    Location.reset_all ();
    Vars.reset ();
    Const.reset ();
    let r = f x in
      Location.reset_all ();
      Vars.reset ();
      Const.reset ();
      r

  let wrap_list = 
    List.map
      (fun (s,f) -> (s,wrap_reset f))

end


module TestGen = struct
  open ExtString
  open ExtList
  open Vars
  open Type
  open Test
  open TestUtils
  open Gen
  open ExtUtils

  let init_tests () = 
    let assert_string s1 s2 = 
      assert_equal
        ~cmp:(Utils.compare_to_equal String.compare)
        ~printer:(fun x -> x)
        s1
        s2
    in
    let assert_string_n s1 s2 = 
      assert_string ("\n" ^ s1 ^ "\n") ("\n" ^ s2 ^ "\n")
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
      let tenv = TEnv.empty in
      let lev = LEVar.create () in
      let v = Syntax.v_undef () in
      let { typevar = tv } = create_v tenv lev v in
        assert_const "undefined = tv_0; tv_0 <: tv_1" (Const.string_of ());
        assert_const "tv_1" (TVar.string_of tv);
        let var = Syntax.create_var "var" in
        let v2 = Syntax.v_var var in
        let tv2 = TVar.create () in
        let tenv = TEnv.add var tv2 tenv in
        let _ = Const.reset () in
        let {typevar = tv2' } = create_v tenv lev v2 in
          assert_const "tv_2 <: tv_3" (Const.string_of ());
          assert_const "tv_3" (TVar.string_of tv2');
          
    in
    let t2 () =
      let tenv = TEnv.empty in
      let lev = LEVar.create () in
      let s_new = Syntax.s_new () in
      let { typevar = tv }, phi, lev' = 
        create_s tenv lev s_new 
      in
        assert_const 
          ("le_0 :=#{l0} le_0; l0 IN mu_1; l0 EX mu_0; "
           ^ "tv_0 = obj(@{l0}); le_1 = le_0[l0 -> {}]")
          (Const.string_of ());
        assert_string "tv_0" (TVar.string_of tv);
        assert_string "mu_1" (string_of_phi phi);
        assert_string "le_1" (LEVar.string_of lev');
    in
    let t3 () =
      let tenv = TEnv.empty in
      let lev = LEVar.create () in
      let s_undef = Syntax.s_val (Syntax.v_undef ()) in
      let { typevar = tv }, phi, lev' = 
        create_s tenv lev s_undef 
      in
        assert_const "undefined = tv_0; tv_0 <: tv_1" (Const.string_of ());
        assert_string "tv_1" (TVar.string_of tv);
        assert_string "{}" (string_of_phi phi);
        assert_string "le_0" (LEVar.string_of lev');
    in

    let t4 () =
      let tenv = TEnv.empty in
      let lev = LEVar.create () in
      let s_app = Syntax.s_app (Syntax.v_undef ()) (Syntax.v_undef ()) in
      let { typevar = tv }, phi, lev' = create_s tenv lev s_app in
        assert_const 
          ("le_0 :=#mu_0 le_0; mu_1 d mu_0; undefined <: tv_1;"
           ^" undefined = tv_2; tv_2 <: tv_3;"
           ^" undefined = tv_4; tv_4 <: tv_5;"
           ^" tv_3 = (le_0,tv_1 x tv_5)-mu_0->(le_1,tv_0)") 
          (Const.string_of ());
        assert_string "tv_0" (TVar.string_of tv);
        assert_string "mu_0" (string_of_phi phi);
        assert_string "le_1" (LEVar.string_of lev');
    in
    let t5 () =
      let tenv = TEnv.empty in
      let lev = LEVar.create () in
      let v = Syntax.v_undef () in
      let s_read = Syntax.s_read v (Syntax.create_label "read") in
      let { typevar = tv }, phi, lev' = create_s tenv lev s_read in
        assert_const
          ("mu_0 s {}; undefined = tv_1; tv_1 <: tv_2; "
           ^"tv_2 = obj(pv_0mu_1); le_0 |-r pv_0 mu_1.read : tv_0")
          (Const.string_of ());
        assert_string "tv_0" (TVar.string_of tv);
        assert_string "mu_0" (string_of_phi phi);
        assert_string "le_0" (LEVar.string_of lev');
    in
    let t6 () =
      let tenv = TEnv.empty in
      let lev = LEVar.create () in
      let v = Syntax.v_undef () in
      let s_write = Syntax.s_write 
        v 
        (Syntax.create_label "w") 
        (Syntax.v_undef ()) 
      in
      let { typevar = tv }, phi, lev' = create_s tenv lev s_write in
        assert_const
          ("mu_0 s {}; undefined = tv_1; undefined = tv_3; "
           ^"tv_0 = undefined; tv_2 = obj(pv_0mu_1); "
           ^"le_0 |-w tv_2.w = tv_4 => le_1; "
           ^"tv_1 <: tv_2; tv_3 <: tv_4")
          (Const.string_of ());
        assert_string "tv_0" (TVar.string_of tv);
        assert_string "mu_0" (string_of_phi phi);
        assert_string "le_1" (LEVar.string_of lev');
    in

    let t7 () =
      let tenv = TEnv.empty in
      let lev = LEVar.create () in
      let s_app = Syntax.s_mcall 
        (Syntax.v_undef ()) 
        (Syntax.create_label "m") 
        (Syntax.v_undef ()) 
      in
      let { typevar = tv }, phi, lev' = create_s tenv lev s_app in
        assert_const
          ("le_0 :=#mu_1 le_0; mu_2 d mu_1; undefined = tv_3; tv_3 <: tv_4; "
           ^"undefined = tv_5; tv_5 <: tv_6; tv_0 = (le_0,tv_2 x tv_6)-mu_1->(le_1,tv_2); "
           ^"tv_4 = obj(pv_0mu_0); "
           ^"le_0 |-r pv_0 mu_0.m : tv_0") 
          (Const.string_of ());
        assert_string "tv_2" (TVar.string_of tv);
        assert_string "mu_1" (string_of_phi phi);
        assert_string "le_1" (LEVar.string_of lev');
    in

    let t8 () =
      let e_undef = Syntax.e_sexp (Syntax.s_val (Syntax.v_undef ())) in
      let tenv = TEnv.empty in
      let lev = LEVar.create () in
      let { typevar = tv }, {location = phi; lenv = lev'; tenv = tev' } = 
        create_e tenv lev e_undef 
      in
        assert_const
          ("undefined = tv_0; tv_0 <: tv_1") 
          (Const.string_of ());
        assert_string "tv_1" (TVar.string_of tv);
        assert_string "{}" (string_of_phi phi);
        assert_string "le_0" (LEVar.string_of lev');
        assert_string "" (TEnv.string_of tev');
    in

    let t9 () =
      let e_undef = Syntax.e_sexp (Syntax.s_val (Syntax.v_undef ())) in
      let tenv = TEnv.empty in
      let lev = LEVar.create () in
      let vlam = Syntax.v_lam 
        (Syntax.create_var "this") 
        (Syntax.create_var "x")
        e_undef
      in
      let { typevar = tv } = create_v tenv lev vlam in
        assert_const
          ("{} s mu_1; mu_0 s mu_1; mu_1 d mu_2; "
           ^"undefined = tv_2; tv_2 <: tv_3; "
           ^"(le_1,tv_1 x tv_0)-mu_1->(le_1,tv_3) = tv_4; tv_4 <: tv_5") 
          (Const.string_of ());
        assert_string "tv_5" (TVar.string_of tv);
    in

    let create_let_ex () =
      let s_undef = Syntax.s_val (Syntax.v_undef ()) in
      let e_let = Syntax.e_let 
        (Syntax.create_var "x") 
        s_undef 
        (Syntax.e_sexp (Syntax.s_val (Syntax.v_var (Syntax.create_var "x"))))
      in
      let tenv = TEnv.empty in
      let lev = LEVar.create () in
      let _ = Const.reset () in
      let { typevar = tv }, {location = phi; lenv = lev'; tenv = tev' } = 
        create_e tenv lev e_let
      in
        tv,phi,lev',tev'
    in

    let t10 () =
      let tv,phi,lev',tev' = create_let_ex () in
        assert_const
          ("le_0 :=#mu_0 le_0; {} s mu_1; mu_0 d {}; mu_0 d mu_2; "
           ^"undefined = tv_1; tv_1 <: tv_2; tv_0 <: tv_3; tv_2 <: tv_0")
          (Const.string_of ());
        assert_string "tv_3" (TVar.string_of tv);
        assert_string "mu_1" (string_of_phi phi);
        assert_string "le_0" (LEVar.string_of lev');
        assert_string "" (TEnv.string_of tev');
        ()

    in
      wrap_list 
        [("gen test for basic values, undef, variable", t1);
         ("gen test for basic s_exp, new", t2);
         ("gen test for basic s_exp, value", t3);
         ("gen test for basic s_exp, app", t4);
         ("gen test for basic s_exp, read", t5);
         ("gen test for basic s_exp, write", t6);
         ("gen test for basic s_exp, mcall", t7);
         ("gen test for basic e_sexp", t8);
         ("gen test for basic v_lam, "
          ^"does not test TEnv folds, because TEnv is empty", t9);       
         ("gen test for basic e_let", t10);
        ]

  let _ =
    install_tests "Gen" (wrap_reset init_tests)
    
end
