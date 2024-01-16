type ident =
    StrId of string
  | NumId of string * int
type kident =
    ident

let fresh_counter =
  ref 0
let fresh prefix =
  begin
    fresh_counter := !fresh_counter + 1;
    NumId (prefix, !fresh_counter)
  end













type exp =
    Var of ident
  | Const of ident
  | Pure of ident * exp list
  | Oper of ident * exp list
  | App of exp * exp
  | Let of ident * exp * exp
  | If of exp * exp * exp
  | Close of exp * exp
  | Vector of exp list
  | Vindex of exp * int
type definition =
    ident * ident list * ident * exp
type scheme = definition list * exp










type trivial =
    TVar of ident
  | TConst of ident
  | TPure of ident * trivial list
  | TClose of ident * ident
  | TIndex of trivial * int

type serious =
    SReturn of ident
  | SApp2 of ident * ident
  | SApp3 of ident * ident * ident
  | SLetTriv of ident * trivial * serious
  | SLetOper of ident * ident * ident list * serious
  | SLetApp2 of ident * ident * ident * serious
  | SLetApp3 of ident * ident * ident * ident * serious
  | SLetVect of ident * ident list * serious
  | SIf of ident * serious * serious
  | SLetCont of kident * ident list * ident * serious * serious
  | SYield of kident * ident





let rename i subst =
  try
    List.assoc i subst
  with x ->
    i

let rec remove y xs =
  match xs with
    [] -> []
  | x::xs -> if x=y then xs else x::remove y xs

let rec union xs ys =
  match xs with
    [] -> ys
  | x::xs ->
      let xs' = union xs ys in
      if List.mem x xs' then xs' else x::xs'

let rec free_trivial tt =
  match tt with
    TVar i ->
      [i]
  | TConst i ->
      []
  | TPure (i, ts) ->
      free_trivials ts
  | TClose (i1, i2) ->
      union [i1] [i2]
  | TIndex (t, _) ->
      free_trivial t
and free_trivials ts =
  match ts with
    [] -> []
  | tt::ts ->
      union (free_trivial tt) (free_trivials ts)

let rec free_serious ss =
  match ss with
    SReturn i ->
      [i]
  | SApp2 (i1, i2) ->
      union [i1] [i2]
  | SApp3 (i1,i2,i3) ->
      union [i1] (union [i2] [i3])
  | SLetTriv (ib, tt, ss) ->
      union (free_trivial tt) (remove ib (free_serious ss))
  | SLetOper (ib, io, is, ss) ->
      union is (remove ib (free_serious ss))
  | SLetApp2 (ib, i1, i2, ss) ->
      union [i1] (union [i2] (remove ib (free_serious ss)))
  | SLetApp3 (ib, i1, i2, i3, ss) ->
      union [i1] (union [i2] (union [i3] (remove ib (free_serious ss))))
  | SLetVect (ib, is, ss) ->
      union is (remove ib (free_serious ss))
  | SIf (i1, ss1, ss2) ->
      union [i1] (union (free_serious ss1) (free_serious ss2))
  | SLetCont (k, is, ib, ss1, ss2) ->
      union is (free_serious ss2)
  | SYield (k, i) ->
      [i]











(* toTrivial e *)
(* toTrivial : ... * exp * (trivial -> serious) -> serious *)
let rec toTrivial subst e c =
  match e with
    Var i ->
      c (TVar (rename i subst))
  | Const p ->
      c (TConst p)
  | Pure (p, es) ->
      toTrivialL subst es
	(fun ts ->
	  c (TPure (p, ts)))
  | Vindex (e, i) ->
      toTrivial subst e
	(fun t ->
	  c (TIndex (t, i)))
  | _ ->
      toSerious subst e
	(fun xx ->
	  c (TVar xx))
and toTrivialL subst es c =
  match es with
    [] -> c []
  | e::es ->
      toTrivial subst e
	(fun t ->
	  toTrivialL subst es
	    (fun ts ->
	      c (t::ts)))

and


















(* toSerious e (fun i -> SReturn i) *)
(* toSerious : ... * exp * (ident -> serious) -> serious *)
    toSerious subst e c =
  match e with
    Var i ->
      c (rename i subst)
  | Const p ->
      let fr = fresh "t" in
      SLetTriv (fr, TConst p, c fr)
  | Pure (p, es) ->
      toTrivialL subst es
	(fun ts ->
	  let fr = fresh "t" in
	  SLetTriv (fr, TPure (p, ts), c fr))
  | Oper (o, es) ->
      let fr = fresh "t" in
      toSeriousL subst es
	(fun is ->
	  SLetOper (fr, o, is, c fr))
  | App (Close (e1, e2), e) ->
      toSerious subst e1
	(fun x1 ->
	  toSerious subst e2
	    (fun x2 ->
	      toSerious subst e
		(fun x3 ->
		  let fr = fresh "t" in
		  SLetApp3 (fr, x1, x2, x3, c fr))))
  | App (f, e) ->
      toSerious subst f
	(fun ff ->
	  toSerious subst e
	    (fun ee ->
	      let fr = fresh "t" in
	      SLetApp2 (fr, ff, ee, c fr)))
  | Let (x, e1, e2) ->
      toSerious subst e1
	(fun xx ->
	  toSerious ((x, xx) :: subst) e2 c)
  | If (e1, e2, e3) ->
      toSerious subst e1
	(fun x1 ->
	  let kk = fresh "k" in
	  let xx = fresh "t" in
	  let body = c xx in 
	  SLetCont
	    (kk, remove xx (free_serious body), xx, body,
	     SIf
	       (x1,
		toSerious subst e2 (fun i -> SYield (kk, i)),
		toSerious subst e3 (fun i -> SYield (kk, i)))))
  | Close (e1, e2) ->
      toSerious subst e1
	(fun x1 ->
	  toSerious subst e2
	    (fun x2 ->
	      let fr = fresh "t" in
	      SLetTriv (fr, TClose (x1, x2), c fr)))
  | Vector (es) ->
      toSeriousL subst es
	(fun xs ->
	  let fr = fresh "t" in
	  SLetVect (fr, xs, c fr))
  | Vindex (e, i) ->
      toSerious subst e
	(fun xx ->
	  let fr = fresh "t" in
	  SLetTriv (fr, TIndex (TVar xx, i), c fr))




and
    toSeriousL subst es c =
  match es with
    [] -> c []
  | (e :: es) ->
      toSerious subst e
	(fun x ->
	  toSeriousL subst es
	    (fun xs ->
	      c (x :: xs)))















let rec toSeriousT subst e =
  let c x = SReturn x in
  match e with
    Var i ->
      c (rename i subst)
  | Const p ->
      let fr = fresh "t" in
      SLetTriv (fr, TConst p, c fr)
  | Pure (p, es) ->
      toTrivialL subst es
	(fun ts ->
	  let fr = fresh "t" in
	  SLetTriv (fr, TPure (p, ts), c fr))
  | Oper (o, es) ->
      let fr = fresh "t" in
      toSeriousL subst es
	(fun is ->
	  SLetOper (fr, o, is, c fr))
  | App (Close (e1, e2), e) ->
      toSerious subst e1
	(fun x1 ->
	  toSerious subst e2
	    (fun x2 ->
	      toSerious subst e 
		(fun x3 ->
		  SApp3 (x1, x2, x3))))
  | App (f, e) ->
      toSerious subst f
	(fun ff ->
	  toSerious subst e
	    (fun ee ->
	      SApp2 (ff, ee)))
  | Let (x, e1, e2) ->
      toSerious subst e1
	(fun xx ->
	  toSeriousT ((x, xx) :: subst) e2)
  | If (e1, e2, e3) ->
      toSerious subst e1
	(fun x1 ->
	  SIf
	    (x1,
	     toSeriousT subst e2,
	     toSeriousT subst e3))
  | Close (e1, e2) ->
      toSerious subst e1
	(fun x1 ->
	  toSerious subst e2
	    (fun x2 ->
	      let fr = fresh "t" in
	      SLetTriv (fr, TClose (x1, x2), c fr)))
  | Vector (es) ->
      toSeriousL subst es
	(fun xs ->
	  let fr = fresh "t" in
	  SLetVect (fr, xs, c fr))
  | Vindex (e, i) ->
      toSerious subst e
	(fun xx ->
	  let fr = fresh "t" in
	  SLetTriv (fr, TIndex (TVar xx, i), c fr))











(* examples *)
let var s = Var (StrId s)
let const s = Const (StrId s)
let pure (s, es) = Pure (StrId s, es)
let oper (s, es) = Oper (StrId s, es)
let app (e1, e2) = App (e1, e2)
let mlet (s,e1,e2) = Let (StrId s, e1, e2)
let mif (e1, e2, e3) = If (e1, e2, e3)
let close (e1, e2) = Close (e1, e2)
let vector es =  Vector es
let vindex (e, i) =  Vindex (e, i)


let ex1 = If (pure ("=", [var"n"; const"0"]),
	      const"1",
	      pure ("*",
		    [ var"n";
		      App (Close (var"fac", Vector []),
			   pure ("-", [var"n"; const"1"]))]))






let rec fast_exp x n =
  if n = 0
  then 1
  else
    let e = fast_exp x (n / 2) in
    e * e * (if n mod 2 = 0 then 1 else x)

let ex2 = If (pure ("=", [var"n"; const"0"]),
	      const"1",
	      mlet ("e", App (App (Close (var"fast_exp", Vector[]),
				   var"x"),
			      pure ("/", [var"n"; const"2"])),
		    pure ("*", [var"e";
			       pure ("*", [var"e";
					  If (pure ("mod", [var"n";const"2"]),
					      const"1",
					      var"x")])])))

let ex3 = If (pure ("=", [var"n"; const"0"]),
	      const"1",
	      App (Close (var"down", Vector[]), pure ("-", [var"n"; const"1"])))
