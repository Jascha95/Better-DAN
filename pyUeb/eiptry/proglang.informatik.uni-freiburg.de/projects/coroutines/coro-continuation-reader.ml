(* fresh-identifier utilities *)

let fresh_counter =
  ref 0
let fresh_reset () =
  fresh_counter := 0
let step_counter () =
  begin
    fresh_counter := !fresh_counter + 1;
    !fresh_counter
  end
let fresh_variable stub =
  let n = step_counter () in
    stub ^ "$" ^ string_of_int n
let fresh_location () =
  step_counter ()


type ident = string
type location = int

type location_or_ident =
  | LIIdent of ident
  | LILoc of location

type expr =
  | Loc of location
  | Var of ident
  | Lam of ident * expr
  | App of expr * expr 
  | Assign of ident * expr
  | Cond of expr * expr * expr
  | Equals of expr * expr
  | Nil

      (* asymmetric coroutines *)
  | Labeled of location * expr
  | Create of expr
  | Resume of expr * expr
  | Yield of expr

      (* symmetric coroutines *)
  | Transfer of expr * expr

(* example expressions *)
let ex1 = Lam ("x", Var "x")
let ex2 = App (ex1, Nil)
let ex3 = App (ex1, ex2)

(*examples with coroutines*)
let ex4 = Lam ("f", App (Lam ("l", Lam ("x", Resume (Var "l", Var"x"))),
			 Create (Var "f")))
let ex5 = Cond (App (App (ex4, Lam ("x", Equals (Yield (Var "x"), Yield Nil))),
		     Create Nil),
		Lam ("z", Lam ("w", Var "z")),
		Lam ("z", Lam ("w", Var "w")))
let ex6 = App (Lam ("e",
		    App (Lam ("f",
			      App (Lam ("g",
					App (Lam ("h",
						  Cond (Resume (Var "h", Var "e"),
							Loc (-1),
							Loc (-2))),
					     Resume (Var"g", Var"e"))),
				   Resume (Var "f", Var"f"))),
			 Create (Lam ("self", Equals (Yield (Var "self"), Yield (Var "self")))))),
	       Create (Lam ("z", Var "z")))


let rename e i j =
  let rec ren e =
    match e with
    | Loc l -> Loc l
    | Var k -> if i=k then Var j else Var k
    | Lam (k, e) -> if i=k then Lam (k, e) else Lam (k, ren e)
    | App (e1, e2) -> App (ren e1, ren e2)
    | Assign (k, e) -> if i=k then Assign (j, ren e) else Assign (k, ren e)
    | Cond (e1, e2, e3) -> Cond (ren e1, ren e2, ren e3)
    | Equals (e1, e2) -> Equals (ren e1, ren e2)
    | Nil -> Nil
	  
    | Create (e) -> Create (ren e)
    | Resume (e1, e2) -> Resume (ren e1, ren e2)
    | Yield (e) -> Yield (ren e)
    | Labeled (l, e) -> Labeled (l, ren e)

    | Transfer (e1, e2) -> Transfer (ren e1, ren e2)

  in ren e


(*the store*)
type 'value store =
    (location_or_ident * 'value) list
let empty = []
let lookup s x = List.assoc x s
let update s x v = (x,v) :: List.remove_assoc x s

(*in meta continuation passing style, replaced state monad by reader monad for current_coroutine*)
type cvalue =
  | CVLoc of location
  | CVLam of ident * expr
  | CVNil

  | CVCont of ccont
and ccont =
    (cvalue * cvalue store -> mcont -> cvalue)
and mcont =
    (cvalue * cvalue store -> cvalue)

let rec evalrf2_expr (e, theta, lc) mc ec =
  match e with
  | Loc l ->
      ec (CVLoc l, theta) mc
  | Var i ->
      ec (lookup theta (LIIdent i), theta) mc
  | Lam (i, e) ->
      ec (CVLam (i, e), theta) mc
  | App (e1, e2) ->
      evalrf2_expr (e2, theta, lc) mc 
	(fun (v2, theta) mc ->
	   evalrf2_expr (e1, theta, lc) mc
	     (fun (v, theta) mc ->
		(match v with
		   | CVLam (i, e) ->
		       let newx = fresh_variable i in
		       evalrf2_expr
			 (rename e i newx, update theta (LIIdent newx) v2, lc)
			 mc ec)))
  | Assign (i, e) ->
      evalrf2_expr (e, theta, lc) mc 
	(fun (v, theta) mc ->
	   ec (v, update theta (LIIdent i) v) mc)
  | Cond (e1, e2, e3) ->
      evalrf2_expr (e1, theta, lc) mc
	(fun (v, theta) mc ->
	   (match v with
	      |	CVNil -> evalrf2_expr (e3, theta, lc) mc ec
	      |	_ -> evalrf2_expr (e2, theta, lc) mc ec))
  | Equals (e1, e2) ->
      evalrf2_expr (e2, theta, lc) mc 
	(fun (v2, theta) mc ->
	   evalrf2_expr (e1, theta, lc) mc
	     (fun (v1, theta) mc ->
	        let CVLoc l1 = v1 in
		let CVLoc l2 = v2 in
		  ec ((if l1 = l2 then CVLoc l1 else CVNil), theta)  mc))
  | Nil ->
      ec (CVNil, theta) mc
	
  | Create e ->
      evalrf2_expr (e, theta, lc) mc
	(fun (v, theta) mc ->
	  let newl = fresh_location () in
	  ec (CVLoc newl, update theta (LILoc newl) v) mc)
  | Resume (e1, e2) ->
      evalrf2_expr (e2, theta, lc) mc
	(fun (v2, theta) mc ->
	   evalrf2_expr (e1, theta, lc) mc
	     (fun (v1, theta) mc ->
		(match v1 with
		   | CVLoc l ->
		       let vc = lookup theta (LILoc l) in
		       let theta = update theta (LILoc l) CVNil in
		       let mc = fun (v, theta) ->
			 ec (v, theta) mc
		       in
		       (match vc with
		       | CVLam (i, e) ->
			   let newx = fresh_variable i in
			   evalrf2_expr
			     (rename e i newx, update theta (LIIdent newx) v2, l)
			     mc (fun (v, theta) mc -> mc (v, theta))
		       | CVCont ecinner ->
			   ecinner (v2, theta) mc))))
  | Yield e ->
      evalrf2_expr (e, theta, lc) mc 
	(fun (v, theta) mc ->
	  let theta = update theta (LILoc lc) (CVCont ec) in
	  mc (v, theta))

  | Transfer (e1, e2) ->
      evalrf2_expr (e2, theta, lc) mc
	(fun (v2, theta) mc ->
	   evalrf2_expr (e1, theta, lc) mc
	     (fun (v1, theta) mc ->
		(match v1 with
		| CVLoc l0 ->
		    let vc = lookup theta (LILoc l0) in
		    let theta = update theta (LILoc l0) CVNil in
		    let theta = update theta (LILoc lc) (CVCont ec) in
		    (match vc with
		    | CVLam (i, e) ->
			let newx = fresh_variable i in
			evalrf2_expr
			  (rename e i newx, update theta (LIIdent newx) v2, l0)
			  mc (fun (v, theta) mc -> mc (v, theta))
		    | CVCont ecinner ->
			ecinner (v2, theta) mc))))      

let run_evalrf2 e =
  evalrf2_expr (e, empty, 0)
    (fun (v, theta) -> v)
    (fun (v, theta) mc -> mc (v, theta))


(* back to direct style for the meta continuation *)
type cvalue1 =
  | MVLoc of location
  | MVLam of ident * expr
  | MVNil

  | MVCont of ccont1
and ccont1 =
    (cvalue1 * cvalue1 store -> cvalue1 * cvalue1 store)

let rec evalmf2_expr (e, theta, lc) ec =
  match e with
  | Loc l ->
      ec (MVLoc l, theta)
  | Var i ->
      ec (lookup theta (LIIdent i), theta)
  | Lam (i, e) ->
      ec (MVLam (i, e), theta)
  | App (e1, e2) ->
      evalmf2_expr (e2, theta, lc) 
	(fun (v2, theta) ->
	   evalmf2_expr (e1, theta, lc)
	     (fun (v, theta) ->
		(match v with
		   | MVLam (i, e) ->
		       let newx = fresh_variable i in
		       evalmf2_expr
			 (rename e i newx, update theta (LIIdent newx) v2, lc)
			 ec)))
  | Assign (i, e) ->
      evalmf2_expr (e, theta, lc) 
	(fun (v, theta) ->
	   ec (v, update theta (LIIdent i) v))
  | Cond (e1, e2, e3) ->
      evalmf2_expr (e1, theta, lc)
	(fun (v, theta) ->
	   (match v with
	      |	MVNil -> evalmf2_expr (e3, theta, lc) ec
	      |	_ -> evalmf2_expr (e2, theta, lc) ec))
  | Equals (e1, e2) ->
      evalmf2_expr (e2, theta, lc) 
	(fun (v2, theta) ->
	   evalmf2_expr (e1, theta, lc)
	     (fun (v1, theta) ->
	        let MVLoc l1 = v1 in
		let MVLoc l2 = v2 in
		  ec ((if l1 = l2 then MVLoc l1 else MVNil), theta) ))
  | Nil ->
      ec (MVNil, theta)
	
  | Create e ->
      evalmf2_expr (e, theta, lc)
	(fun (vc, theta) ->
	  let newl = fresh_location () in
	  let v = MVCont
	    (match vc with
	       | MVLam (i, e) ->
		   (fun (v2, theta) ->
		      let newx = fresh_variable i in
			evalmf2_expr
			 (rename e i newx, update theta (LIIdent newx) v2, newl)
			  (fun (v, theta) -> (v, theta))))
	  in
	    ec (MVLoc newl, update theta (LILoc newl) v))
  | Resume (e1, e2) ->
      evalmf2_expr (e2, theta, lc)
	(fun (v2, theta) ->
	   evalmf2_expr (e1, theta, lc)
	     (fun (v1, theta) ->
		(match v1 with
		   | MVLoc l ->
		       let vc = lookup theta (LILoc l) in
		       let theta = update theta (LILoc l) MVNil in
		       (match vc with
		       | MVCont ecinner ->
			   ec (ecinner (v2, theta))))))
  | Yield e ->
      evalmf2_expr (e, theta, lc)
	(fun (v, theta) ->
	  let theta = update theta (LILoc lc) (MVCont ec) in
	    (v, theta))

  | Transfer (e1, e2) ->
      evalmf2_expr (e2, theta, lc)
	(fun (v2, theta) ->
	   evalmf2_expr (e1, theta, lc)
	     (fun (v1, theta) ->
		(match v1 with
		| MVLoc l0 ->
		    let vc = lookup theta (LILoc l0) in
		    let theta = update theta (LILoc l0) MVNil in
		    let theta = update theta (LILoc lc) (MVCont ec) in
		    (match vc with
		    | MVCont ecinner ->
			ecinner (v2, theta)))))      

let run_evalmf2 e =
  fresh_reset ();
  evalmf2_expr (e, empty, 0)
    (fun (v, theta) -> (v, theta))

