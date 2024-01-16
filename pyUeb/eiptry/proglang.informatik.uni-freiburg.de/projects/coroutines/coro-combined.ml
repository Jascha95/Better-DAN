(*2nd attempt: asymmetric coroutines*)
let fresh_counter =
  ref 0
let reset_counter () =
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

  | Cont of evaluation_context_item list

and evaluation_context_item =
  | ECApplyR of expr
  | ECApplyL of value
  | ECAssign of ident
  | ECCond of expr * expr
  | ECEqualsR of expr
  | ECEqualsL of value

  | ECCreate
  | ECResumeR of expr
  | ECResumeL of value
  | ECYield

  | ECTransferR of expr
  | ECTransferL of value

and value =
  | VLoc of location
  | VLam of ident * expr
  | VNil

  | VCont of evaluation_context_item list

type meta_evaluation_context_item =
  | MCLabeled of evaluation_context_item list * location

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
	       Create Nil)


let v2e v =
  match v with
    | VLoc l -> Loc l
    | VLam (i, e) -> Lam (i,e)
    | VNil -> Nil

    | VCont ec -> Cont ec

(*
type store = location_or_ident -> value

let empty =
  function y -> raise Not_found
let lookup s x =
  s x
let update s x v =
  function y -> if x=y then v else s x
*)
type 'value store =
    (location_or_ident * 'value) list
let empty = []
let lookup s x = List.assoc x s
let update s x v = (x,v) :: List.remove_assoc x s


type potential_redex =
  | PRIdent of ident
  | PRBeta of value * value
  | PRAssign of ident * value
  | PRCond of value * expr * expr
  | PREquals of value * value

  | PRCreate of value
  | PRResume of value * value
  | PRYield of evaluation_context_item list * value
  | PRLabeled of location * value

  | PRTransfer of evaluation_context_item list * value * value

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

    | Cont ec -> Cont ec
    (* we don't have to rename inside a continuation because it is formed
       from frames of an evaluation context so that all free variables are
       already substituted for by the previous evaluation *)
  in ren e

let rec wrap_expr_ec ec e =
  match ec with
    | [] ->  e
    | (ECApplyR e1 :: rest) -> wrap_expr_ec rest (App (e1, e))
    | (ECApplyL v2 :: rest) -> wrap_expr_ec rest (App (e, v2e v2))
    | (ECAssign i :: rest) -> wrap_expr_ec rest (Assign (i, e))
    | (ECCond (e2, e3) :: rest) -> wrap_expr_ec rest (Cond (e, e2, e3))
    | (ECEqualsR e1 :: rest) -> wrap_expr_ec rest (Equals (e1, e))
    | (ECEqualsL v2 :: rest) -> wrap_expr_ec rest (Equals (e, v2e v2))

    | (ECCreate :: rest) -> wrap_expr_ec rest (Create e)
    | (ECResumeR e1 :: rest) -> wrap_expr_ec rest (Resume (e1, e))
    | (ECResumeL v2 :: rest) -> wrap_expr_ec rest (Resume (e, v2e v2))
    | (ECYield :: rest) -> wrap_expr_ec rest (Yield e)

    | (ECTransferR e1 :: rest) -> wrap_expr_ec rest (Transfer (e1, e))
    | (ECTransferL v2 :: rest) -> wrap_expr_ec rest (Transfer (e, v2e v2))

let rec wrap_expr_mc mc ec e =
  let e1 = wrap_expr_ec ec e in
  match mc with
  | [] ->
      e1
  | (MCLabeled (ec, l) :: mc) ->
      wrap_expr_mc mc ec (Labeled (l, e1))


let contract (pr, theta, l) =
  match pr with
  | PRIdent ident ->
      (v2e (lookup theta (LIIdent ident)), theta, l)
  | PRBeta (VLam (i, e), v2) ->
      let newx = fresh_variable i in
      (rename e i newx, update theta (LIIdent newx) v2, l)
  | PRBeta (VCont ec, v2) ->
      (*preferably we'd like to append ec to the enclosing evaluation context*)
      (wrap_expr_ec ec (v2e v2), theta, l)
  | PRAssign (i, v) ->
      (v2e v, update theta (LIIdent i) v, l)
  | PRCond (VNil, e2, e3) ->
      (e3, theta, l)
  | PRCond (v, e2, e3) ->
      (e2, theta, l)
  | PREquals (VLoc l1, VLoc l2) ->
      (if l1 = l2 then Loc l1 else Nil), theta, l
	
  | PRCreate v ->
      let newl = fresh_location () in
      (Loc newl, update theta (LILoc newl) v, l)
  | PRResume (VLoc l1, v) ->
      (Labeled (l, App (v2e (lookup theta (LILoc l1)), v2e v)),
       update theta (LILoc l1) VNil, l1)
  | PRLabeled (l1, v) ->
      (v2e v, theta, l1)
  | PRYield (ec, v) ->
      (v2e v, update theta (LILoc l) (VCont ec), l)

  | PRTransfer (ec, VLoc l1, v) ->
      (App (v2e (lookup theta (LILoc l1)), v2e v),
       update (update theta (LILoc l1) VNil) (LILoc l) (VCont ec),l1)
	
type decomposition =
  | Value of value
  | Decomposition of
      meta_evaluation_context_item list *
	evaluation_context_item list *
	potential_redex

let rec dec_expr e ec mc =
  match e with
  | Loc l -> dec_context ec (VLoc l) mc
  | Var i -> Decomposition (mc, ec, PRIdent i)
  | Lam (i, e) -> dec_context ec (VLam (i, e)) mc
  | App (e1, e2) -> dec_expr e2 (ECApplyR e1 :: ec) mc
  | Assign (i, e) -> dec_expr e (ECAssign i :: ec) mc
  | Cond (e1, e2, e3) -> dec_expr e1 (ECCond (e2, e3) :: ec) mc
  | Equals (e1, e2) -> dec_expr e2 (ECEqualsR e1 :: ec) mc
  | Nil -> dec_context ec (VNil) mc
	
  | Create e -> dec_expr e (ECCreate :: ec) mc
  | Resume (e1, e2) -> dec_expr e2 (ECResumeR e1 :: ec) mc
  | Yield e -> dec_expr e (ECYield :: ec) mc
  | Labeled (l, e) -> dec_expr e [] (MCLabeled (ec, l) :: mc)

  | Transfer (e1, e2) -> dec_expr e2 (ECTransferR e1 :: ec) mc
	
  | Cont ec1 -> dec_context ec (VCont ec1) mc

and dec_context ec v mc =
  match ec with
  | [] ->  dec_outer_context v mc
  | (ECApplyR e1 :: rest) -> dec_expr e1 (ECApplyL v :: rest) mc
  | (ECApplyL v2 :: rest) -> Decomposition (mc, rest, PRBeta (v, v2))
  | (ECAssign i :: rest) -> Decomposition (mc, rest, PRAssign (i, v))
  | (ECCond (e2, e3) :: rest) -> Decomposition (mc, rest, PRCond (v, e2, e3))
  | (ECEqualsR e1 :: rest) -> dec_expr e1 (ECEqualsL v :: rest) mc
  | (ECEqualsL v2 :: rest) -> Decomposition (mc, rest, PREquals (v, v2))
	
  | (ECCreate :: rest) -> Decomposition (mc, rest, PRCreate v)
  | (ECResumeR e1 :: rest) -> dec_expr e1 (ECResumeL v :: rest) mc
  | (ECResumeL v2 :: rest) -> Decomposition (mc, rest, PRResume (v, v2))
  | (ECYield :: rest) -> Decomposition (mc, [], PRYield (rest, v))

  | (ECTransferR e1 :: rest) -> dec_expr e1 (ECTransferL v :: rest) mc
  | (ECTransferL v2 :: rest) -> Decomposition (mc, [], PRTransfer (rest, v, v2))
	
and dec_outer_context v mc =
  match mc with
  | [] -> Value v
  | (MCLabeled (ec, l) :: rest) -> Decomposition (rest, ec, PRLabeled (l, v))

(* the one-step reduction function *)

let reduce (e, theta, l) =
  match dec_expr e [] [] with
    | Value v -> (v2e v, theta, l)
    | Decomposition (mc, ec, pr) ->
	let (contractum, theta, l) = contract (pr, theta, l) in
	  (wrap_expr_mc mc ec contractum, theta, l)

let nreduce n e =
  reset_counter ();
  let rec red n c =
    if n=0 then c else red (n-1) (reduce c)
  in red n (e, empty, 0)

(********************************************************************)
(* small-step abstract machine by refocusing *)

type config =
  | CResult of value * value store * location
  | CDecomposition of
      meta_evaluation_context_item list *
	evaluation_context_item list * expr * value store * location

let step conf =
  match conf with
  | CResult (v, theta, l) -> CResult (v, theta, l)
  | CDecomposition (mc, ec, e, theta, l) ->
      match dec_expr e ec mc with
      |	Value v -> CResult (v, theta, l)
      |	Decomposition (mc, ec, pr) ->
	  let (contractum, theta, l) = contract (pr, theta, l) in
	  CDecomposition (mc, ec, contractum, theta, l)

let nstep n e =
  let rec stp n c =
    if n=0 then c else stp (n-1) (step c)
  in stp n (CDecomposition ([], [], e, empty, 0))

let fixstep e =
  let rec run c =
    match c with
    | CResult (v, theta, l) -> c
    | CDecomposition (mc, ec, e, theta, l) -> run (step c)
  in run (CDecomposition ([], [], e, empty, 0))

let fixstep_step_inlined e = (*and simplified*)
  let rec run (mc, ec, e, theta, l) =
    match dec_expr e ec mc with
    | Value v -> (v, theta, l)
    | Decomposition (mc, ec, pr) ->
	let (contractum, theta, l) = contract (pr, theta, l) in
	run (mc, ec, contractum, theta, l)
  in run ([], [], e, empty, 0)

let fixstep_contract_inlined e =
  let rec run (mc, ec, e, theta, l) =
    match dec_expr e ec mc with
    | Value v -> (v, theta, l)
    | Decomposition (mc, ec, pr) ->
	  match pr with
	  | PRIdent ident ->
	      run (mc, ec, v2e (lookup theta (LIIdent ident)), theta, l)
	  | PRBeta (VLam (i, e), v2) ->
	      let newx = fresh_variable i in
	      run (mc, ec, rename e i newx, update theta (LIIdent newx) v2, l)
	  | PRBeta (VCont ecinner, v2) ->
      (*preferably we'd like to append ec to the enclosing evaluation context*)
	      run (mc, ec, wrap_expr_ec ecinner (v2e v2), theta, l)
	  | PRAssign (i, v) ->
	      run (mc, ec, v2e v, update theta (LIIdent i) v, l)
	  | PRCond (VNil, e2, e3) ->
	      run (mc, ec, e3, theta, l)
	  | PRCond (v, e2, e3) ->
	      run (mc, ec, e2, theta, l)
	  | PREquals (VLoc l1, VLoc l2) ->
	      run (mc, ec, (if l1 = l2 then Loc l1 else Nil), theta, l)
		
	  | PRCreate v ->
	      let newl = fresh_location () in
	      run (mc, ec, Loc newl, update theta (LILoc newl) v, l)
	  | PRResume (VLoc l0, v) ->
	      run (mc, ec,
	       Labeled (l, App (v2e (lookup theta (LILoc l0)), v2e v)),
	       update theta (LILoc l0) VNil, l0)
	  | PRLabeled (l0, v) ->
	      run (mc, ec, v2e v, theta, l)
	  | PRYield (ecinner, v) ->
	      run (mc, ec, v2e v, update theta (LILoc l) (VCont ecinner), l)

	  | PRTransfer (ecinner, VLoc l0, v) ->
	      run (mc, ec, App (v2e (lookup theta (LILoc l0)), v2e v),
		   update (update theta (LILoc l0) VNil) (LILoc l) (VCont ecinner), l0)
  in reset_counter ();
  run ([], [], e, empty, 0)

(* 1. append ecinner (does not change the complexity, it's still linear)
   2. compress corridor transitions for PRResume
  *)
let fixstep_contract_optimized e =
  let rec run (mc, ec, e, theta, lc) =
    match dec_expr e ec mc with
    | Value v -> (v, theta, lc)
    | Decomposition (mc, ec, pr) ->
	  match pr with
	  | PRIdent ident ->
	      run (mc, ec, v2e (lookup theta (LIIdent ident)), theta, lc)
	  | PRBeta (VLam (i, e), v2) ->
	      let newx = fresh_variable i in
	      run (mc, ec, rename e i newx, update theta (LIIdent newx) v2, lc)
	  | PRBeta (VCont ecinner, v2) ->
	      run (mc, ecinner @ ec, v2e v2, theta, lc)
	  | PRAssign (i, v) ->
	      run (mc, ec, v2e v, update theta (LIIdent i) v, lc)
	  | PRCond (VNil, e2, e3) ->
	      run (mc, ec, e3, theta, lc)
	  | PRCond (v, e2, e3) ->
	      run (mc, ec, e2, theta, lc)
	  | PREquals (VLoc l1, VLoc l2) ->
	      run (mc, ec, (if l1 = l2 then Loc l1 else Nil), theta, lc)
		
	  | PRCreate v ->
	      let newl = fresh_location () in
	      run (mc, ec, Loc newl, update theta (LILoc newl) v, lc)
	  | PRResume (VLoc l0, v2) ->
	      let vc = lookup theta (LILoc l0) in
	      let theta = update theta (LILoc l0) VNil in
	      (match vc with
	      |	VLam (i, e) ->
		  let newx = fresh_variable i in
		  run (MCLabeled (ec, lc) :: mc, [],
		       rename e i newx, update theta (LIIdent newx) v2, l0)
	      |	VCont ecinner ->
		  run (MCLabeled (ec, lc) :: mc, ecinner, v2e v2, theta, l0))
	  | PRLabeled (l, v) ->
	      run (mc, ec, v2e v, theta, l)
	  | PRYield (ecinner, v) ->
	      run (mc, ec, v2e v, update theta (LILoc lc) (VCont ecinner), lc)

	  | PRTransfer (ecinner, VLoc l0, v2) ->
	      let vc = lookup theta (LILoc l0) in
	      let theta = update theta (LILoc l0) VNil in
	      let theta = update theta (LILoc lc) (VCont ecinner) in
	      (match vc with
	      |	VLam (i, e) ->
		  let newx = fresh_variable i in
		  run (mc, [],
		       rename e i newx, update theta (LIIdent newx) v2, l0)
	      |	VCont ecinner ->
		  run (mc, ecinner, v2e v2, theta, l0))
  in run ([], [], e, empty, 0)

(********************************************************************)
(* inlining of dec_expr
   transition compression (eval expr applied to known value -> eval_context) *)

let rec eval_expr (mc, ec, e, theta, lc) =
  match e with
  | Loc l -> eval_context (VLoc l, theta, lc) ec mc
  | Var i -> eval_context (lookup theta (LIIdent i), theta, lc) ec mc
  | Lam (i, e) -> eval_context (VLam (i, e), theta, lc) ec mc
  | App (e1, e2) -> eval_expr (mc, ECApplyR e1 :: ec, e2, theta, lc)
  | Assign (i, e) -> eval_expr (mc, ECAssign i :: ec, e, theta, lc)
  | Cond (e1, e2, e3) -> eval_expr (mc, ECCond (e2, e3) :: ec, e1, theta, lc)
  | Equals (e1, e2) -> eval_expr (mc, ECEqualsR e1 :: ec, e2, theta, lc)
  | Nil -> eval_context (VNil, theta, lc) ec mc
	
  | Create e -> eval_expr (mc, ECCreate :: ec, e, theta, lc)
  | Resume (e1, e2) -> eval_expr (mc, ECResumeR e1 :: ec, e2, theta, lc)
  | Yield e -> eval_expr (mc, ECYield :: ec, e, theta, lc)

  | Transfer (e1, e2) -> eval_expr (mc, ECTransferR e1 :: ec, e2, theta, lc)
(*
  | Labeled (l, e) -> eval_expr (MCLabeled (ec, l) :: mc, [], e, theta, l)
*)
  | Cont ec1 -> eval_context (VCont ec1, theta, lc) ec mc

and eval_context (v, theta, lc) ec mc =
  match ec with
  | [] ->
      eval_outer_context mc (v, theta, lc)
  | (ECApplyR e1 :: rest) ->
      eval_expr (mc, ECApplyL v :: rest, e1, theta, lc)
  | (ECApplyL v2 :: rest) ->
      (match v with
      |	VLam (i, e) ->
	  let newx = fresh_variable i in
	  eval_expr (mc, rest, rename e i newx, update theta (LIIdent newx) v2, lc))
  | (ECAssign i :: rest) ->
      eval_context (v, update theta (LIIdent i) v, lc) rest mc
  | (ECCond (e2, e3) :: rest) ->
      (match v with
      |	VNil -> eval_expr (mc, rest, e3, theta, lc)
      |	_ -> eval_expr (mc, rest, e2, theta, lc))
  | (ECEqualsR e1 :: rest) ->
      eval_expr (mc, ECEqualsL v :: rest, e1, theta, lc)
  | (ECEqualsL v2 :: rest) ->
      let VLoc l1 = v in
      let VLoc l2 = v2 in
      eval_expr (mc, rest, (if l1 = l2 then Loc l1 else Nil), theta, lc)
	
  | (ECCreate :: rest) -> 
      let newl = fresh_location () in
      eval_expr (mc, rest, Loc newl, update theta (LILoc newl) v, lc)
  | (ECResumeR e1 :: rest) ->
      eval_expr (mc, ECResumeL v :: rest, e1, theta, lc)
  | (ECResumeL v2 :: rest) ->
      (match v with
      |	VLoc l0 ->
	  let vc = lookup theta (LILoc l0) in
	  let theta = update theta (LILoc l0) VNil in
	  (match vc with
	  | VLam (i, e) ->
	      let newx = fresh_variable i in
	      eval_expr (MCLabeled (rest, lc) :: mc, [],
			 rename e i newx, update theta (LIIdent newx) v2, l0)
	  | VCont ecinner ->
	      eval_context (v2, theta, l0) ecinner (MCLabeled (rest, lc) :: mc)))
  | (ECYield :: rest) ->
      let theta = update theta (LILoc lc) (VCont rest) in
      eval_outer_context mc (v, theta, lc)

  | (ECTransferR e1 :: rest) ->
      eval_expr (mc, ECTransferL v :: rest, e1, theta, lc)
  | (ECTransferL v2 :: rest) ->
      (match v with
      |	VLoc l0 ->
	  let vc = lookup theta (LILoc l0) in
	  let theta = update theta (LILoc l0) VNil in
	  let theta = update theta (LILoc lc) (VCont rest) in
	  (match vc with
	  | VLam (i, e) ->
	      let newx = fresh_variable i in
	      eval_expr (mc, [],
			 rename e i newx, update theta (LIIdent newx) v2, l0)
	  | VCont ecinner ->
	      eval_context (v2, theta, l0) ecinner mc))
	
and eval_outer_context mc (v, theta, lc) =
  match mc with
  | [] -> v			(* error if called from yield? *)
  | (MCLabeled (ec, l) :: mc) ->
      eval_context (v, theta, l) ec mc

let run_eval e =
  eval_expr ([], [], e, empty, 0)

(********************************************************************)
(* move meta continuation to last argument position *)
(* refunctionalize the meta continuation *)

let rec evalrf_expr (ec, e, theta, lc) mc  =
  match e with
  | Loc l -> evalrf_context (VLoc l, theta, lc) ec mc
  | Var i -> evalrf_context (lookup theta (LIIdent i), theta, lc) ec mc
  | Lam (i, e) -> evalrf_context (VLam (i, e), theta, lc) ec mc
  | App (e1, e2) -> evalrf_expr (ECApplyR e1 :: ec, e2, theta, lc) mc
  | Assign (i, e) -> evalrf_expr (ECAssign i :: ec, e, theta, lc) mc 
  | Cond (e1, e2, e3) -> evalrf_expr (ECCond (e2, e3) :: ec, e1, theta, lc) mc
  | Equals (e1, e2) -> evalrf_expr (ECEqualsR e1 :: ec, e2, theta, lc) mc
  | Nil -> evalrf_context (VNil, theta, lc) ec mc
	
  | Create e -> evalrf_expr (ECCreate :: ec, e, theta, lc) mc
  | Resume (e1, e2) -> evalrf_expr (ECResumeR e1 :: ec, e2, theta, lc) mc
  | Yield e -> evalrf_expr (ECYield :: ec, e, theta, lc) mc

  | Transfer (e1, e2) -> evalrf_expr (ECTransferR e1 :: ec, e2, theta, lc) mc

  | Cont ec1 -> evalrf_context (VCont ec1, theta, lc) ec mc

and evalrf_context (v, theta, lc) ec mc =
  match ec with
  | [] ->
      mc (v, theta, lc)
  | (ECApplyR e1 :: rest) ->
      evalrf_expr (ECApplyL v :: rest, e1, theta, lc) mc
  | (ECApplyL v2 :: rest) ->
      (match v with
      |	VLam (i, e) ->
	  let newx = fresh_variable i in
	  evalrf_expr (rest, rename e i newx, update theta (LIIdent newx) v2, lc) mc)
  | (ECAssign i :: rest) ->
      evalrf_context (v, update theta (LIIdent i) v, lc) rest mc
  | (ECCond (e2, e3) :: rest) ->
      (match v with
      |	VNil -> evalrf_expr (rest, e3, theta, lc) mc
      |	_ -> evalrf_expr (rest, e2, theta, lc) mc)
  | (ECEqualsR e1 :: rest) ->
      evalrf_expr (ECEqualsL v :: rest, e1, theta, lc) mc
  | (ECEqualsL v2 :: rest) ->
      let VLoc l1 = v in
      let VLoc l2 = v2 in
      evalrf_context ((if l1 = l2 then VLoc l1 else VNil), theta, lc) rest mc
	
  | (ECCreate :: rest) -> 
      let newl = fresh_location () in
      evalrf_context (VLoc newl, update theta (LILoc newl) v, lc) rest mc
  | (ECResumeR e1 :: rest) ->
      evalrf_expr (ECResumeL v :: rest, e1, theta, lc) mc
  | (ECResumeL v2 :: ec) ->
      (match v with
      |	VLoc l ->
	  let vc = lookup theta (LILoc l) in
	  let theta = update theta (LILoc l) VNil in
	  let mc = fun (v, theta, l) ->
	    evalrf_context (v, theta, lc) ec mc
	  in
	  (match vc with
	  | VLam (i, e) ->
	      let newx = fresh_variable i in
	      evalrf_expr
		([], rename e i newx, update theta (LIIdent newx) v2, l)
		mc
	  | VCont ecinner ->
	      evalrf_context (v2, theta, l) ecinner mc))
  | (ECYield :: ec) ->
      let theta = update theta (LILoc lc) (VCont ec) in
      mc (v, theta, lc)

  | (ECTransferR e1 :: rest) ->
      evalrf_expr (ECTransferL v :: rest, e1, theta, lc) mc
  | (ECTransferL v2 :: rest) ->
      (match v with
      |	VLoc l0 ->
	  let vc = lookup theta (LILoc l0) in
	  let theta = update theta (LILoc l0) VNil in
	  let theta = update theta (LILoc lc) (VCont rest) in
	  (match vc with
	  | VLam (i, e) ->
	      let newx = fresh_variable i in
	      evalrf_expr ([], rename e i newx, update theta (LIIdent newx) v2, l0) mc
	  | VCont ecinner ->
	      evalrf_context (v2, theta, l0) ecinner mc))

let run_evalrf e =
  evalrf_expr ([], e, empty, 0) (fun (v, theta, lc) -> v)

(********************************************************************)
(* move continuation to next to last argument position *)
(* refunctionalize the continuation *)
(* needs a change in the underlying datatype *)
(* the expressions Labeled(l, e) and Cont(ec) are no longer needed *)
(* refunctionalized the option datatype *)

type cvalue =
  | CVLoc of location
  | CVLam of ident * expr
  | CVNil

  | CVCont of ccont
and ccont =
    (cvalue * cvalue store * location -> mcont -> cvalue)
and mcont =
    (cvalue * cvalue store * location -> cvalue)

let rec evalrf2_expr (e, theta, lc) mc ec =
  match e with
  | Loc l ->
      ec (CVLoc l, theta, lc) mc
  | Var i ->
      ec (lookup theta (LIIdent i), theta, lc) mc
  | Lam (i, e) ->
      ec (CVLam (i, e), theta, lc) mc
  | App (e1, e2) ->
      evalrf2_expr (e2, theta, lc) mc 
	(fun (v2, theta, lc) mc ->
	   evalrf2_expr (e1, theta, lc) mc
	     (fun (v, theta, lc) mc ->
		(match v with
		   | CVLam (i, e) ->
		       let newx = fresh_variable i in
		       evalrf2_expr
			 (rename e i newx, update theta (LIIdent newx) v2, lc)
			 mc ec)))
  | Assign (i, e) ->
      evalrf2_expr (e, theta, lc) mc 
	(fun (v, theta, lc) mc ->
	   ec (v, update theta (LIIdent i) v, lc) mc)
  | Cond (e1, e2, e3) ->
      evalrf2_expr (e1, theta, lc) mc
	(fun (v, theta, lc) mc ->
	   (match v with
	      |	CVNil -> evalrf2_expr (e3, theta, lc) mc ec
	      |	_ -> evalrf2_expr (e2, theta, lc) mc ec))
  | Equals (e1, e2) ->
      evalrf2_expr (e2, theta, lc) mc 
	(fun (v2, theta, lc) mc ->
	   evalrf2_expr (e1, theta, lc) mc
	     (fun (v1, theta, lc) mc ->
	        let CVLoc l1 = v1 in
		let CVLoc l2 = v2 in
		  ec ((if l1 = l2 then CVLoc l1 else CVNil), theta, lc)  mc))
  | Nil ->
      ec (CVNil, theta, lc) mc
	
  | Create e ->
      evalrf2_expr (e, theta, lc) mc
	(fun (v, theta, lc) mc ->
	  let newl = fresh_location () in
	  ec (CVLoc newl, update theta (LILoc newl) v, lc) mc)
  | Resume (e1, e2) ->
      evalrf2_expr (e2, theta, lc) mc
	(fun (v2, theta, lc) mc ->
	   evalrf2_expr (e1, theta, lc) mc
	     (fun (v1, theta, lc) mc ->
		(match v1 with
		   | CVLoc l ->
		       let vc = lookup theta (LILoc l) in
		       let theta = update theta (LILoc l) CVNil in
		       let mc = fun (v, theta, l) ->
			 ec (v, theta, lc) mc
		       in
		       (match vc with
		       | CVLam (i, e) ->
			   let newx = fresh_variable i in
			   evalrf2_expr
			     (rename e i newx, update theta (LIIdent newx) v2, l)
			     mc (fun (v, theta, lc) mc -> mc (v, theta, lc))
		       | CVCont ecinner ->
			   ecinner (v2, theta, l) mc))))
  | Yield e ->
      evalrf2_expr (e, theta, lc) mc 
	(fun (v, theta, lc) mc ->
	  let theta = update theta (LILoc lc) (CVCont ec) in
	  mc (v, theta, lc))

  | Transfer (e1, e2) ->
      evalrf2_expr (e2, theta, lc) mc
	(fun (v2, theta, lc) mc ->
	   evalrf2_expr (e1, theta, lc) mc
	     (fun (v1, theta, lc) mc ->
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
			  mc (fun (v, theta, lc) mc -> mc (v, theta, lc))
		    | CVCont ecinner ->
			ecinner (v2, theta, l0) mc))))      

let run_evalrf2 e =
  evalrf2_expr (e, empty, 0)
    (fun (v, theta, l) -> v)
    (fun (v, theta, l) mc -> mc (v, theta, l))

(********************************************************************)
(* back to direct, the meta continuation *)

type mdvalue =
  | MDVLoc of location
  | MDVLam of ident * expr
  | MDVNil

  | MDVCont of mdcont
and mdcont =
    (mdvalue * mdvalue store * location -> mdvalue * mdvalue store * location)

let rec evaldfm_expr (e, theta, lc) ec  =
  match e with
  | Loc l ->
      ec (MDVLoc l, theta, lc)
  | Var i ->
      ec (lookup theta (LIIdent i), theta, lc)
  | Lam (i, e) ->
      ec (MDVLam (i, e), theta, lc)
  | App (e1, e2) ->
      evaldfm_expr (e2, theta, lc)
	(fun (v2, theta, lc) ->
	  evaldfm_expr (e1, theta, lc)
	    (fun (v, theta, lc) ->
	      (match v with
	      | MDVLam (i, e) ->
		  let newx = fresh_variable i in
		  evaldfm_expr (rename e i newx, update theta (LIIdent newx) v2, lc)
		    ec)))
  | Assign (i, e) ->
      evaldfm_expr (e, theta, lc) 
	(fun (v, theta, lc) ->
	   ec (v, update theta (LIIdent i) v, lc))
  | Cond (e1, e2, e3) ->
      evaldfm_expr (e1, theta, lc)
	(fun (v, theta, lc) ->
	   (match v with
	      |	MDVNil -> evaldfm_expr (e3, theta, lc) ec
	      |	_ -> evaldfm_expr (e2, theta, lc) ec))
  | Equals (e1, e2) ->
      evaldfm_expr (e2, theta, lc) 
	(fun (v2, theta, lc) ->
	   evaldfm_expr (e1, theta, lc)
	     (fun (v1, theta, lc) ->
	        let MDVLoc l1 = v1 in
		let MDVLoc l2 = v2 in
		  ec ((if l1 = l2 then MDVLoc l1 else MDVNil), theta, lc)))
  | Nil ->
      ec (MDVNil, theta, lc)
	
  | Create e ->
      evaldfm_expr (e, theta, lc)
	(fun (v, theta, lc) ->
	  let newl = fresh_location () in
	  ec (MDVLoc newl, update theta (LILoc newl) v, lc))
  | Resume (e1, e2) ->
      evaldfm_expr (e2, theta, lc)
	(fun (v2, theta, lc) ->
	  evaldfm_expr (e1, theta, lc)
	    (fun (v1, theta, lc) ->
	      (match v1 with
	      | MDVLoc l ->
		  let vc = lookup theta (LILoc l) in
		  let theta = update theta (LILoc l) MDVNil in
		  (match vc with
		  | MDVLam (i, e) ->
		      let newx = fresh_variable i in
		      let (v, theta, l) =
			evaldfm_expr
			  (rename e i newx, update theta (LIIdent newx) v2, l)
			  (fun x -> x)
		      in ec (v, theta, lc)
		  | MDVCont ecinner ->
		      let (v, theta, l) = ecinner (v2, theta, l) in
		      ec (v, theta, lc)))))
  | Yield e ->
      evaldfm_expr (e, theta, lc)
	(fun (v, theta, lc) ->
	  let theta = update theta (LILoc lc) (MDVCont ec) in
	  (v, theta, lc))

  | Transfer (e1, e2) ->
      evaldfm_expr (e2, theta, lc)
	(fun (v2, theta, lc) ->
	   evaldfm_expr (e1, theta, lc)
	     (fun (v1, theta, lc) ->
		(match v1 with
		| MDVLoc l0 ->
		    let vc = lookup theta (LILoc l0) in
		    let theta = update theta (LILoc l0) MDVNil in
		    let theta = update theta (LILoc lc) (MDVCont ec) in
		    (match vc with
		    | MDVLam (i, e) ->
			let newx = fresh_variable i in
			evaldfm_expr
			  (rename e i newx, update theta (LIIdent newx) v2, l0)
			  (fun x -> x)
		    | MDVCont ecinner ->
			ecinner (v2, theta, l0)))))      
	

let run_evaldfm e =
  evaldfm_expr (e, empty, 0) (fun x -> x)


(********************************************************************)
(* back to direct style, 2nd edition, the standard continuation *)
(* need an installed version of a shift-reset implementation *)
(* requires caml extended with Delimcc to run *)

open Delimcc

type dvalue =
  | DVLoc of location
  | DVLam of ident * expr
  | DVNil

  | DVCont of dcont
and dcont =
    (dvalue * dvalue store * location -> dvalue * dvalue store * location)

let pdf : (dvalue * dvalue store * location) prompt =
  new_prompt ()

let rec evaldf_expr (e, theta, lc) =
  match e with
  | Loc l ->
      (DVLoc l, theta, lc)
  | Var i ->
      (lookup theta (LIIdent i), theta, lc)
  | Lam (i, e) ->
      (DVLam (i, e), theta, lc)
  | App (e1, e2) ->
      let (v2, theta, lc) = evaldf_expr (e2, theta, lc) in
      let (v1, theta, lc) = evaldf_expr (e1, theta, lc) in
      (match v1 with
      | DVLam (i, e) ->
	  let newx = fresh_variable i in
	  evaldf_expr (rename e i newx, update theta (LIIdent newx) v2, lc))
  | Assign (i, e) ->
      let (v, theta, lc) = evaldf_expr (e, theta, lc) in
	(v, update theta (LIIdent i) v, lc)
  | Cond (e1, e2, e3) ->
      let (v, theta, lc) = evaldf_expr (e1, theta, lc) in
	(match v with
	   | DVNil -> evaldf_expr (e3, theta, lc)
	   | _ -> evaldf_expr (e2, theta, lc))
  | Equals (e1, e2) ->
      let (v2, theta, lc) = evaldf_expr (e2, theta, lc) in
      let (v1, theta, lc) = evaldf_expr (e1, theta, lc) in
      let DVLoc l1 = v1 in
      let DVLoc l2 = v2 in
      ((if l1 = l2 then DVLoc l1 else DVNil), theta, lc)
  | Nil ->
      (DVNil, theta, lc)
	
  | Create e ->
      let (v, theta, lc) = evaldf_expr (e, theta, lc) in
      let newl = fresh_location () in
      (DVLoc newl, update theta (LILoc newl) v, lc)
  | Resume (e1, e2) ->
      let (v2, theta, lc) = evaldf_expr (e2, theta, lc) in
      let (v1, theta, lc) = evaldf_expr (e1, theta, lc) in
      (match v1 with
      | DVLoc l ->
	  let vc = lookup theta (LILoc l) in
	  let theta = update theta (LILoc l) DVNil in
	  (match vc with
	  | DVLam (i, e) ->
	      let (v, theta, l) =
		push_prompt pdf
		  (fun () ->
		    let newx = fresh_variable i in
		    evaldf_expr
		      (rename e i newx,
		       update theta (LIIdent newx) v2, l))
	      in (v, theta, lc)
	  | DVCont ecinner ->
	      let (v, theta, l) = ecinner (v2, theta, l) in
	      (v, theta, lc)))
  | Yield e ->
      let (v, theta, lc) = evaldf_expr (e, theta, lc) in
      shift pdf (fun ec ->
	let theta = update theta (LILoc lc) (DVCont ec) in
	(v, theta, lc))

  | Transfer (e1, e2) ->
      let (v2, theta, lc) = evaldf_expr (e2, theta, lc) in
      let (v1, theta, lc) = evaldf_expr (e1, theta, lc) in
      shift pdf (fun ec ->
	(match v1 with
	| DVLoc l0 ->
	    let vc = lookup theta (LILoc l0) in
	    let theta = update theta (LILoc l0) DVNil in
	    let theta = update theta (LILoc lc) (DVCont ec) in
	    (match vc with
	    | DVLam (i, e) ->
		let newx = fresh_variable i in
		evaldf_expr
		  (rename e i newx, update theta (LIIdent newx) v2, l0)
	    | DVCont ecinner ->
		ecinner (v2, theta, l0))))

(*
  shift : 'a prompt -> (('b -> 'a) -> 'a) -> 'b
  'b = dvalue store * dvalue
  'a = dvalue store * dvalue * dupdater
*)

let run_evaldf e =
  evaldf_expr (e, empty, 0)
    

(********************************************************************)
(* closure un-convert *)
(* requires caml extended with Delimcc to run *)

open Delimcc

type duvalue =
  | DULoc of location
  | DUFun of ducont
  | DUNil
and ducont =
    (duvalue * duvalue store * location -> duvalue * duvalue store * location)

let pdu : (duvalue * duvalue store * location) prompt =
  new_prompt ()

let rec evaldu_expr (e, theta, lc) =
  match e with
  | Loc l ->
      (DULoc l, theta, lc)
  | Var i ->
      (lookup theta (LIIdent i), theta, lc)
  | Lam (i, e) ->
      (DUFun (fun (v2, theta, lc) ->
	let newx = fresh_variable i in
	evaldu_expr (rename e i newx, update theta (LIIdent newx) v2, lc)),
       theta, lc)
  | App (e1, e2) ->
      let (v2, theta, lc) = evaldu_expr (e2, theta, lc) in
      let (v1, theta, lc) = evaldu_expr (e1, theta, lc) in
      (match v1 with
      |	DUFun (g) ->
	  g (v2, theta, lc))
  | Assign (i, e) ->
      let (v, theta, lc) = evaldu_expr (e, theta, lc) in
	(v, update theta (LIIdent i) v, lc)
  | Cond (e1, e2, e3) ->
      let (v, theta, lc) = evaldu_expr (e1, theta, lc) in
	(match v with
	   | DUNil -> evaldu_expr (e3, theta, lc)
	   | _ -> evaldu_expr (e2, theta, lc))
  | Equals (e1, e2) ->
      let (v2, theta, lc) = evaldu_expr (e2, theta, lc) in
      let (v1, theta, lc) = evaldu_expr (e1, theta, lc) in
      let DULoc l1 = v1 in
      let DULoc l2 = v2 in
      ((if l1 = l2 then DULoc l1 else DUNil), theta, lc)
  | Nil ->
      (DUNil, theta, lc)
	
  | Create e ->
      let (v, theta, lc) = evaldu_expr (e, theta, lc) in
      let newl = fresh_location () in
      (DULoc newl, update theta (LILoc newl) v, lc)
  | Resume (e1, e2) ->
      let (v2, theta, lc) = evaldu_expr (e2, theta, lc) in
      let (v1, theta, lc) = evaldu_expr (e1, theta, lc) in
      (match v1 with
      | DULoc l ->
	  let vc = lookup theta (LILoc l) in
	  let theta = update theta (LILoc l) DUNil in
	  (match vc with
	  | DUFun (g) ->
	      let (v, theta, l) =
		push_prompt pdu
		  (fun () ->
		    g (v2, theta, l))
	      in (v, theta, lc)))
  | Yield e ->
      let (v, theta, lc) = evaldu_expr (e, theta, lc) in
      shift pdu (fun ec ->
	let theta = update theta (LILoc lc) (DUFun ec) in
	(v, theta, lc))

  | Transfer (e1, e2) ->
      let (v2, theta, lc) = evaldu_expr (e2, theta, lc) in
      let (v1, theta, lc) = evaldu_expr (e1, theta, lc) in
      shift pdu (fun ec ->
	(match v1 with
	| DULoc l0 ->
	    let vc = lookup theta (LILoc l0) in
	    let theta = update theta (LILoc l0) DUNil in
	    let theta = update theta (LILoc lc) (DUFun ec) in
	    (match vc with
	    | DUFun (g) ->
		g (v2, theta, l0))))


let run_evaldu e =
  evaldu_expr (e, empty, 0)
    
(* globalize the single-threaded store *)
type dgvalue =
  | DGLoc of location
  | DGFun of (dgvalue -> dgvalue)
  | DGNil

let the_theta : dgvalue store ref =
  ref empty
let current_l =
  ref 0

let update_ a v =
  the_theta := update !the_theta a v
let update_Loc l v =
  the_theta := update !the_theta (LILoc l) v
let lookup_ a =
  lookup !the_theta a
let lookup_Loc l =
  lookup !the_theta (LILoc l)

let pdg : dgvalue prompt =
  new_prompt ()

let rec evaldg_expr e =
  match e with
  | Loc l ->
      DGLoc l
  | Var i ->
      lookup_ (LIIdent i)
  | Lam (i, e) ->
      DGFun (fun v2 ->
	let newx = fresh_variable i in
	update_ (LIIdent newx) v2;
	evaldg_expr (rename e i newx))
  | App (e1, e2) ->
      let v2 = evaldg_expr e2 in
      let v1 = evaldg_expr e1 in
      (match v1 with
      |	DGFun (g) ->
	  g v2)
  | Assign (i, e) ->
      let v = evaldg_expr e in
      update_ (LIIdent i) v;
      v
  | Cond (e1, e2, e3) ->
      let v = evaldg_expr e1 in
	(match v with
	   | DGNil -> evaldg_expr e3
	   | _ -> evaldg_expr e2)
  | Equals (e1, e2) ->
      let v2 = evaldg_expr e2 in
      let v1 = evaldg_expr e1 in
      let DGLoc l1 = v1 in
      let DGLoc l2 = v2 in
      (if l1 = l2 then DGLoc l1 else DGNil)
  | Nil ->
      DGNil
	
  | Create e ->
      let v = evaldg_expr e in
      let newl = fresh_location () in
      update_ (LILoc newl) v;
      DGLoc newl
  | Resume (e1, e2) ->
      let v2 = evaldg_expr e2 in
      let v1 = evaldg_expr e1 in
      (match v1 with
      | DGLoc l ->
	  let vc = lookup_ (LILoc l) in
	  update_ (LILoc l) DGNil;
	  (match vc with
	  | DGFun (g) ->
	      let lc = !current_l in
	      let v =
		push_prompt pdg
		  (fun () ->
		    current_l := l;
		    g v2)
	      in current_l := lc;
	      v))
  | Yield e ->
      let v = evaldg_expr e in
      let lc = !current_l in
      shift pdg (fun ec ->
	update_ (LILoc lc) (DGFun ec);
	v)

  | Transfer (e1, e2) ->
      let v2 = evaldg_expr e2 in
      let v1 = evaldg_expr e1 in
      shift pdg (fun ec ->
	(match v1 with
	| DGLoc l0 ->
	    let lc = !current_l in
	    let vc = lookup_ (LILoc l0) in
	    update_ (LILoc l0) DGNil;
	    update_ (LILoc lc) (DGFun ec);
	    current_l := l0;
	    (match vc with
	    | DGFun (g) ->
		g v2)))

let run_evaldg e =
  reset_counter ();
  the_theta := empty;
  current_l := 0;
  evaldg_expr e
