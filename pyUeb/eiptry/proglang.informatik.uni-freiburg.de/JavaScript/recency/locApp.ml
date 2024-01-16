open ProglangUtils
open Graph
open Location
open ProgLabel

(* Shortcuts for types *)
type var = Syntax.var
type e = Syntax.e
type plab = Syntax.plab
type loc = Loc.t
type label = Syntax.label


(* Abstract Values as in the paper *)
module Val : sig
  type elem
  val create_loc : loc -> elem
  val create_lam : var -> var -> e -> elem
  type set
  val empty : set
  val mem : elem -> set -> bool
  val add : elem -> set -> set
  val is_empty : set -> bool
  val union : set -> set -> set * bool
  val iter_lam : (var -> var -> e -> unit) list -> set -> unit
  val iter_loc : (loc -> unit) list -> set -> unit
  val string_of : set -> string
end = struct
  type afun = Syntax.var * Syntax.var * Syntax.e
  type elem = 
      Fun of afun
    | Location of Loc.t
  let create_loc l = Location l
  let create_lam v1 v2 e = Fun (v1,v2,e)
    
  module ValSetLam = ExtSSet.Make(
    struct 
      type t = afun
      let compare v1 v2 = Pervasives.compare v1 v2
      let string_of (v1,v2,e) = Syntax.string_of_lam_with_plabs v1 v2 e
      let sep = ","
    end)
  module ValSetLoc = ExtSSet.Make(
    struct 
      type t = Loc.t
      let compare = Loc.compare
      let string_of = Loc.string_of
      let sep = ","
    end
    )

  type set = ValSetLam.t * ValSetLoc.t
  let cardinal (s1,s2) = ValSetLam.cardinal s1 + ValSetLoc.cardinal s2

  let empty = ValSetLam.empty, ValSetLoc.empty
  let mem e (slam,sloc) = match e with
      Fun afun -> ValSetLam.mem afun slam
    | Location l -> ValSetLoc.mem l sloc

  let add = function
      Fun afun -> (fun (slam,sloc) -> ValSetLam.add afun slam, sloc)
    | Location l -> (fun (slam,sloc) -> slam, ValSetLoc.add l sloc)
  let is_empty (s1,s2) = ValSetLam.is_empty s1 && ValSetLoc.is_empty s2

  let union s1 s2 =
    let anz = cardinal s2 in
    let ns = ValSetLam.union (fst s1) (fst s2), 
      ValSetLoc.union (snd s1) (snd s2)  
    in
    let nanz = cardinal ns in
      ns, nanz > anz

  let rec iter_lam = function
    | [] -> (fun s -> ())
    | f :: fs -> 
        (fun s -> 
           ValSetLam.iter (fun (v1,v2,e) -> f v1 v2 e) (fst s);
           iter_lam fs s
        )
  let rec iter_loc = function
    | [] -> (fun s -> ())
    | f :: fs -> (fun s -> ValSetLoc.iter f (snd s); iter_loc fs s)

  let string_of (s1,s2) = 
    let sep = 
      if ((not (ValSetLam.is_empty s1)) && (not (ValSetLoc.is_empty s2))) then
        ","
      else
        ""
    in
      ValSetLam.string_of s1 ^ sep ^ ValSetLoc.string_of s2

  (* Test Unit *)
  module ValTest = 
  struct
    open Test
      
    let t1 () = 
      let _ = Loc.reset () in
      let e = empty in
      let e = add (create_loc (Loc.create_new ())) e in
      let e = add (create_loc (Loc.create_new ())) e in
      let l3 = Loc.create_new () in
      let e = add (create_loc l3) e in
      let e = add (create_loc (Loc.create_new ())) e in
      let e = add (create_loc l3) e in
      let f = create_lam 
        (Syntax.create_var "y")
        (Syntax.create_var "x")
        (Syntax.e_sexp (Syntax.s_new ()))
      in
      let g = create_lam 
        (Syntax.create_var "y")
        (Syntax.create_var "x")
        (Syntax.e_sexp (Syntax.s_new ()))
      in
      let e = add f e in
      let e = add g e in
      let e = add f e in
      let es_exp = "(\\y,x.new_l4p12),(\\y,x.new_l5p13),l0,l1,l2,l3" in
      let es = string_of e in
        assert_equal
          ~cmp:(=)
          ~printer:(fun s -> s)
          es_exp
          es
    let _ =
      install_tests "locApp.Val"
        [("Test 1", t1)]
  end
end

(* Nodes of the graph *)
module Node : sig
  type t
  val make_env : var -> t
  val make_cache : plab -> t
  val make_heap : loc -> label -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val string_of : t -> string
end = struct
  type t =
    | AbsEnv of var
    | AbsCa of plab
    | AbsH of loc * label 
  let make_env v = AbsEnv v
  let make_cache l = AbsCa l
  let make_heap loc lab = AbsH (loc,lab)
  let compare = Pervasives.compare
  let equal n1 n2 = (compare n1 n2 == 0)
  let hash = Hashtbl.hash

  let string_of = function
    | AbsEnv v -> "r("^(Syntax.string_of_var v)^")"
    | AbsCa p -> "C("^(Syntax.string_of_plab p)^")"
    | AbsH (l,a) ->
        "H("^(Loc.string_of l)^")("^(Syntax.string_of_label a)^")"
end

(* Annotations of the graph vertexies *)
module Annot : sig
  type t
  type fa_lam
  type fa_loc
  val create : unit -> t
  val create_fa_lam : (var -> var -> Syntax.e -> unit) -> fa_lam
  val create_fa_loc : (loc -> unit) -> fa_loc

  val val_mem : t -> Val.elem -> bool
  val add_val : t -> Val.elem -> unit
  val add_lam : t -> fa_lam -> unit
  val add_loc : t -> fa_loc -> unit

  val is_empty : t -> bool
  val union    : t -> t -> bool
  val run_funs : t -> unit

  val string_of : t -> string
end = struct
  type fa_lam = var -> var -> Syntax.e -> unit
  type fa_loc = loc -> unit
  type t = {
    absval : Val.set ref;
    lamfun : fa_lam list ref;
    locfun : fa_loc list ref;
  }
  let create () = 
    { absval = ref Val.empty; 
      lamfun = ref []; 
      locfun = ref [] 
    }
  let create_fa_lam x = x
  let create_fa_loc x = x
  let is_empty {absval = a} = Val.is_empty !a

  let union {absval = left} {absval = right} =
    let new_right,changed = Val.union !left !right in
      right := new_right;
      changed
  let run_funs { absval = a; lamfun = lamf; locfun = locf } =
    Val.iter_lam !lamf !a;
    Val.iter_loc !locf !a

  let val_mem {absval = a} e = Val.mem e !a
  let add_val {absval = a} e = a := Val.add e !a
  let add_lam {lamfun = l} lam = l := lam :: !l
  let add_loc {locfun = l} loc = l := loc :: !l
  let string_of { absval = v; lamfun = laf; locfun = lof} = 
    "{value = "^(Val.string_of !v) 
    ^ "; lamf = " ^(string_of_int (List.length !laf))
    ^ "; locf = " ^(string_of_int (List.length !lof)) ^ "}"

  (* Test Unit *)
  module ValTest = 
  struct
    open Test
      
    let add_val () = 
      let _ = Loc.reset () in
      let a = create () in
      let _ = add_val a (Val.create_loc (Loc.create_new ())) in
      let _ = add_val a (Val.create_loc (Loc.create_new ())) in
      let _ = add_lam a (create_fa_lam (fun v1 v2 e -> ())) in
      let _ = add_lam a (create_fa_lam (fun v1 v2 e -> ())) in
      let _ = add_loc a (create_fa_loc (fun l -> ())) in
      let _ = add_loc a (create_fa_loc (fun l -> ())) in
      let _ = add_loc a (create_fa_loc (fun l -> ())) in
      let _ = add_loc a (create_fa_loc (fun l -> ())) in
        "error in add_lam, not the right count of lambdas added to the list." 
        @? (List.length !(a.lamfun) == 2);
        "error in add_loc, not the right count of lambdas added to the list." 
        @? (List.length !(a.locfun) == 4)

    let _ =
      install_tests "locApp.Annot"
        [("add values", add_val)]
  end    
end
    

(* This is the graph, together with a hash table to store the
   annotations and a worklist to notice which vertexies needs
   to traverse for solving the constraint system.
   - add_subset_xy:
     These methods adds an edge in the graph between x and y,
     so that x \subseteq y should hold.
     We add x to the worklist to ensure that this constraint is propergated.
   - add_to_x:         x \in {c,e,h}
     This is needed to fill the graph with it's initial values and
     to propergate the abstract values within the graph.
     We add the node to the worklist if one of this function is called.
   - add_to_c_lam / add_to_c_loc:
     This is needed to modell the \forall constrains from the paper.
     These are functions that are called every time a node is 
     traversed. Here we ensure that every fa_lam function is called 
     for every \lambda \in Annot, and fa_loc for every l \in Annot.
     If a lam or loc function is added to a node, the node we add
     it to the worklist.
*)
module Data : sig
  type t
  val create : int -> t
                (* data *)  (* first part *)  (*second part*)
  val add_subset_ec : t ->   var ->                   plab -> unit
  val add_subset_ce : t ->  plab ->                    var -> unit
  val add_subset_cc : t ->  plab ->                   plab -> unit
  val add_subset_hc : t ->   loc -> label ->          plab -> unit
  val add_subset_ch : t ->  plab ->           loc -> label -> unit
  val add_subset_he : t ->   loc -> label ->           var -> unit
  val add_to_c      : t ->  plab ->               Val.elem -> unit
  val add_to_e      : t ->   var ->               Val.elem -> unit
  val add_to_h      : t ->   loc -> label ->      Val.elem -> unit
  val add_to_c_lam  : t ->  plab ->           Annot.fa_lam -> unit
  val add_to_c_loc  : t ->  plab ->           Annot.fa_loc -> unit

  val traverse : t -> unit
  val string_of     : t -> string
  val wl_empty : t -> bool
end = struct
  module G = struct
    include Graph.Imperative.Digraph.Concrete(Node)

    let vertex_list g =
      let vl = ref [] in
        iter_vertex 
          (fun v -> vl := v :: !vl) 
          (*; print_endline "vertex into list"; ()) *)
          g;
        !vl
    let edge_list g =
      let el = ref [] in
        iter_edges (fun v1 v2 -> el := (v1,v2) :: !el) g;
        !el
      
    let string_of g =
      let vl = vertex_list g in
      let el = edge_list g in
      let vs = String.concat ", " 
        (List.map 
           (fun v -> Node.string_of (V.label v))
           vl) 
      in
      let es = String.concat ", "
        (List.map 
           (fun (v1,v2) -> (Node.string_of (V.label v1)) ^ "->" ^
              (Node.string_of (V.label v2)))
           el)
      in
        "\nGraph:\n   Vertices,"^(string_of_int (List.length vl))
        ^" = {"^vs^"};\n   Edge,"^(string_of_int (List.length el))
        ^" = {" ^ es ^ "}"
  end
  module H = struct
    module HashT = Hashtbl.Make(
      struct
        type t = Node.t
        let equal = (=)
        let hash = Hashtbl.hash
      end)
    type t = Annot.t HashT.t
    let create = HashT.create

    let find h node = 
      try 
        HashT.find h node
      with Not_found ->
        begin
          let c = Annot.create () in
            HashT.add h node c;
            c
        end
    let string_of h = 
      let na_list = HashT.fold (fun n a l -> (n,a) :: l) h [] in
      let hs = String.concat ";\n" 
          (List.map 
             (fun (n,a) -> (Node.string_of n) ^ " -> " ^ (Annot.string_of a))
             na_list)
      in
      "Hashtbl:\n"^hs
      
      
  end
  type t = {
    graph: G.t;
    hashtbl: H.t;
    worklist: Node.t list ref;
  }
  let find {hashtbl = h} = H.find h
  let create size = 
    { graph = G.create ();
      hashtbl = H.create size;
      worklist = ref [];
    }
  let add_to_worklist : t -> Node.t -> unit = fun {worklist = w} n ->
    if (not (List.exists (fun n2 -> Node.compare n n2 == 0) !w)) then
      w := n :: !w

  let add_edge : t -> Node.t -> Node.t -> unit = fun d n1 n2 ->
    let add_node : t -> Node.t -> G.V.t = fun {graph = g} n ->
      let vertex = G.V.create n in
        G.add_vertex g vertex;
        vertex 
    in
    let v1 = add_node d n1 in
    let v2 = add_node d n2 in
      G.add_edge d.graph v1 v2;
      add_to_worklist d n1
    

  (* add the given node to the graph and to the hash table the
     node with assigned value of type eVal *)
  let add_to_node : t -> Val.elem -> Node.t -> unit = fun d value node ->
    let a = find d node in
      (* Is the element already in the set? If it is,
         we do nothing *)
      if (not (Annot.val_mem a value)) then begin
        (* otherwise, we add it and put the node on the working set *)
        Annot.add_val a value;
        add_to_worklist d node
      end
          
  let add_to_e g var value = add_to_node g value (Node.make_env var)
  let add_to_c g plab value = add_to_node g value (Node.make_cache plab)
  let add_to_h g loc label v = add_to_node g v (Node.make_heap loc label)
                                          
  let add_subset_ec : t -> var -> plab -> unit = fun g v l ->
    add_edge g (Node.make_env v) (Node.make_cache l)
  let add_subset_ce : t -> plab -> var -> unit = fun g l v ->
    add_edge g (Node.make_cache l) (Node.make_env v) 
  let add_subset_cc : t -> plab -> plab -> unit = fun g l1 l2 ->
    add_edge g (Node.make_cache l1) (Node.make_cache l2)
  let add_subset_hc : t -> loc -> label -> plab -> unit = fun g l1 a l2 ->
    add_edge g (Node.make_heap l1 a) (Node.make_cache l2)
  let add_subset_ch : t -> plab -> loc -> label -> unit = fun g l1 l2 a ->
    add_edge g (Node.make_cache l1) (Node.make_heap l2 a) 
  let add_subset_he : t -> loc -> label ->  var -> unit = fun g l1 a x ->
    add_edge g (Node.make_heap l1 a) (Node.make_env x) 


  let add_to_c_lam d plab = 
    let n = (Node.make_cache plab) in
      add_to_worklist d n;
      Annot.add_lam (find d n)
  let add_to_c_loc d plab = 
    let n = (Node.make_cache plab) in
      add_to_worklist d n;
      Annot.add_loc (find d n)

  let traverse ({graph = g; hashtbl = h; worklist = wl} as d) =
    let rec do_union node node_an = function
      | [] -> ()
      | s :: sl -> 
          if Annot.union node_an (find d s) then add_to_worklist d s;
          do_union node node_an sl
    in     
    let do_trav node = 
      let node_an = find d node in
        if (Annot.is_empty node_an) then
          (* Annot is empty, nothing to do *)
          ()
        else begin
          (* Ok, let's do the work. First propergate all elements
             to all succesors. 
             Later, call the fun_loc and fun_lam functions
          *)
          do_union node node_an (try G.succ g node with _ -> []);
          Annot.run_funs node_an;
        end
    in

      match !wl with
        | [] -> ()
        | node :: wlt -> 
            let _ = wl := wlt in
              do_trav node
      
  let string_of_wl wl =
    let ws = String.concat ", " (List.map Node.string_of !wl) in
      "Worklist,"^ (string_of_int (List.length !wl)) ^ ": " ^ ws

  let wl_empty {worklist = wl} = 
    match !wl with
        [] -> true
      | _ -> false
    


  let string_of : t -> string = fun {graph = g; hashtbl = h; worklist = wl} ->
    "{" ^ (G.string_of g)^"\n"^(H.string_of h)^"\n"^(string_of_wl wl)^"}"



  module TestData = struct
    open Test
      
    let t1 () = 
      let _ = ProgLabel.reset () in
      let d = create 25 in
      let pl0 = Syntax.create_plab () in
      let pl1 = Syntax.create_plab () in
      let x = (Syntax.create_var "x") in
      let y = (Syntax.create_var "y") in
      let _ = add_subset_ec d x pl0 in
      let _ = add_subset_ec d y pl1 in
      let _ = add_subset_cc d pl0 pl1 in
      let _ = add_to_e d x (Val.create_loc (Loc.create_new ())) in
      let s = string_of d in
      let s_exp = "{\nGraph:\n   Vertices,4 = {C(p1), C(p0), r(y), r(x)};\n"
        ^"   Edge,3 = {C(p0)->C(p1), r(y)->C(p1), r(x)->C(p0)}\n"
        ^"Hashtbl:\nr(x) -> {value = l2; lamf = 0; locf = 0}\n"
        ^"Worklist,3: C(p0), r(y), r(x)}" 
      in
        assert_equal
          ~cmp:(=)
          ~printer:(fun s -> s)
          s_exp
          s      

    let _ =
      install_tests "locApp.Data"
        [("data test 1", t1)]
  end
end      
  
let create e =
  (* first create new empty graph, new empty hash table 
     and new empty work list *)
  let g = Data.create 25 in
  (* now create all vertexes and edges and forall constraints
     of the graph using the add methods.
     This will initialise the worklist automaticly, so after
     doing this we only have to start and propergate the locations
     and lambda throw the graph.
  *)
  let rec create_const_v : Syntax.v -> unit = function
    | Syntax.Var (plab, v) -> 
        Data.add_subset_ec g v plab
    | Syntax.Lam (plab,y,z,eb) ->
        Data.add_to_c g plab (Val.create_lam y z eb);
        create_const_e eb;
    | _ -> ()
  and create_const_e : Syntax.e -> unit = function
    | Syntax.SExp s -> create_const_s s
    | Syntax.Let (plab,_,x,e1,e2) ->
        Data.add_subset_ce g (Syntax.e_get_plab e1) x;
        Data.add_subset_cc g (Syntax.e_get_plab e2) plab;
        create_const_e e1;
        create_const_e e2;
  and create_const_s : Syntax.s -> unit = function
    | Syntax.Val v -> create_const_v v
    | Syntax.App (l,v1,v2,_,_) -> 
        create_const_v v1;
        create_const_v v2;
        let l1 = Syntax.v_get_plab v1 in
        let l2 = Syntax.v_get_plab v2 in
        Data.add_to_c_lam g l1
          (Annot.create_fa_lam
             (fun y z e ->
                Data.add_subset_ce g l2 z;
                Data.add_subset_cc g (Syntax.e_get_plab e) l
             ))
    | Syntax.New (plab,l) -> Data.add_to_c g plab (Val.create_loc l)
    | Syntax.Read p -> create_const_p p
    | Syntax.Write (l,p,v2,_) ->
        create_const_p p;
        create_const_v v2;
        let Syntax.Prop (lr,v1,a) = p in
        let l1 = Syntax.v_get_plab v1 in
        let l2 = Syntax.v_get_plab v2 in
          Data.add_to_c_loc g l1
            (Annot.create_fa_loc (fun loc -> Data.add_subset_ch g l2 loc a))
    | Syntax.MethodCall (l,p,v2,_,_) ->
        create_const_p p;
        create_const_v v2;
        let Syntax.Prop (l1,vo,a) = p in
        let lo = Syntax.v_get_plab vo in
        let l2 = Syntax.v_get_plab v2 in
          Data.add_to_c_lam g l1
            (Annot.create_fa_lam
               (fun y z e ->
                  let l0 = Syntax.e_get_plab e in
                    Data.add_subset_ce g l2 z;
                    Data.add_subset_cc g l0 l;
                    Data.add_subset_ce g lo y;
                    
               ))
          
  and create_const_p : Syntax.p -> unit = function
    | Syntax.Prop (l,v,a) ->
        let l1 = Syntax.v_get_plab v in
          create_const_v v;
          Data.add_to_c_loc g l1
            (Annot.create_fa_loc (fun loc -> Data.add_subset_hc g loc a l ))
  in
    create_const_e e;
    g

let do_one_step = Data.traverse 
let solve d =
  let rec loop () =
    let _ = do_one_step d in
      if (not (Data.wl_empty d)) then
        loop ()
      else
        ()
  in
    loop ()

module TestCreate = struct
  open Test
  open Syntax

  let t1 () =
    let _ = ProgLabel.reset () in
    let _ = Loc.reset () in
    let x = create_var "x" in
    let x1 = create_var "x1" in
    let x2 = create_var "x2" in
    let y = create_var "y" in
    let a = create_label "a" in
    let f = create_label "f" in
    let xfn = e_sexp (s_mcall (v_var x) f (v_var x)) in
    let lamb = e_sexp (s_read (v_var x2) a) in
    let lam = v_lam x1 x2 (lamb) in
    let eiu1 = e_sexp (s_write (v_var x) f lam) in
    let li = e_let (create_var "u1") eiu1 xfn in
    let lm = e_let 
      (create_var "u2") 
      (e_sexp (s_write (v_var x) a (v_var y))) 
      li
    in
    let la = e_let x (e_sexp (s_new ())) lm in
    let e = e_let y (e_sexp (s_val (v_undef ()))) la in
    let d = create e in
      let _ = solve d in
        assert_equal
          ~cmp:(=)
          ~printer:string_of_bool
          true
          (Data.wl_empty d);
        assert_equal
          ~cmp:(=)
          ~printer:(fun s -> s)
          ""
          (Data.string_of d)
        
        
        
  let _ =
    install_tests "locApp"
      []
(*      [("create test 1", t1)] *)

end
