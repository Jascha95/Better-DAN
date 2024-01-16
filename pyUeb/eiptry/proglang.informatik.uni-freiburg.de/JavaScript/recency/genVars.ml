open ProglangUtils
open ExtUtils

exception EUnion of string

module type PREFIX = 
sig
  val prefix : string
end

module type OBSERVER = 
sig
  type t
  val compare : t -> t -> int
  val string_of : t -> string
  val fire : t -> unit
end

module type IMG = 
sig
  type t
  val compare : t -> t -> int
  val merge : t -> t -> t
  val normalize : t -> t
  val string_of : t -> string
end


module type STATE = 
sig
  type obs 
  val add_wl : obs -> unit
end

module type VAR_WITHOUT_IMG =
sig
  type obs
  type t
  val create : unit -> t
  val compare : t -> t -> int
  val total_ord : t -> t -> int
  val add_alias : t -> t -> unit
  val do_normalize : t -> unit
  val add_observer : obs -> t -> unit
  val remove_observer : obs -> t -> unit
  val string_of : t -> string
  val reset : unit -> unit
  val get_all_ts : unit -> t list
end

module type VAR = sig
  include VAR_WITHOUT_IMG
  type img
  val add_img : img -> t -> unit
  val get_img : t -> img option
end
module type CVAR =
  functor (Obs: OBSERVER) ->
    VAR
  with type obs = Obs.t

module type MAKE =
  functor (Pre: PREFIX) ->
    functor (Obs: OBSERVER) ->
      functor (Img: IMG) -> 
        functor (State: STATE with type obs = Obs.t) ->
          VAR
  with type obs = Obs.t
with type img = Img.t

module Make = 
  functor (Pre: PREFIX) -> 
    functor (Obs: OBSERVER) -> 
      functor (Img: IMG) ->
        functor (State: STATE with type obs = Obs.t) ->
struct
  type img = Img.t
  type obs = Obs.t

  module type OBSERVERSET = 
  sig
    type t
    val empty : t
    val add : t -> obs -> t
    val union : t -> t -> t
    val remove : t -> obs -> t
    val fire : t -> unit
    val add_list : t -> obs list -> t
    val from_list : obs list -> t
    val string_of : t -> string
  end
  module ObserverSet : OBSERVERSET = struct
    open ExtList
    type t = obs list

    let empty = []
    let add l a1 = a1 :: l
    let union = List.fold_left add
    let fire l =
      let rec f l = function
        | [] -> ()
        | h :: t -> 
            if (not (List.mem_cmp (Utils.compare_to_equal Obs.compare) h l)) then begin
              State.add_wl h;
              f (h :: l) t
            end else begin
              f l t
            end
      in
        f [] l

    let remove os o = List.remove o os
    let add_list l ol = List.fold_left add ol l
    let from_list l = add_list l empty
    let string_of l =
      String.concat ";" (List.map Obs.string_of l)
  end
    
  type t = 
      { id: int;
        unique: t ref;
        value: img option ref;
        observers: ObserverSet.t ref;
      }

  let empty = None
  let img_equal = Utils.compare_to_equal Img.compare


  let i = ref 0
  let all_ts = ref []
  let get_all_ts () = !all_ts
  let reset () = 
    i := 0;
    all_ts := []
  let create () = 
    let x = !i in
      i := !i + 1;
      (* here we create a new TypeVar, with
       *  the second part points to itself, because 
       * we don't have aliases. The third part is empty,
       * there is no mapping if you create a new typevar.
       * We don't have observer funktions, so set it
       * to the empty list *)
      let rec res = 
        {id = x; 
         unique = ref res; 
         value = ref empty; 
         observers = ref ObserverSet.empty;
        }
      in
        all_ts := res :: !all_ts;
        res

  let get_obs { observers = flr; } = !flr
  let fire_obs t = ObserverSet.fire (get_obs t)

  (* follows the alias links, compact the links and give the 
     unique last type variable *)
  let rec get_unique : t -> t = fun {id = i1; unique = r1 } ->
    let {id = i2; unique = r2} as t2 = !r1 in
      if (i1 = i2) then
        t2
      else
        (r1 := !r2; get_unique t2)

  open ExtList

  let add_img img t =
    let {value = imgr; } as t = get_unique t in
    let old_i = !imgr in
    let add_eol = fun img -> img in
    let res = Option.maybe_tuppel 
      ~f:Img.merge ~fa:add_eol ~fb:add_eol
      !imgr (Some img) 
    in
      match res with
          None -> ()
        | Some im -> 
            begin
                imgr := Some im;
                if Option.maybe_equal img_equal !imgr old_i then
                  () (* print_endline "Images equal" *)
                else begin
                  fire_obs t;
                end
            end
  let add_observer obf t =
    let { observers = flr } = get_unique t in
      flr := ObserverSet.add !flr obf

  let rec compare tv1 tv2 =
    let {id = i1 } = get_unique tv1 in
    let {id = i2 } = get_unique tv2 in
      i1 - i2

  let string_of : t -> string = fun ({ id = i } as t) ->
    let { id = uid } = get_unique t in
      if (i == uid) then
        Pre.prefix ^ (string_of_int i)
      else
        Pre.prefix ^ (string_of_int i) ^ "[" ^ (string_of_int uid)  ^ "]"
        
  let get_img : t -> img option = fun tv ->
    let {value = imgr} = get_unique tv in
      match !imgr with
        | None -> None
        | Some i -> let i = Img.normalize i in
            imgr := Some i;
            Some i

  let add_alias v1 v2 =
    let { id = i1; 
          unique = vr1; 
          value = r1; 
          observers = obs1;
        } as uv1
        = get_unique v1 
    in
    let { id = i2; 
          unique = vr2; 
          value = r2; 
          observers = obs2;
        } = get_unique v2 
    in
      if i1 != i2 then begin
        let add_eol = fun img -> img in
          r1 := Option.maybe_tuppel 
            ~f:Img.merge ~fa:add_eol ~fb:add_eol 
            !r1 !r2;
          vr2 := uv1;
          obs1 := ObserverSet.union !obs1 !obs2;
          fire_obs uv1;
          ignore (r2 := empty);
      end

  let remove_observer ob v =
    let {observers = obs } = get_unique v in
      obs := ObserverSet.remove !obs ob

  let do_normalize t =
    let { value = r } as tu = get_unique t in
      begin
        match !r with
          | Some i -> 
              let im = Img.normalize i in
                if (not (Img.compare im i == 0)) then begin
                  r := Some im;
(*                   print_endline ("fire obs: "^(string_of t)); *)
(*                   print_endline ("image: " ^ Img.string_of im); *)
(*                   print_endline ("obs: " ^ ObserverSet.string_of (get_obs tu)); *)
                  fire_obs tu
                end
          | None -> ()
      end

  let total_ord ({ id = id1 } as t1) ({ id = id2 } as t2) =
    if id1 - id2 == 0 then 0 else
      Pervasives.compare t1 t2

end

module TypeVarTest = struct
  module ISet = struct
    include ExtSSet.Make(
      struct 
        type t = int 
        let compare = (-)
        let sep = ","
        let string_of = string_of_int
      end)
    let normalize t = t
    let merge = union
    let ask_for_obs _ = []
    type obs = int
  end
  module TestVar = Make(struct let prefix = "tv_" end)
    (struct 
        type t=int 
        let compare = (-) 
        let fire x = ()
        let prefix = "tv_"
        let string_of = string_of_int 
      end)
    (ISet)
    (struct
       type obs = int
       let add _ = ()
       let remove _ = ()
       let add_wl _ = ()
     end)
  open Test

  let init_tests () = 
    let make_pairs tvl = List.flatten
      (List.map (fun tv -> List.map (fun tv2 -> (tv,tv2)) tvl) tvl)
    in 
    let comp_cmp_sum tvpl = List.fold_left 
      (fun x y -> x + y)
      0
      (List.map 
         (fun (tv1,tv2) -> 
            if (abs (TestVar.compare tv1 tv2)) > 0 
            then 1 
            else 0
         ) 
         tvpl
    )
    in
      
    let create_tv () =
      let tv1 = TestVar.create () in
      let tv2 = TestVar.create () in
      let tv3 = TestVar.create () in
      let tv4 = TestVar.create () in
      let tv5 = TestVar.create () in
        [tv1;tv2;tv3;tv4;tv5],tv1,tv2,tv3,tv4,tv5
    in
    let t1 () = 
      let tv1 = TestVar.create () in
      let tv2 = TestVar.create () in
        (* here the two should be not equal *)
        assert_equal 
          ~cmp:(fun t1 t2 -> TestVar.compare t1 t2 != 0)
          ~printer: (TestVar.string_of)
          tv1
          tv2;
        ignore (TestVar.add_alias tv1 tv2);
        (* now they should be equal *)
        assert_equal 
          ~cmp:(fun t1 t2 -> TestVar.compare t1 t2 = 0)
          ~printer: (TestVar.string_of)
          tv1
          tv2      
    in
      
      
    let t2 () = 
      let tvl,tv1,tv2,tv3,tv4,tv5 = create_tv () in
        ignore (TestVar.add_alias tv1 tv2);
        ignore (TestVar.add_alias tv3 tv4);
        let sum_1 = comp_cmp_sum (make_pairs tvl) in
          ignore (TestVar.add_alias tv2 tv4);
          let sum_2 = comp_cmp_sum (make_pairs tvl) in
            (* now they should be equal *)
            assert_equal 
              ~printer: (string_of_int)
              16
            sum_1;
            assert_equal 
              ~printer: (string_of_int)
              8
              sum_2
    in
      
    let t3 () = 
      let tvl,tv1,tv2,tv3,tv4,tv5 = create_tv () in
        ignore (TestVar.add_alias tv1 tv2);
        ignore (TestVar.add_alias tv3 tv4);
        let sum_1 = comp_cmp_sum (make_pairs tvl) in
          ignore (TestVar.add_alias tv1 tv4);
          let sum_2 = comp_cmp_sum (make_pairs tvl) in
            (* now they should be equal *)
            assert_equal 
              ~printer: (string_of_int)
              16
              sum_1;
            assert_equal 
              ~printer: (string_of_int)
              8
              sum_2
    in
    let t4 () = 
      let tvl,tv1,tv2,tv3,tv4,tv5 = create_tv () in
        ignore (TestVar.add_alias tv1 tv2);
        ignore (TestVar.add_alias tv3 tv4);
        let sum_1 = comp_cmp_sum (make_pairs tvl) in
          ignore (TestVar.add_alias tv1 tv3);
          let sum_2 = comp_cmp_sum (make_pairs tvl) in
            (* now they should be equal *)
            assert_equal 
              ~printer: (string_of_int)
              16
              sum_1;
            assert_equal 
              ~printer: (string_of_int)
              8
              sum_2
    in
      
    let t5 () = 
      let tvl,tv1,tv2,tv3,tv4,tv5 = create_tv () in
        ignore (TestVar.add_alias tv1 tv2);
        ignore (TestVar.add_alias tv3 tv4);
        let sum_1 = comp_cmp_sum (make_pairs tvl) in
          ignore (TestVar.add_alias tv2 tv3);
          let sum_2 = comp_cmp_sum (make_pairs tvl) in
            (* now they should be equal *)
            assert_equal 
              ~printer: (string_of_int)
              16
              sum_1;
            assert_equal 
              ~printer: (string_of_int)
              8
              sum_2
    in
    let t6 () = 
      let tvl,tv1,tv2,tv3,tv4,tv5 = create_tv () in
        ignore (TestVar.add_alias tv1 tv2);
        ignore (TestVar.add_alias tv3 tv4);
        let sum_1 = comp_cmp_sum (make_pairs tvl) in
          ignore (TestVar.add_alias tv2 tv5);
          let sum_2 = comp_cmp_sum (make_pairs tvl) in
            ignore (TestVar.add_alias tv5 tv4);
            let sum_3 = comp_cmp_sum (make_pairs tvl) in
              (* now they should be equal *)
              assert_equal 
                ~printer: (string_of_int)
                16
                sum_1;
              assert_equal 
                ~printer: (string_of_int)
                12
                sum_2;
              assert_equal 
                ~printer: (string_of_int)
                0
                sum_3
    in
    let iset_cmp = Option.maybe_equal (fun t1 t2 -> ISet.compare t1 t2 = 0) in
    let iset_pri = Option.maybe_string ISet.string_of in
      
    let t7 () =
      let _,tv1,tv2,tv3,tv4,tv5 = create_tv () in
        ignore (TestVar.add_img (ISet.singleton 1) tv1);
        ignore (TestVar.add_img (ISet.singleton 2) tv2);
        let te = Some (ISet.add 2 (ISet.singleton 1)) in
          ignore (TestVar.add_alias tv1 tv2);
          let img_tv2 = TestVar.get_img tv2 in
            assert_equal 
              ~cmp: iset_cmp
              ~printer: iset_pri
              te
              img_tv2
    in
    let t8 () =
      let _,tv1,tv2,tv3,tv4,tv5 = create_tv () in
        
        ignore (TestVar.add_img (ISet.singleton 1) tv1);
        ignore (TestVar.add_img (ISet.singleton 2) tv2);
        ignore (TestVar.add_alias tv1 tv2);
        ignore (TestVar.add_alias tv2 tv3);
        ignore (TestVar.add_img (ISet.singleton 3) tv3);
        
        ignore (TestVar.add_img (ISet.singleton 4) tv4);
        ignore (TestVar.add_img (ISet.singleton 5) tv5);
        ignore (TestVar.add_alias tv4 tv5);
        let te123 = ISet.add 3 (ISet.add 2 (ISet.singleton 1))  in
        let te45 = ISet.add 4 (ISet.singleton 5) in
        let te12345 = ISet.merge te123 te45 in
          assert_equal 
            ~cmp: iset_cmp
            ~printer: iset_pri
            (Some te45)
            (TestVar.get_img tv5);
          TestVar.add_alias tv2 tv5;
          assert_equal 
            ~cmp: iset_cmp
            ~printer: iset_pri
            (Some te12345)
            (TestVar.get_img tv5)  
    in
      [("type var create test", t1);
       ("type var union find test", t2);
       ("type var union find test", t3);
       ("type var union find test", t4);
       ("type var union find test", t5);
       ("type var union find test", t6); 
       ("type var img tests", t7);
       ("type var img tests complex", t8);
      ]


  let _ =
    install_tests "TestVars" init_tests

end
