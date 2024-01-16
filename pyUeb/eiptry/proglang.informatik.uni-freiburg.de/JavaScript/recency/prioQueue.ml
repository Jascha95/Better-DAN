open ProglangUtils

module type DATA = sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
  val string_of : t -> string
end

module type S = sig
  type t 
  type key 
  type data
  exception Empty
  val create : unit -> t
  val clear : t -> unit
  val is_empty : t -> bool
  val top : t -> data
  val pop : t -> data
  val push : key -> data -> t -> unit
  val string_of : t -> string
  val remove : data -> t -> unit
end

module Make(Ord: PrioQueues.ORD)(Data: DATA) = struct
  open ExtUtils
  module PQ = PrioQueues.Make(Ord)
  module HD = Hashtbl.Make(
    struct
      type t = Data.t
      let hash = Data.hash
      let equal = (Utils.compare_to_equal Data.compare)
    end)                  
  let find h e =
    try
      let t_remove,in_queue = HD.find h e in
        t_remove, in_queue
    with Not_found ->
      HD.add h e (0,false);
      0,false

  type t = 
      { qu: Data.t PQ.t;
        h: (int * bool) HD.t;
        mutable anz: int;
      }

  type key = Ord.t
  type data = Data.t
  exception Empty = PQ.Empty

  let create () =
    { qu = PQ.create ();
      h = HD.create 64;
      anz = 0;
    }
  let clear t = PQ.clear t.qu; HD.clear t.h; t.anz <- 0
      
  let is_empty {anz = a } = (a == 0)

  let rec top t = 
    let v = PQ.top t.qu in
    let to_remove,in_queue = find t.h v in
      if (to_remove > 0) then begin
        PQ.pop t.qu;
        HD.add t.h v (to_remove,in_queue);
        top t
      end else begin
        if (to_remove == 0) then
          v
        else
          failwith "internal error in prioQueue data structure!"
      end

  let rec pop t = 
    let v = PQ.pop t.qu in
    let to_remove,in_queue = find t.h v in
      if (to_remove > 0) then begin
        HD.add t.h v (to_remove - 1, in_queue);
        pop t
      end else begin
        if (to_remove == 0) then begin
          HD.add t.h v (0,false);
          t.anz <- t.anz - 1;
          v
        end else begin
          failwith "internal error in prioQeueue data structure!"
        end
      end

    
  let push key data t =
    let to_remove,in_queue = find t.h data in
      if (not in_queue) then begin
        PQ.push key data t.qu;
        HD.add t.h data (to_remove,true);
        t.anz <- t.anz + 1;
      end

      
  let string_of {qu = q} =
    PQ.string_of Data.string_of q

  let remove data t =
    let to_remove,in_queue = find t.h data in
      if in_queue then begin
        HD.add t.h data (to_remove + 1, false);
        t.anz <- t.anz - 1
      end
end

module PrioQueueTest = struct
  open Test
  open ExtUtils

  module PQ = Make(
    struct 
      type t = int
      let compare = (-)
      let string_of = string_of_int
    end)(
    struct 
      type t = int
      let compare = (-)
      let hash = Hashtbl.hash
      let string_of = string_of_int
    end)

  let init_tests () = 
    let assert_string s1 s2 = 
      assert_equal
        ~cmp:(Utils.compare_to_equal String.compare)
        ~printer:(fun x -> x)
        s1
        s2
    in
    let push_str i1 i2 pq =
      let _ = PQ.push i1 i2 pq in PQ.string_of pq 
    in
    let remove_str i1 pq =
      let _ = PQ.remove i1 pq in PQ.string_of pq
    in
    let pop_str pq =
      let r = PQ.pop pq in PQ.string_of pq,r
    in
      
    let t1 () = 
      let pq = PQ.create () in
      let s1 = PQ.string_of pq in
      let s2 = push_str 1 5 pq in 
      let s3 = push_str 1 8 pq in 
      let s4 = push_str 1 8 pq in 
      let s5,r1 = pop_str pq in
      let s6 = push_str 1 5 pq in
      let s7 = remove_str 8 pq in
      let s8,r2 = pop_str pq in
        assert_string "" s1;
        assert_string "1:[5]" s2;
        assert_string "1:[8;5]" s3;
        assert_string "1:[8;5]" s4;
        assert_string "1:[8]" s5;
        assert_equal 5 r1;
        assert_string "1:[5;8]" s6;
        assert_string "1:[5;8]" s7;
        assert_equal 5 r1;
        assert_string "" s8;
        
          
          ()
    in
      ["string_of test",t1]

  let _ =
    install_tests "Inf.Type" (init_tests) 


end
