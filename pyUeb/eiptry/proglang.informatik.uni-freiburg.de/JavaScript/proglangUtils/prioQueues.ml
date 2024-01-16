open ExtSQueue

module type ORD = sig
  include Map.OrderedType

  val string_of : t -> string
end

module type S = sig
  type 'a t
  type key 

  exception Empty

  val create : unit -> 'a t
  val clear : 'a t -> unit
  val is_empty : 'a t -> bool

  val top : 'a t -> 'a 
  val pop : 'a t -> 'a 

  val push : key -> 'a -> 'a t -> unit

  val string_of : ('a -> string) -> 'a t -> string
end

module Make : functor (Ord: ORD) -> S 
  with type key = Ord.t
=
  functor(Ord: ORD) ->
struct
  open ExtSQueue

  type key = Ord.t
  type 'a t = (key * 'a Queue.t) list ref
  exception Empty
      
  let create () = ref []
  let clear q = q := []

  let is_empty pq = match !pq with
    | [] -> true
    | _ -> false

  let push key data pq = 
    let rec aux acc = function 
      | [] -> 
          let q = Queue.create () in
          let _ = Queue.add data q in
            List.rev ((key,q) :: acc)
      | ((hkey,hq) :: t) as l ->
          let c = Ord.compare hkey key in
            if (c == 0) then begin
              Queue.add data hq;
              List.rev_append acc l
            end else begin
              if (c > 0) then begin
                let q = Queue.create () in
                let _ = Queue.add data q in
                List.rev_append acc ((key,q) :: l)
              end else begin
                aux ((hkey,hq) :: acc) t
              end
            end
    in
    let pq_new = aux [] !pq in
      pq := pq_new
            

  let top pq = match !pq with
    | [] -> raise Empty
    | (min,q) :: _ ->
        Queue.top q

  let pop pq = match !pq with
    | [] -> raise Empty
    | (min,q) :: ql ->
        let e = Queue.pop q in
          if Queue.is_empty q then
            pq := ql;
          e

  let string_of str pq = 
    String.concat
      ";\n"
      (List.map
         (fun (min,q) ->
            let mins = Ord.string_of min in
            let qs = Queue.string_of str q in
              mins^":"^qs)
         !pq)

end


module PrioQueuesTest = struct
  open Test
  open ExtUtils

  module PQ = Make(
    struct
      type t = int
      let compare = (-)
      let string_of = string_of_int
    end)
    
  let init_tests () = 
    let assert_string s1 s2 =
      assert_equal
        ~cmp:(Utils.compare_to_equal String.compare)
        ~printer:(fun s -> s)
        s1
        s2
    in
    let assert_int i1 i2 = 
      assert_equal
        ~cmp:(Utils.compare_to_equal (-))
        ~printer:string_of_int
        i1
        i2
    in
        
    let t1 () = 
      let q = PQ.create () in
        assert_equal (PQ.is_empty q) true;
        assert_string
          ""
          (PQ.string_of string_of_int q);
        (try
          let _ = PQ.pop q in
            ()
        with e ->
          begin
            match e with
              | PQ.Empty -> ()
              | _ -> assert_failure "Pop should throw an Empty error";
          end);
        let _ = PQ.push 3 5 q in
        assert_string
          "3:[5]"
          (PQ.string_of string_of_int q);
        let _ = PQ.push 5 9 q in
        assert_string
          "3:[5];\n5:[9]"
          (PQ.string_of string_of_int q);
        let _ = PQ.push 3 8 q in
        assert_string
          "3:[8;5];\n5:[9]"
          (PQ.string_of string_of_int q);
        let t1 = PQ.top q in
        let t2 = PQ.pop q in
        let t3 = PQ.pop q in
        let t4 = PQ.pop q in
          
          assert_equal 5 t1;
          assert_equal 5 t2;
          assert_equal 8 t3;
          assert_equal 9 t4;
    in
    let t2 () =
      let q = PQ.create () in
        PQ.push 2 5 q;
        PQ.push 2 8 q;
        PQ.push 2 3 q;
        PQ.push 5 1 q;
        let t1 = PQ.pop q in
        let t2 = PQ.pop q in
          PQ.push 2 9 q;
          PQ.push 4 7 q;
          PQ.push 8 2 q;
          assert_string 
            "2:[9;3];\n4:[7];\n5:[1];\n8:[2]"
            (PQ.string_of string_of_int q);
          let t3 = PQ.pop q in
          let t4 = PQ.pop q in
          let t5 = PQ.pop q in
          let t6 = PQ.pop q in
          let t7 = PQ.pop q in
            assert_int 5 t1;
            assert_int 8 t2;
            assert_int 3 t3;
            assert_int 9 t4;
            assert_int 7 t5;
            assert_int 1 t6;
            assert_int 2 t7
    in
      
      [("init test, push, pop, is_empty", t1);
       ("push, pop test", t2);
      ]

  let _ = install_tests "PrioQueues" init_tests

end
