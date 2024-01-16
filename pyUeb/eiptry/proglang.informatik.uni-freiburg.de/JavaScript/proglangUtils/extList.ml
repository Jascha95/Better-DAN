open ExtUtils

module List = struct
  include List

  let is_empty = function
    | [] -> true
    | _ -> false

  let find' f l =
    try
      Some (find f l)
    with
        Not_found -> None

  let rec take n l = 
    if n = 0 then [] 
    else
      if n < 0 then raise (Invalid_argument ("Utils.take: negative argument"))
      else
        match l with
          | [] -> []
          | x::xs -> x :: take (n-1) xs

    
  let rec compare cmp l1 l2 =
    match (l1,l2) with
      | [],[] -> 0
      | e1 :: l1, e2 :: l2 -> 
          let i = cmp e1 e2 in
            if i == 0 then
              compare cmp l1 l2
            else
              i
      | _ :: _, [] -> 1
      | [], _ :: _ -> -1
          


  let mapi (f : int -> 'a -> 'b) (l : 'a list) : 'b list =
    let folder (sofar, n) x = f n x :: sofar, succ n in
      List.rev (fst (List.fold_left folder ([], 0) l))
        
  let select pred l =
    let rec aux acc = function
      | [] -> List.rev acc, []
      | h :: t when pred h ->
          aux (h :: acc) t
      | l -> List.rev acc, l 
    in
      aux [] l

  let rec mem_cmp cmp a = function
    | [] -> false
    | e :: l -> if cmp a e then true else mem_cmp cmp a l
    
  let add e l =
    if (List.mem e l) then l
    else e :: l
  let union l1 l2 = fold_right add l1 l2
  let remove elm l = 
    let rec remove' elm l acc =
      match l with
        | [] -> rev acc
        | a :: l ->
          if (a == elm) then
            List.rev_append acc l
          else
            remove' elm l (a :: acc)
    in
      remove' elm l []
  let diff l1 l2=
    List.fold_left
      (Utils.flip remove) 
      l1 
      l2

  let inter l1 l2 =
    let rec i' acc l1 = function
      | [] -> rev acc
      | e :: l2 -> 
          if mem e l1 then
            i' (e :: acc) l1 l2
          else
            i' acc l1 l2
    in
      i' [] l1 l2 


  let rec rem acc eq e = function
    | [] -> false,List.rev acc
    | h :: t -> 
        if eq h e then
          true,rev_append acc t
        else
          rem (h :: acc) eq e t

  let rec compare_ignore_order ?(equal = Pervasives.(=)) l1 = function
    | [] -> if is_empty l1 then true else false
    | h :: t ->
        let b,nl = rem [] equal h l1 in
          if (not b) then
            false
          else
            compare_ignore_order ~equal:equal nl t

  let rec subset ?(equal = Pervasives.(=)) l1 l2 = match l1,l2 with
    | [], _ -> true
    | a :: l, l2 -> 
        if mem_cmp equal a l2 then
          subset ~equal:equal l l2
        else
          false
    
  let maybe_forall f l = 
    let rec aux acc = function
      | [] -> acc
      | h :: t -> 
          begin
            match f h with
              | None -> aux None t
              | Some false -> Some false
              | Some true -> aux acc t
          end
    in
      aux (Some true) l

  let maybe_exists f l =
    let rec aux acc = function
      | [] -> acc
      | h :: t ->
          begin
            match f h with
              | None -> aux None t
              | Some false -> aux acc t
              | Some true -> Some true
          end
    in
      aux (Some false) l


  let rec assoc' ?(equal = (=)) a = function
    | [] -> None
    | (a1,b1) :: tail ->
        if equal a1 a then Some b1 else assoc' a tail

end


module TestExtList = struct
  open Test

  let init_tests () = 
    let t1 () = 
      let l1 = [1;2;3] in
      let l2 = [1;5;8;2;13;-5] in
      let l1' = List.remove 1 l1 in
      let l2' = List.remove 8 l2 in
      let l2'' = List.remove (-5) l2 in
      let l2''' = List.remove 0 l2 in
      let l3 = [1;5;1;8;2;13;-5] in
      let l3' = List.remove 1 l3 in
      let l4 = [] in
      let l4' = List.remove 0 l4 in
        assert_equal 
          l1'
          [2;3];
        assert_equal 
          l2'
          [1;5;2;13;-5];
        assert_equal 
          l2''
          [1;5;8;2;13];
        assert_equal 
          l2'''
          [1;5;8;2;13;-5];
        assert_equal 
          l3'
          [5;1;8;2;13;-5];
        assert_equal 
          l4'
          []
    in
    let t2 () =
      let l1 = [1;2;3;4;5;6;7;8] in
      let l2 = [1;5;-5] in
      let l1' = List.diff l1 l2 in
      let l1'' = List.diff l1 [] in
      let l1''' = List.diff l2 l1 in
        assert_equal
          l1'
          [2;3;4;6;7;8];
        assert_equal
          l1''
          l1;
        assert_equal
          l1'''
          [-5]
    in
    let t3 () =
      let l1 = [1;2;3;4;5;6;7;8] in
      let l2 = [1;2;4;10;3] in
      let l1' = List.inter l1 l2 in
      let l1'' = List.inter l2 l1 in
        assert_equal
          l1'
          [1;2;4;3];
        assert_equal
          l1''
          [1;2;3;4];
        assert_equal
          (List.inter l1 [])
          [];
        assert_equal
          (List.inter [] l1)
          []

    in

    let t4 () =
      let l1 = [1;2;3;5] in
      let l2 = [1;5;3;2] in
      let l3 = [0;1;5;3;2] in
      let l4 = [0;0;1;5;3;2] in
      let l5 = [1;5;3;2;0;0] in
      let b1 = List.compare_ignore_order l1 l2 in
      let b2 = List.compare_ignore_order l3 l4 in
      let b3 = List.compare_ignore_order l1 l4 in
      let b4 = List.compare_ignore_order l4 l5 in
      let b5 = List.compare_ignore_order l3 l5 in
        assert_equal
          b1
          true;
        assert_equal 
          b2 
          false;
        assert_equal 
          b3 
          false;
        assert_equal 
          b5 
          false;
        assert_equal 
          b4
          true
    in
    let t5 () =
      let l1 = [1;2;3;4] in
      let l2 = [1;3;2;4;5;6;7;0] in
      let l3 = [4;1] in
      let l4 = [1;1;3;3;3;3;3] in
        assert_equal
          (List.subset l1 l2)
          true;
        assert_equal
          (List.subset l2 l1)
          false;
        assert_equal
          (List.subset l3 l2)
          true;
        assert_equal
          (List.subset l3 l1)
          true;
        assert_equal 
          (List.subset [] [])
          true;
        assert_equal 
          (List.subset [] l3)
          true;
        assert_equal 
          (List.subset [] l2)
          true;
        assert_equal 
          (List.subset [] l1)
          true;
        assert_equal 
          (List.subset l4 l1)
          true
        

    in
      [("remove test", t1);
       ("diff test", t2);
       ("inter test", t3);
       ("compare_ignore_order test", t4);
       ("subset test",t5);
      ]

  let _ = install_tests "ExtList" init_tests

end
