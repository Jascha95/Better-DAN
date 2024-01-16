let rec inc_list_list l = 
  let rec inc_list = function
      []   -> []
    | x::l -> x+1 :: inc_list l
  in
    match l with
        []   -> []
      | x::l -> inc_list x :: inc_list_list l

let inc_list_list_with_map = List.map (List.map ((+) 1))


let rec concat sep = function
    [] -> ""
  | x::[] -> x
  | x::xs -> x ^ sep ^ concat sep xs

let rec concat_with_fold sep l =
  let f s = function
      None    -> Some s
    | Some s' -> Some (s ^ sep ^ s')
  in 
    match List.fold_right f l None with
        None   -> ""
      | Some s -> s

type ('a, 'b) tree = Node of ('a * 'b * ('a, 'b) tree * ('a, 'b) tree) 
                   | Leaf

let rec insert k v = function
    Leaf -> 
      Node (k, v, Leaf, Leaf)
  | Node (k', v', l, r) ->
      if k < k' then Node (k', v', insert k v l, r)
      else if k > k' then Node (k', v', l, insert k v r)
      else Node (k, v, l, r)

let rec find k = function
    Leaf -> None
  | Node (k', v, l, r) ->
      if k < k' then find k l
      else if k > k' then find k r
      else Some v


type op = Add | Sub | Mul | Div
type expr = Number of int | Binop of (expr * op * expr)

let t1 = Binop (Number 4, Add, 
                Binop (Number 6, Mul, Binop (Number 8, Sub, Number 3)))

let t2 = Binop (Binop (Number 4, Add, Number 6), Div, 
                Binop (Number 4, Sub, Number 4))

let fun_of_binop = function
    Add -> (+)
  | Sub -> (-)
  | Mul -> ( * )
  | Div -> (/)
      
let rec eval = function
      Number i         -> i
    | Binop (i, op, j) -> fun_of_binop op (eval i) (eval j)

let rec eval' = function
      Number i         -> Some i
    | Binop (i, op, j) -> 
        match eval' i with
            None   -> None
          | Some l ->
              match eval' j with
                  None   -> None
                | Some r -> 
                    if r = 0 && op = Div then None
                    else Some (fun_of_binop op l r)

type expr' = Number' of int | Binop' of (expr' * op * expr')
           | Var of string | Let of (string * expr' * expr')

let t3 = Let ("x", Binop' (Number' 4, Add, Number' 5),
              Let ("y", Binop' (Number' 2, Mul, Number' 3),
                   Binop' (Var "x", Add, Binop' (Var "y", Add, Number' 3))))


let rec eval'' env = function
    Number' i ->
      Some i
  | Var s ->
      find s env
  | Let (s, e1, e2) ->
      (match eval'' env e1 with
           None -> None
         | Some i -> eval'' (insert s i env) e2)
  | Binop' (i, op, j) -> 
      match eval'' env i with
          None   -> None
        | Some l ->
            match eval'' env j with
                None   -> None
                | Some r -> 
                    if r = 0 && op = Div then None
                    else Some (fun_of_binop op l r)

