open Syntax
open Environment

type value =
    VInt of int
  | VFun of (value list -> (value -> value) -> value)
  | VUnit
  | VCons of (value * value)
  | VNil

let rec string_of_value v =
  match v with
      VInt i -> string_of_int i
    | VUnit -> "()"
    | VFun _ -> "<fun>"
    | VCons (v1, v2) -> ("VCons (" ^ string_of_value v1 ^ ", " ^
                           string_of_value v2 ^ ")")
    | VNil -> "nil"

let exInt v =
  match v with 
      VInt i -> i
    | _ -> failwith (string_of_value v ^ " is not an int")

let exFun v =
  match v with
      VFun f -> f
    | _ -> failwith (string_of_value v ^ " is not a function")

let eval_builtin f vs c = 
  match (f, vs) with
      ("+", [v1; v2]) -> c (VInt (exInt v1 + exInt v2))
    | ("*", [v1; v2]) -> c (VInt (exInt v1 * exInt v2))
    | ("==", [v1; v2]) -> 
        if v1 = v2 then c (VInt 1) else c (VInt 0)
    | ("print", [v]) -> print_endline (string_of_value v); c VUnit
    | ("call/cc", [v]) ->
        let cc = VFun (fun vs _ -> 
                         (match vs with
                              [v] -> c v
                            | _ -> failwith ("continuation applied to wrong " ^
                                               "number of arguments")))
        in (exFun v) [cc] c
    | ("cons", [v1;v2]) -> c (VCons (v1, v2))
    | ("nil", []) -> c VNil
    | ("null", [v]) -> 
        (match v with
            VNil -> c (VInt 1)
          | _ -> c (VInt 0))
    | ("head", [v]) ->
        (match v with
             VCons (v1,v2) -> c v1
           | VNil -> failwith ("head: empty list")
           | _ -> failwith ("head: wrong type"))
    | ("tail", [v]) ->
        (match v with
             VCons (v1,v2) -> c v2
           | VNil -> failwith ("tail: empty list")
           | _ -> failwith ("tail: wrong type"))
    | (f, _) ->
        failwith ("unknown builtin " ^ f ^ " or wrong number of arguments")

let rec eval e env c =
  match e with
      Int (i) -> 
        c (VInt (i))
    | Builtin (f, es) ->
        evals es env (function vs -> eval_builtin f vs c)
    | If (e1, e2, e3) ->
        eval e1 env (function v1 ->
                       if exInt v1 <> 0
                       then eval e2 env c
                       else eval e3 env c)
    | Ident (x) ->
        c (lookup x env)
    | Let (x, e1, e2) ->
        eval e1 env (function v1 ->
                       eval e2 (extend env x v1) c)
    | Function (x0, xs, e) ->
        let rec f = (VFun
                       (function vs ->
                          if List.length vs <> List.length xs then
                            failwith ("Function " ^ x0 ^ 
                                        " called with wrong number of arguments")
                          else
                            eval e (extends env (x0::xs) (f::vs))))
        in c f
    | Apply (e0, es) ->
        eval e0 env (function v0 ->
                       evals es env (function vs -> (exFun v0) vs c))
        

and evals es env c =
  match es with
      [] ->
        c []
    | e :: es ->
        eval e env (function v ->
                      evals es env (function vs ->
                                         c (v :: vs)))

let top_eval e = string_of_value (eval e [] (fun x -> x))
