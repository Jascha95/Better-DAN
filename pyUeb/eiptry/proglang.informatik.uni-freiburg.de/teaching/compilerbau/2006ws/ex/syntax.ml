type id = string

type 'e equation = Equation of id      (* function name *) 
                             * id list (* arguments *)
                             * 'e      (* body *)
(* Source language *)

type exp = Var of id
         | App of exp * exp                     (* function application *)
         | Letvar of id * exp * exp             (* let x = 5 in e *)
         | Letrec of (exp equation) list * exp  (* letrec f_1 x_1 ... x_n1 = e_m
                                                          ...
                                                          f_m x_1 ... x_nm = e_m
                                                   in e *)


(* Target language *)

type scheme = Scheme of (exp' equation) list * exp'

and exp' = Var' of id
         | App' of exp' * exp'
         | Letvar' of id * exp' * exp'

