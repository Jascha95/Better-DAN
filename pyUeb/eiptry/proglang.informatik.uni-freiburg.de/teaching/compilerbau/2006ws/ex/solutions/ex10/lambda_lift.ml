open Syntax

let equation_id (Equation (x, _, _)) = x
let equation_body (Equation (_, _, e)) = e
let equation_args (Equation (_, args, _)) = args

module IdSet =
struct
  include Set.Make (struct type t = id let compare = String.compare end)
  let add_list l s = List.fold_right add l s
  let from_list l = add_list l empty
  let union_list l = List.fold_right union l empty
end

let rec free_vars = function
  | Var x -> IdSet.singleton x
  | App (e1, e2) -> IdSet.union (free_vars e1) (free_vars e2)
  | Letvar (x, e1, e2) ->
      IdSet.union (free_vars e1) (IdSet.remove x (free_vars e2))
  | Letrec (eqs, e) ->
      let ids = List.map equation_id eqs in
      let free_body = free_vars_equation_bodies eqs in
      let free_e = free_vars e in
        IdSet.union (IdSet.diff free_body (IdSet.from_list ids)) free_e

and free_vars_equation_bodies eqs =
  IdSet.union_list (List.map (fun eq -> free_vars (equation_body eq)) eqs)

let extend_subst s l = s @ l

let app e0 args = List.fold_left (fun e1 e2 -> App (e1, e2)) e0 args

let rec prepare_lifting letrec_bound subst = function
  | Var x -> (Var x, subst)
  | App (e1, e2) -> 
      let (e1', subst1) = prepare_lifting letrec_bound subst e1 in
      let (e2', subst2) = prepare_lifting letrec_bound subst1 e2 in
        (App (e1', e2'), subst2)
  | Letvar (x, e1, e2) ->
      let (e1', subst1) = prepare_lifting letrec_bound subst e1 in
      let (e2', subst2) = prepare_lifting letrec_bound subst1 e2 in
        (Letvar (x, e1' , e2'), subst2)
  | Letrec (eqs, e) ->
      let ids = List.map equation_id eqs in
      let letrec_bound' = IdSet.add_list ids letrec_bound in
      let free = 
        IdSet.diff 
          (free_vars_equation_bodies eqs) 
          (IdSet.add_list 
             (List.flatten (List.map equation_args eqs)) 
             letrec_bound') 
      in
      let ym = IdSet.elements free in
      let (eqs', subst') = 
        List.fold_right 
          (fun (Equation (f, args, e)) (eqs, subst) ->
             let (e', subst') = prepare_lifting letrec_bound' subst e in
               (Equation (f, ym@args, e') :: eqs, subst'))
          eqs
          ([], subst)
      in
      let (e', subst'') = prepare_lifting letrec_bound' subst' e in
      let subst''' = 
        extend_subst 
          subst'' 
          (List.map 
             (fun f -> (f, app (Var f) (List.map (fun i -> Var i) ym)))
             ids)
      in
        (Letrec (eqs', e'), subst''')

let empty_subst = []

let rec apply_subst subst = function
  | Var x ->
      (try List.assoc x subst with Not_found -> Var x)
  | App (e1, e2) ->
      App (apply_subst subst e1, apply_subst subst e2)
  | Letvar (x, e1, e2) ->
      (* no need to avoid capture because variables are unique! *)
      Letvar (x, apply_subst subst e1, apply_subst subst e2)
  | Letrec (eqs, e) ->
      let eqs' = List.map (fun (Equation (f, args, e)) -> 
                             Equation (f, args, apply_subst subst e)) eqs in
      let e' = apply_subst subst e in
        Letrec (eqs', e')

let do_lifting e =
  let rec do_lifting' = function
    | Var x -> 
        ([], Var' x)
    | App (e1, e2) ->
        let (eqs1, e1') = do_lifting' e1 in
        let (eqs2, e2') = do_lifting' e2 in
          (eqs1 @ eqs2, App' (e1', e2'))
    | Letvar (x, e1, e2) ->
        let (eqs1, e1') = do_lifting' e1 in
        let (eqs2, e2') = do_lifting' e2 in
          (eqs1 @ eqs2, Letvar' (x, e1', e2'))
    | Letrec (eqs, e) ->
        let eqs' =
          List.fold_right
            (fun (Equation (f, args, e)) eqs ->
               let (eqs', e') = do_lifting' e in
                 Equation (f, args, e') :: (eqs @ eqs'))
            eqs
            []
        in
        let (eqs'', e') = do_lifting' e in
          (eqs' @ eqs'', e')
  in
  let (eqs, e') = do_lifting' e in
    Scheme (eqs, e')

let lift e = 
  let (e', subst) = prepare_lifting IdSet.empty empty_subst e in
  let e'' = apply_subst subst e' in
    do_lifting e''
