(** Some Utils *)

module Utils = struct

  type os_type = Win32 | Unix | Cygwin
  exception Error of string

  let get_os_type =
    match Sys.os_type with
      | "Win32" -> Win32
      | "Cygwin" -> Cygwin
      | "Unix" -> Unix
      | other -> Log.fatal ("Unknown OS: " ^ other)


  let replicate n x =
    let rec f n acc =
      if n <= 0 then acc
      else f (n-1) (x::acc)
    in
      f n []


  let comp t1 t2 = 
    if (t1 = t2) then 0
    else (if (t1 < t2) then -1 else 1)
      
  let circ f g =
    fun x -> f (g x)
      
  let rec fast_concat ll =
    match ll with
	    [] -> []
      | h :: t -> List.rev_append h (fast_concat t)


  type 'a cfun = ('a -> 'a -> int)
  let compare_2_tup : 'a cfun * 'b cfun -> 'a * 'b -> 'a * 'b -> int 
    = 
    fun (f1,f2) (a1,b1) (a2,b2) ->
      let res = f1 a1 a2 in
        if res <> 0 then 
          res 
        else 
          f2 b1 b2
            
  let compare_3_tup : 'a cfun * 'b cfun * 'c cfun 
      -> 'a * 'b * 'c -> 'a * 'b * 'c -> int
    = fun (f1,f2,f3) (a1,b1,c1) (a2,b2,c2) ->
      let res = f1 a1 a2 in
        if res <> 0 then 
          res 
        else 
          compare_2_tup (f2,f3) (b1,c1) (b2,c2)
            

  let compare_4_tup : 'a cfun * 'b cfun * 'c cfun * 'd cfun 
      -> 'a * 'b * 'c * 'd -> 'a * 'b * 'c * 'd -> int
    = fun  (f1,f2,f3,f4) (a1,b1,c1,d1) (a2,b2,c2,d2) ->
      let res = f1 a1 a2 in
        if res <> 0 then 
          res 
        else 
          compare_3_tup (f2,f3,f4) (b1,c1,d1) (b2,c2,d2)


  let read_opt ?(none_func=fun () -> assert false) = function
      Some x -> x
    | None -> none_func ()

  let lazy_sprintf str fmt =
    lazy (Printf.ksprintf (fun msg -> msg) str fmt)

  let rec fixpoint f x =
    let y = f x in
      if x = y 
      then y
      else fixpoint f y

  let prune_string len str =
    let l = String.length str in
      if len < 4 || l <= len
      then str
      else
        let start_len = len / 2 + len mod 2 - 2 in
        let end_len = len / 2 - 2 in
          String.sub str 0 start_len ^ " .. " ^ String.sub str (l - end_len) end_len

  let compare_to_equal f a b =
    if f a b == 0 then true else false

  let flip f x y = f y x

  let apply f x = f x
  let apply' x f = f x

  let (<.<) i f =
    if i <> 0 then i else f ()

end
