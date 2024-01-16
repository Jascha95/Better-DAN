let product l = 
  let rec aux l c =
    match l with
        [] -> c 1
      | (x::xs) -> if x = 0 then 0 else aux xs (fun y -> c (x * y))
  in aux l (fun x -> x)
