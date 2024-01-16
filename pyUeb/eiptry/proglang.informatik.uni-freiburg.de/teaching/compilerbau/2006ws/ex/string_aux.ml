let list_of_string s =
  let res = ref [] in
    String.iter (fun c -> res := c :: !res) s; List.rev (!res)

  
