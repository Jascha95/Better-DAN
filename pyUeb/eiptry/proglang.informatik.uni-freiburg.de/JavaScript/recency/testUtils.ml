open OUnit

module Make(R: sig val reset : unit -> unit end) = 
struct
  let wrap_reset f x =
    R.reset ();
    let result = f x in
      R.reset ();
      result
        
        
  let wrap_list l = 
    List.map
      (fun (s,f) -> (s,wrap_reset f))
      l
end
