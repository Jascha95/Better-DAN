let maybe_tuppel ~f:f ~fa:f1 ~fb:f2 o1 o2 =
  match (o1,o2) with
    | None, None -> None
    | None, Some e2 -> Some (f2 e2)
    | Some e1,None -> Some (f1 e1)
    | Some e1,Some e2 -> Some (f e1 e2)

let maybe_cmp cmp o1 o2 =
  match (o1,o2) with
    | None, None -> 0
    | None, Some _ -> 1
    | Some _, None -> -1
    | Some o1, Some o2 -> cmp o1 o2

let maybe_equal cmp o1 o2 =
  match (o1,o2) with
    | None, None -> true
    | None, Some _ | Some _, None -> false
    | Some o1, Some o2 -> cmp o1 o2

let maybe_string string_of = function
  | None -> "none"
  | Some a -> "some " ^ string_of a

let bind ~f1:f1 ~f2:f2 o1 o2 =
  match f1 o1 with
    | Some false -> Some false
    | Some true -> f2 o2
    | None ->
        begin
          match f2 o2 with
            | Some false -> Some false
            | _ -> None
        end

let doo f = function
  | None -> None
  | Some a -> Some (f a)


let oand b1 = function
  | Some false -> Some false
  | None -> begin
      match b1 with
        | Some false -> Some false
        | _ -> None
    end
  | Some true -> begin
      match b1 with
        | Some false -> Some false
        | None -> None
        | _ -> Some true
    end

let oor b1 = function
  | Some true -> Some true
  | None -> begin
      match b1 with
        | Some true -> Some true
        | _ -> None
    end
  | Some false -> begin
      match b1 with
        | Some true -> Some true
        | None -> None
        | _ -> Some false
    end
      
let rec oands_priv acc = function
  | [] -> acc
  | h :: tail -> oands_priv (oand acc h) tail
let oands bl = oands_priv (Some true) bl
