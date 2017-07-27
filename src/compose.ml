open Core

let compose x y =
  let x' = (Option.value x ~default:Operation.Empty) in
  let y' = (Option.value y ~default:Operation.Empty) in
  let open Operation in
  match x', y' with
  | Retain(a), Retain(b) ->
    if a > b then (List.Swap(Retain(a-b)), List.Tail, List.Append(y'))
    else if a < b then (List.Tail, List.Swap(Retain(b-a)), List.Append(x'))
    else (List.Tail, List.Tail, List.Append(x'))
  | Delete,    _         -> (List.Tail, List.Identity, List.Append(x'))
  | _,         Insert(_) -> (List.Identity, List.Tail, List.Append(y'))
  | Insert(_), Retain(1) -> (List.Tail, List.Tail, List.Append(x'))
  | Insert(_), Retain(b) -> (List.Tail, List.Swap(Retain(b-1)), List.Append(x'))
  | Insert(_), Delete    -> (List.Tail, List.Tail, List.Identity)
  | Retain(_), Delete    -> (List.Tail, List.Tail, List.Append(y'))
  | _, _                 -> raise (Failure "Unreachable")

 let compose_operations lista listb =
  let rec compose_list a b acc =
    match a, b with
    | [], [] -> acc
    | _ ->
      let a_hd, b_hd = (List.hd a, List.hd b) in
      let c1, c2, c3 = compose a_hd b_hd in
      compose_list (Operation.List.apply a c1) (Operation.List.apply b c2) (Operation.List.apply acc c3)
  in List.rev @@ compose_list lista listb []
