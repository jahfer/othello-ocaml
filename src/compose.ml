open Core
module OList = Operation.List
type t = Operation.edit_operation list

let compose x y =
  let x' = (Option.value x ~default:Operation.Empty) in
  let y' = (Option.value y ~default:Operation.Empty) in
  let open Operation in
  match x', y' with
  | Retain(a), Retain(b) ->
    if a > b then           OList.(Swap(Retain(a-b)), Tail, Append(y'))
    else if a < b then      OList.(Tail, Swap(Retain(b-a)), Append(x'))
    else                    OList.(Tail, Tail, Append(x'))
  | Delete,    _         -> OList.(Tail, Identity, Append(x'))
  | _,         Insert(_) -> OList.(Identity, Tail, Append(y'))
  | Insert(_), Retain(1) -> OList.(Tail, Tail, Append(x'))
  | Insert(_), Retain(b) -> OList.(Tail, Swap(Retain(b-1)), Append(x'))
  | Insert(_), Delete    -> OList.(Tail, Tail, Identity)
  | Retain(_), Delete    -> OList.(Tail, Tail, Append(y'))
  | _, _                 -> raise (Failure "Unreachable")

 let compose_operations lista listb =
  let rec compose_list a b acc =
    match a, b with
    | [], [] -> acc
    | _ ->
      let a_hd, b_hd = (List.hd a, List.hd b) in
      let c1, c2, c3 = compose a_hd b_hd in
      compose_list (OList.apply a c1) (OList.apply b c2) (OList.apply acc c3)
  in List.rev @@ compose_list lista listb []
