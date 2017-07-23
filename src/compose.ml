open Core

type composition =
| Identity
| Tail
| Append of Operation.edit_operation
| Swap of Operation.edit_operation

let comp x y =
  let x' = (Option.value x ~default:Operation.Empty) in
  let y' = (Option.value y ~default:Operation.Empty) in
  let open Operation in
  match x', y' with
  | Retain(a), Retain(b) ->
    if a > b then (Swap(Retain(a-b)), Tail, Append(y'))
    else if a < b then (Tail, Swap(Retain(b-a)), Append(x'))
    else (Tail, Tail, Append(x'))
  | Delete,    _         -> (Tail, Identity, Append(x'))
  | _,         Insert(_) -> (Identity, Tail, Append(y'))
  | Insert(_), Retain(1) -> (Tail, Tail, Append(x'))
  | Insert(_), Retain(b) -> (Tail, Swap(Retain(b-1)), Append(x'))
  | Insert(_), Delete    -> (Tail, Tail, Identity)
  | Retain(_), Delete    -> (Tail, Tail, Append(y'))
  | _, _                 -> raise (Failure "Unreachable")

let apply_comp olist ocomp =
  match ocomp with
  | Identity -> olist
  | Append(a) -> a :: olist
  | Swap(a) -> a :: (List.tl_exn olist)
  | Tail -> List.tl_exn olist

let compose_operations lista listb =
  if (List.map ~f:Operation.size lista) = (List.map ~f:Operation.size listb) then
    let rec compose_list a b acc =
      match a, b with
      | [], [] -> acc
      | _ ->
        let a_hd, b_hd = (List.hd a, List.hd b) in
        let c1, c2, c3 = comp a_hd b_hd in
        compose_list (apply_comp a c1) (apply_comp b c2) (apply_comp acc c3)
    in List.rev @@ compose_list lista listb []
  else raise (Failure "Operation lists must be of same length")
