open Core

(* type composition =
| Identity
| Tail
| Append of Operation.edit_operation
| Swap of Operation.edit_operation *)

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

(* let apply_comp olist ocomp =
  match ocomp with
  | Identity -> olist
  | Append(a) -> a :: olist
  | Swap(a) -> a :: (List.tl_exn olist)
  | Tail -> List.tl_exn olist *)

let compose_operations lista listb =
  Operation.List.cross lista listb compose

(* let compose_operations lista listb =
  if (List.map ~f:Operation.size lista) = (List.map ~f:Operation.size listb) then
    let rec compose_list a b acc =
      match a, b with
      | [], [] -> acc
      | _ ->
        let a_hd, b_hd = (List.hd a, List.hd b) in
        let c1, c2, c3 = comp a_hd b_hd in
        compose_list (apply_comp a c1) (apply_comp b c2) (apply_comp acc c3)
    in List.rev @@ compose_list lista listb []
  else raise (Failure "Operation lists must be of same length") *)
