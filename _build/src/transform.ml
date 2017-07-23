open Core

type application =
| Identity
| Tail
| Append of Operation.edit_operation
| Swap of Operation.edit_operation

let apply_comp olist ocomp =
  match ocomp with
  | Identity -> olist
  | Append(a) -> a :: olist
  | Swap(a) -> a :: (List.tl_exn olist)
  | Tail -> List.tl_exn olist

let compress l =
  let rec compress_list lst acc =
    let open Operation in
    match lst, acc with
    | [], _ -> acc
    | Retain(x)::x_tl, Retain(y)::y_tl -> compress_list x_tl (Retain(x+y) :: y_tl)
    | x::tl, _ -> compress_list tl (x :: acc)
  in List.rev @@ compress_list l []

let xform x y =
  let x' = (Option.value x ~default:Operation.Empty) in
  let y' = (Option.value y ~default:Operation.Empty) in
  let open Operation in
  match x', y' with
  | Insert(_), _         -> (Tail, Identity, Append(x'), Append(Operation.Retain(1)))
  | _, Insert(_)         -> (Identity, Tail, Append(Operation.Retain(1)), Append(y'))
  | Retain(a), Retain(b) ->
    if a > b then (Swap(Retain(a-b)), Tail, Append(y'), Append(y'))
    else if a < b then (Tail, Swap(Retain(b-a)), Append(x'), Append(x'))
    else (Tail, Tail, Append(x'), Append(x'))
  | Delete, Delete       -> (Tail, Tail, Identity, Identity)
  | Delete, Retain(1)    -> (Tail, Tail, Append(x'), Identity)
  | Delete, Retain(b)    -> (Tail, Swap(Retain(b-1)), Append(x'), Identity)
  | Retain(1), Delete    -> (Tail, Tail, Identity, Append(y'))
  | Retain(a), Delete    -> (Swap(Retain(a-1)), Tail, Identity, Append(y'))
  | _, _                 -> raise (Failure "Unreachable")

let transform_operations x y =
  let rec transform_list a b (lhs, rhs) =
    match a, b with
    | [], [] -> (List.rev lhs, List.rev rhs)
    | _ ->
      let a_hd, b_hd = (List.hd a, List.hd b) in
      let x1, x2, x3, x4 = xform a_hd b_hd in
      transform_list (apply_comp a x1) (apply_comp b x2) ((apply_comp lhs x3), (apply_comp rhs x4))
  in
  let lhs, rhs = transform_list x y ([], []) in
  (compress lhs), (compress rhs)
