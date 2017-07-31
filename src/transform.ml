open Core

type t = Operation.edit_operation list

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
  | Insert(_), _         -> (List.Tail, List.Identity, List.Append(x'), List.Append(Operation.Retain(1)))
  | _, Insert(_)         -> (List.Identity, List.Tail, List.Append(Operation.Retain(1)), List.Append(y'))
  | Retain(a), Retain(b) ->
    if a > b then (List.Swap(Retain(a-b)), List.Tail, List.Append(y'), List.Append(y'))
    else if a < b then (List.Tail, List.Swap(Retain(b-a)), List.Append(x'), List.Append(x'))
    else (List.Tail, List.Tail, List.Append(x'), List.Append(x'))
  | Delete, Delete       -> (List.Tail, List.Tail, List.Identity, List.Identity)
  | Delete, Retain(1)    -> (List.Tail, List.Tail, List.Append(x'), List.Identity)
  | Delete, Retain(b)    -> (List.Tail, List.Swap(Retain(b-1)), List.Append(x'), List.Identity)
  | Retain(1), Delete    -> (List.Tail, List.Tail, List.Identity, List.Append(y'))
  | Retain(a), Delete    -> (List.Swap(Retain(a-1)), List.Tail, List.Identity, List.Append(y'))
  | _, _                 -> raise (Failure "Unreachable")

 let transform_operations x y =
  let rec transform_list a b (lhs, rhs) =
    match a, b with
    | [], [] -> (List.rev lhs, List.rev rhs)
    | _ ->
      let a_hd, b_hd = (List.hd a, List.hd b) in
      let x1, x2, x3, x4 = xform a_hd b_hd in
      transform_list (Operation.List.apply a x1) (Operation.List.apply b x2) ((Operation.List.apply lhs x3), (Operation.List.apply rhs x4))
  in
  let lhs, rhs = transform_list x y ([], []) in
  (compress lhs), (compress rhs)
