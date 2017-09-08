open Core

type mutation = Retain of int | Insert of char | Delete | Empty

module type Action = sig
  type t
  type _ u
  val initial : 'a list u
  val instruct : t option -> t option -> t List_iterator.instruction * t List_iterator.instruction * t List_iterator.instruction u
  val accumulate : 'a list u -> 'a List_iterator.instruction u -> 'a list u
  val finalize : t list u -> t list u
end

module type Executor_intf = sig
  type t
  type _ u
  val reduce : t list -> t list -> t list u
end

module Executor(M : Action) : (Executor_intf with type t = M.t and type 'a u = 'a M.u) = struct
  type t = M.t
  type 'a u = 'a M.u

  let rec fold a b acc =
    match a, b with
    | [], [] -> M.finalize acc
    | _ ->
      let ia, ib, iacc = M.instruct (List.hd a) (List.hd b) in
      fold (List_iterator.step a ia) (List_iterator.step b ib) (M.accumulate acc iacc)

  let reduce a b = fold a b M.initial
end

module type OperationAction = Action with type t = mutation

module Compose : (OperationAction with type 'a u = 'a) = struct
  type t = mutation
  type 'a u = 'a

  let initial = []
  let finalize a = List.rev a
  let accumulate a ia = List_iterator.step a ia

  let instruct x y =
    let x' = (Option.value x ~default:Empty) in
    let y' = (Option.value y ~default:Empty) in
    let open List_iterator in
    match x', y' with
    | Retain(a), Retain(b) ->
      if a > b then           (Swap(Retain(a-b)), Tail, Append(y'))
      else if a < b then      (Tail, Swap(Retain(b-a)), Append(x'))
      else                    (Tail, Tail, Append(x'))
    | Delete,    _         -> (Tail, Identity, Append(x'))
    | _,         Insert(_) -> (Identity, Tail, Append(y'))
    | Insert(_), Retain(1) -> (Tail, Tail, Append(x'))
    | Insert(_), Retain(b) -> (Tail, Swap(Retain(b-1)), Append(x'))
    | Insert(_), Delete    -> (Tail, Tail, Identity)
    | Retain(_), Delete    -> (Tail, Tail, Append(y'))
    | _, _                 -> raise (Failure "Unreachable")
end
module ComposeExecutor = Executor(Compose)

module Transform : (OperationAction with type 'a u = 'a * 'a) = struct
  type t = mutation
  type 'a u = 'a * 'a

  let rev_compress l =
    let rec compress_list lst acc =
      match lst, acc with
      | [], _ -> acc
      | Retain(x) :: x_tl, Retain(y) :: y_tl -> compress_list x_tl (Retain(x + y) :: y_tl)
      | x :: tl, _ -> compress_list tl (x :: acc)
    in compress_list l []

  let initial = ([], [])
  let finalize (a, b) = rev_compress a, rev_compress b
  let accumulate (a, b) (ia, ib) = (List_iterator.step a ia), (List_iterator.step b ib)

  let instruct x y =
    let x' = (Option.value x ~default:Empty) in
    let y' = (Option.value y ~default:Empty) in
    let open List_iterator in
    match x', y' with
    | Insert(_), _         -> (Tail, Identity, (Append(x'), Append(Retain(1))))
    | _, Insert(_)         -> (Identity, Tail, (Append(Retain(1)), Append(y')))
    | Retain(a), Retain(b) ->
      if a > b then           (Swap(Retain(a-b)), Tail, (Append(y'), Append(y')))
      else if a < b then      (Tail, Swap(Retain(b-a)), (Append(x'), Append(x')))
      else                    (Tail, Tail, (Append(x'), Append(x')))
    | Delete, Delete       -> (Tail, Tail, (Identity, Identity))
    | Delete, Retain(1)    -> (Tail, Tail, (Append(x'), Identity))
    | Delete, Retain(b)    -> (Tail, Swap(Retain(b-1)), (Append(x'), Identity))
    | Retain(1), Delete    -> (Tail, Tail, (Identity, Append(y')))
    | Retain(a), Delete    -> (Swap(Retain(a-1)), Tail, (Identity, Append(y')))
    | _, _                 -> raise (Failure "Unreachable")
end
module TransformExecutor = Executor(Transform)
