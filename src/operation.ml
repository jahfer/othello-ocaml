open Core

type edit_operation = Retain of int | Insert of char | Delete | Empty

type 'a list_application =
  | Identity
  | Tail
  | Append of 'a
  | Swap of 'a

let apply l a =
  match a with
  | Identity -> l
  | Append(x) -> x :: l
  | Swap(x) -> x :: (List.tl_exn l)
  | Tail -> List.tl_exn l

module type Action = sig
  type t = edit_operation
  type 'a u = 'a list_application
  type _ v
  type _ w
  val initial : 'a v
  val instruct : t option -> t option -> t u * t u * t w
  val accumulate : 'a v -> 'a w -> 'a v
  val finalize : t v -> t v
end

module type Executor_intf = sig
  type t
  type _ v
  val reduce : t list -> t list -> t v
end

module Executor(M : Action) : (Executor_intf with type t = M.t and type 'a v = 'a M.v) = struct
  type t = M.t
  type 'a v = 'a M.v

  let rec fold a b acc =
    match a, b with
    | [], [] -> M.finalize acc
    | _ ->
      let ia, ib, iacc = M.instruct (List.hd a) (List.hd b) in
      fold (apply a ia) (apply b ib) (M.accumulate acc iacc)

  let reduce a b = fold a b M.initial
end

module Compose : (Action with type 'a v = 'a list) = struct
  type t = edit_operation
  type 'a u = 'a list_application
  type 'a v = 'a list
  type 'a w = 'a list_application

  let initial = []
  let finalize a = List.rev a
  let accumulate a ia = apply a ia

  let instruct x y =
    let x' = (Option.value x ~default:Empty) in
    let y' = (Option.value y ~default:Empty) in
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

module Transform : (Action with type 'a v = 'a list * 'a list) = struct
  type t = edit_operation
  type 'a u = 'a list_application
  type 'a v = 'a list * 'a list
  type 'a w = 'a list_application * 'a list_application

  let rev_compress l =
    let rec compress_list lst acc =
      match lst, acc with
      | [], _ -> acc
      | Retain(x) :: x_tl, Retain(y) :: y_tl -> compress_list x_tl (Retain(x + y) :: y_tl)
      | x :: tl, _ -> compress_list tl (x :: acc)
    in compress_list l []

  let initial = ([], [])
  let finalize (a, b) = rev_compress a, rev_compress b
  let accumulate (a, b) (ia, ib) = (apply a ia), (apply b ib)

  let instruct x y =
    let x' = (Option.value x ~default:Empty) in
    let y' = (Option.value y ~default:Empty) in
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
