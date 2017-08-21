open Core

type edit_operation =
  | Retain of int
  | Insert of char
  | Delete
  | Empty

let to_string operation =
  match operation with
  | Retain x -> sprintf "Retain(%d)" x
  | Insert x -> sprintf "Insert(%c)" x
  | Delete   -> sprintf "Delete"
  | Empty    -> sprintf "Empty"

type list_application =
  | Identity
  | Tail
  | Append of edit_operation
  | Swap of edit_operation

let apply l a =
  match a with
  | Identity -> l
  | Append(x) -> x :: l
  | Swap(x) -> x :: (List.tl_exn l)
  | Tail -> List.tl_exn l

module type Executor_intf = sig
  type t
  type v
  val reduce : t list -> t list -> v
end

module type Action = sig
  type t = edit_operation
  type u = list_application
  type v
  type w
  val initial : v
  val instruct : t option -> t option -> u * u * w
  val accumulate : v -> w -> v
  val finalize : v -> v
end

module Executor(M : Action) : (Executor_intf with type t = M.t and type v = M.v) = struct
  include M

  let rec fold a b acc =
    match a, b with
    | [], [] -> M.finalize acc
    | _ ->
      let ia, ib, iacc = M.instruct (List.hd a) (List.hd b) in
      fold (apply a ia) (apply b ib) (M.accumulate acc iacc)

  let reduce a b = fold a b M.initial
end

module Compose : (Action with type t = edit_operation and type v = edit_operation list) = struct
  type t = edit_operation
  type u = list_application
  type v = edit_operation list
  type w = u

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

(* module Transform : Action = struct
  type t = edit_operation
  type u = list_application

  let instruct x y =
    let x' = (Option.value x ~default:Empty) in
    let y' = (Option.value y ~default:Empty) in
    match x', y' with
    | Insert(_), _         -> (Tail, Identity, Append(x'), Append(Retain(1)))
    | _, Insert(_)         -> (Identity, Tail, Append(Retain(1)), Append(y'))
    | Retain(a), Retain(b) ->
      if a > b then           (Swap(Retain(a-b)), Tail, Append(y'), Append(y'))
      else if a < b then      (Tail, Swap(Retain(b-a)), Append(x'), Append(x'))
      else                    (Tail, Tail, Append(x'), Append(x'))
    | Delete, Delete       -> (Tail, Tail, Identity, Identity)
    | Delete, Retain(1)    -> (Tail, Tail, Append(x'), Identity)
    | Delete, Retain(b)    -> (Tail, Swap(Retain(b-1)), Append(x'), Identity)
    | Retain(1), Delete    -> (Tail, Tail, Identity, Append(y'))
    | Retain(a), Delete    -> (Swap(Retain(a-1)), Tail, Identity, Append(y'))
    | _, _                 -> raise (Failure "Unreachable")
end
module TransformExecutor = Executor(Transform) *)
