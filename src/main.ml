open Core

 let rec print_list = function
  | [] -> print_string "\n"
  | hd::tl -> print_string hd ; print_string " " ; print_list tl

let print_oplist l =
  let strings = List.map ~f:Operation.to_string l in
  print_list strings

(* let () =
  let ops1 = [Operation.Retain(2); Operation.Insert('a')] in
  let ops2 = [Operation.Retain(2); Operation.Insert('t')] in
  let xf1, xf2 = Transform.transform_operations ops1 ops2 in
  let () = print_oplist xf1 in
  print_oplist xf2 *)

let () =
  let ops1 = [Operation.Insert('a'); Operation.Retain(1)] in
  let ops2 = [Operation.Retain(2); Operation.Insert('b')] in
  let composed = Compose.compose_operations ops1 ops2 in
  let strings = List.map ~f:Operation.to_string composed in
  print_list strings

(* module type REDUCABLE_LIST = sig
  type t
  type u
  val (<%>) : t list -> u -> t list
  val fold : t list -> t list -> t list -> t list
end

module type OperationAction = sig
  type t = Operation.edit_operation
  type u = Operation.List.application
  val instruct : t option -> t option -> u * u * u
end

module OperationExecutor(M : OperationAction) :
  (REDUCABLE_LIST with type t = M.t and type u = M.u) = struct
  include M

  let (<%>) l a =
    match a with
    | Operation.List.Identity -> l
    | Operation.List.Append(x) -> x :: l
    | Operation.List.Swap(x) -> x :: (List.tl_exn l)
    | Operation.List.Tail -> List.tl_exn l

  let rec fold a b acc =
    match a, b with
    | [], [] -> List.rev acc
    | _ ->
      let ia, ib, iacc = M.instruct (List.hd a) (List.hd b) in
      fold
        (a <%> ia)
        (b <%> ib)
        (acc <%> iacc)
end *)

(* module Compose : OperationAction = struct
  type t = Operation.edit_operation
  type u = Operation.List.application

  let instruct x y =
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
end

module ComposeExecutor = OperationExecutor(Compose) *)

let () =
  let ops1 = [Operation.Insert('a'); Operation.Retain(1)] in
  let ops2 = [Operation.Retain(2); Operation.Insert('b')] in
  let composed = Operation.ComposeExecutor.fold ops1 ops2 [] in
  let strings = List.map ~f:Operation.to_string composed in
  print_list strings
