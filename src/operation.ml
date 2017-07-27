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

let size operation =
  match operation with
  | Retain x -> x
  | Delete -> 1
  | Insert _ | _ -> 0

module List = struct
  type t = edit_operation
  type application =
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
end
