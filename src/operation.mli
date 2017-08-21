type edit_operation = Retain of int | Insert of char | Delete | Empty
type list_application = Identity | Tail | Append of edit_operation | Swap of edit_operation
val to_string : edit_operation -> string
val apply : edit_operation list -> list_application -> edit_operation list

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

module type Executor_intf = sig
  type t
  type v
  val reduce : t list -> t list -> v
end

module Executor : functor (M : Action) -> (Executor_intf with type t = M.t and type v = M.v)

module Compose : (Action with type t = edit_operation and type v = edit_operation list)
module ComposeExecutor : (Executor_intf with type t = Compose.t and type v = Compose.v)
