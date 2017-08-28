type edit_operation = Retain of int | Insert of char | Delete | Empty
type 'a list_application = Identity | Tail | Append of 'a | Swap of 'a
val apply : 'a list -> 'a list_application -> 'a list

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

module Executor : functor (M : Action) -> (Executor_intf with type t = M.t and type 'a v = 'a M.v)

module Compose : (Action with type 'a v = 'a list)
module ComposeExecutor : (Executor_intf with type t = Compose.t and type 'a v = 'a Compose.v)

module Transform : (Action with type 'a v = 'a list * 'a list)
module TransformExecutor : (Executor_intf with type t = Transform.t and type 'a v = 'a Transform.v)
