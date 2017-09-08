type mutation = Retain of int | Insert of char | Delete | Empty

module type Action = sig
  type t
  type _ u
  val initial : 'a list u
  val instruct :
    t option -> t option ->
    (t List_iterator.instruction * t List_iterator.instruction * t List_iterator.instruction u)
  val accumulate : 'a list u -> 'a List_iterator.instruction u -> 'a list u
  val finalize : t list u -> t list u
end

module type Executor_intf = sig
  type t
  type _ u
  val reduce : t list -> t list -> t list u
end

module Executor : functor (M : Action) -> (Executor_intf with type t = M.t and type 'a u = 'a M.u)

module type OperationAction = Action with type t = mutation

module Compose : (OperationAction with type 'a u = 'a)
module ComposeExecutor : (Executor_intf with type t = Compose.t and type 'a u = 'a Compose.u)

module Transform : (OperationAction with type 'a u = 'a * 'a)
module TransformExecutor : (Executor_intf with type t = Transform.t and type 'a u = 'a Transform.u)
