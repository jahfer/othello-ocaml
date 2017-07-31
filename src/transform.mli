type t = Operation.edit_operation list

val compress: t -> t
val transform_operations: t -> t -> (t * t)
