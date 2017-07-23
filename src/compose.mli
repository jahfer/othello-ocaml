open Operation

type composition =
| Identity
| Tail
| Append of edit_operation
| Swap of edit_operation

val compose_operations: edit_operation list -> edit_operation list -> edit_operation list
