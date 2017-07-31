type edit_operation = Retain of int | Insert of char | Delete | Empty
val to_string: edit_operation -> string
val size: edit_operation -> int

module List : sig
  type t = edit_operation
  type application = Identity | Tail | Append of t | Swap of t
  val apply: t list -> application -> t list
end
