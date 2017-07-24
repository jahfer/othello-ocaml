type edit_operation = Retain of int | Insert of char | Delete | Empty
val to_string: edit_operation -> string
val size: edit_operation -> int

module List : sig
  type t = edit_operation
  type application = Identity | Tail | Append of edit_operation | Swap of edit_operation
  val apply: t list -> application -> t list
  (* val cross: t list -> t list -> (t option -> t option -> application * application * application) -> t list *)
end
