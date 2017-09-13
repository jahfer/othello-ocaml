type mutation = Retain of int | Insert of char | Delete | Empty

module Compose : sig
  val exec : mutation list -> mutation list -> mutation list
end

module Transform : sig
  val exec : mutation list -> mutation list -> mutation list * mutation list
end
