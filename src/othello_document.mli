module type Document_intf = sig
  type 'a t
  type 'a u
  val initial : 'a t
  val apply_operation : 'a t -> 'a u Mutation.t -> 'a u option * 'a t
  val concat : 'a t -> 'a u -> 'a t
end

module Make(M : Document_intf) : sig
  val apply : 'a M.t -> 'a M.u Mutation.t list -> 'a M.t
end
