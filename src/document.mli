module type Document_intf = sig
  type 'a t
  type 'a u
  val initial : 'a t
  val apply_operation : 'a t -> 'a u Mutation.t -> 'a u option * 'a t
  val append_to_final : 'a t -> 'a u -> 'a t
end

module Make(M : Document_intf) : sig
  val apply : 'a M.t -> 'a M.u Mutation.t list -> 'a M.t
end

module ListDocument : sig
  val apply : 'a list -> 'a Mutation.t list -> 'a list
end
