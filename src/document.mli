module type Document_intf = sig
  type t
  type u
  val initial : t
  val apply_operation : t -> u Mutation.t -> u option * t
  val append_to_final : t -> u -> t
end

module Make(M : Document_intf) : sig
  val apply : M.t -> M.u Mutation.t list -> M.t
end
