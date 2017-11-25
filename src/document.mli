module type Document_intf = sig
  type t
  val initial : t
  val apply_operation : t -> t Mutation.t -> t option * t
  val append_to_final : t -> t -> t
end

module Make(M : Document_intf) : sig
  val apply : M.t -> M.t Mutation.t list -> M.t
end
