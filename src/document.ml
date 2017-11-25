open Core

module type Document_intf = sig
  type t
  val initial : t
  val apply_operation : t -> t Mutation.t -> t option * t
  val append_to_final : t -> t -> t
end

module Make(M : Document_intf) = struct
  type t = M.t

  let apply d o =
    let rec apply_operations out doc = function
      | [] -> out
      | x :: ops -> let (hd, tl) = M.apply_operation doc x in
        let out' = match hd with
        | Some(x) -> M.append_to_final out x
        | None -> out
        in apply_operations out' tl ops
      in apply_operations M.initial d o
end
