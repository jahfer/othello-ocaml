open Core

module type Document_intf = sig
  type t
  type u
  val initial : t
  val apply_operation : t -> u Mutation.t -> u option * t
  val append_to_final : t -> u -> t
end

module Make(M : Document_intf) = struct
  type t = M.t
  type u = M.u

  let apply d o =
    let rec apply_operations out built_op = function
      | [] -> out
      | x :: ops -> let (hd, tl) = M.apply_operation built_op x in
        let out' = match hd with
        | Some(x) -> M.append_to_final out x
        | None -> out
        in apply_operations out' tl ops
      in apply_operations M.initial d o
end
