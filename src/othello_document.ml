open Core

module type Document_intf = sig
  type 'a t
  type 'a u
  val initial : 'a t
  val apply_operation : 'a t -> 'a u Mutation.t -> 'a u option * 'a t
  val concat : 'a t -> 'a u -> 'a t
end

module Make(M : Document_intf) = struct
  type 'a t = 'a M.t
  type 'a u = 'a M.u

  (* This fn is too opinionated; requires rebuilding the
     data structure on every append! *)
  let apply d o =
    let rec apply_operations out built_op = function
      | [] -> out
      | x :: ops -> let (hd, tl) = M.apply_operation built_op x in
        let out' = match hd with
          | Some(x) -> M.concat out x
          | None -> out
        in apply_operations out' tl ops
    in apply_operations M.initial d o
end
