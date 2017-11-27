open Core

module type Document_intf = sig
  type 'a t
  type 'a u
  val initial : 'a t
  val apply_operation : 'a t -> 'a u Mutation.t -> 'a u option * 'a t
  val append_to_final : 'a t -> 'a u -> 'a t
end

module Make(M : Document_intf) = struct
  type 'a t = 'a M.t
  type 'a u = 'a M.u

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

module ListDocument = Make(struct
  type 'a t = 'a list
  type 'a u = 'a

  let initial = []
  let append_to_final doc el = doc @ [el]

  let apply_operation doc = function
    | Mutation.Insert(x) -> Some(x), doc
    | Mutation.Delete    -> None, List.tl_exn doc
    | Mutation.Retain(x) when x = 1 -> Some(List.hd_exn doc), List.tl_exn doc
    | _ -> raise (Invalid_argument "Bad instruction")
end)
