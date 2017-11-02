open Core

module M = Mutation

module ComposeApplicative : (Reducer.Applicative with type t = M.t and type 'a u = 'a) = struct
  type t = M.t
  type 'a u = 'a

  let fmap f x = f x
  let apply = fmap

  let instruct x y =
    let x' = (Option.value x ~default:M.Empty) in
    let y' = (Option.value y ~default:M.Empty) in
    let open List_iterator in
    match x', y' with
    | M.Retain(a), M.Retain(b) ->
      if a > b then           (Swap(M.Retain(a-b)), Tail, Append(y'))
      else if a < b then      (Tail, Swap(M.Retain(b-a)), Append(x'))
      else                    (Tail, Tail, Append(x'))
    | M.Delete,    _           -> (Tail, Identity, Append(x'))
    | _,         M.Insert(_)   -> (Identity, Tail, Append(y'))
    | M.Insert(_), M.Retain(1) -> (Tail, Tail, Append(x'))
    | M.Insert(_), M.Retain(b) -> (Tail, Swap(M.Retain(b-1)), Append(x'))
    | M.Insert(_), M.Delete    -> (Tail, Tail, Identity)
    | M.Retain(_), M.Delete    -> (Tail, Tail, Append(y'))
    | _, _                 -> raise (Failure "Unreachable")
end

module ComposeReducer = Reducer.Make(ComposeApplicative)

let exec x y =
  List.rev @@ ComposeReducer.reduce x y []
