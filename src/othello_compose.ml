open Core

module Mut = Mutation

module ComposeApplicative : (Reducer.Applicative with type t = Mut.t and type 'a u = 'a) = struct
  type t = Mut.t
  type 'a u = 'a

  let fmap f x = f x
  let apply = fmap

  let instruct x y =
    let x' = (Option.value x ~default:Mut.Empty) in
    let y' = (Option.value y ~default:Mut.Empty) in
    let open List_iterator in
    match x', y' with
    | Mut.Retain(a), Mut.Retain(b) ->
      if a > b then                   (Swap(Mut.Retain(a-b)), Tail, Append(y'))
      else if a < b then              (Tail, Swap(Mut.Retain(b-a)), Append(x'))
      else                            (Tail, Tail, Append(x'))
    | Mut.Delete,    _             -> (Tail, Identity, Append(x'))
    | _,         Mut.Insert(_)     -> (Identity, Tail, Append(y'))
    | Mut.Insert(_), Mut.Retain(1) -> (Tail, Tail, Append(x'))
    | Mut.Insert(_), Mut.Retain(b) -> (Tail, Swap(Mut.Retain(b-1)), Append(x'))
    | Mut.Insert(_), Mut.Delete    -> (Tail, Tail, Identity)
    | Mut.Retain(_), Mut.Delete    -> (Tail, Tail, Append(y'))
    | _, _                         -> raise (Failure "Unreachable")
end

module ComposeReducer = Reducer.Make(ComposeApplicative)

let exec x y =
  List.rev @@ ComposeReducer.reduce x y []
