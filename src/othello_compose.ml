open Core

module Mut = Mutation

type v = char

module ComposeApplicative : (Reducer.Applicative with type 'a t = 'a Mut.t and type 'b u = 'b) = struct
  type 'a t = 'a Mut.t
  type 'b u = 'b

  let fmap f x = f x
  let apply = fmap

  let instruct x y =
    let open List_iterator in
    match x, y with
    | None, Some(Mut.Insert(_) as y') -> (Identity, Tail, Append(y'))
    | None, None -> raise (Invalid_argument "Bad instruction")
    | Some(x'), Some(y') -> (match x', y' with
      | Mut.Retain(a), Mut.Retain(b) when a > b -> (Swap(Mut.Retain(a-b)), Tail, Append(y'))
      | Mut.Retain(a), Mut.Retain(b) when a < b -> (Tail, Swap(Mut.Retain(b-a)), Append(x'))
      | Mut.Retain(_), Mut.Retain(_) -> (Tail, Tail, Append(x'))
      | Mut.Delete,    _             -> (Tail, Identity, Append(x'))
      | _,             Mut.Insert(_) -> (Identity, Tail, Append(y'))
      | Mut.Insert(_), Mut.Retain(1) -> (Tail, Tail, Append(x'))
      | Mut.Insert(_), Mut.Retain(b) -> (Tail, Swap(Mut.Retain(b-1)), Append(x'))
      | Mut.Insert(_), Mut.Delete    -> (Tail, Tail, Identity)
      | Mut.Retain(_), Mut.Delete    -> (Tail, Tail, Append(y')))
    | _ -> raise (Invalid_argument "Bad instruction")
end

module ComposeReducer = Reducer.Make(ComposeApplicative)

let exec x y =
  List.rev @@ ComposeReducer.reduce x y []
