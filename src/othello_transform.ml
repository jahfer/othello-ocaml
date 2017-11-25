open Core

module Mut = Mutation

module TransformApplicative : (Reducer.Applicative with type 'a t = 'a Mut.t and type 'a u = 'a * 'a) = struct
  type 'a t = 'a Mut.t
  type 'a u = 'a * 'a

  let fmap f (t1, t2) = f t1, f t2
  let apply (f1, f2) (t1, t2) = f1 t1, f2 t2

  let instruct x y =
    let open List_iterator in
    match x, y with
    | Some(Mut.Insert(_) as x'), _ -> (Tail, Identity, (Append(x'), Append(Mut.Retain(1))))
    | _, Some(Mut.Insert(_) as y') -> (Identity, Tail, (Append(Mut.Retain(1)), Append(y')))
    | Some(x'), Some(y')           -> (match x', y' with
      | Mut.Retain(a), Mut.Retain(b) when a > b -> (Swap(Mut.Retain(a-b)), Tail, (Append(y'), Append(y')))
      | Mut.Retain(a), Mut.Retain(b) when a < b -> (Tail, Swap(Mut.Retain(b-a)), (Append(x'), Append(x')))
      | Mut.Retain(_), Mut.Retain(_) -> (Tail, Tail, (Append(x'), Append(x')))
      | Mut.Delete,    Mut.Delete    -> (Tail, Tail, (Identity, Identity))
      | Mut.Delete,    Mut.Retain(1) -> (Tail, Tail, (Append(x'), Identity))
      | Mut.Delete,    Mut.Retain(b) -> (Tail, Swap(Mut.Retain(b-1)), (Append(x'), Identity))
      | Mut.Retain(1), Mut.Delete    -> (Tail, Tail, (Identity, Append(y')))
      | Mut.Retain(a), Mut.Delete    -> (Swap(Mut.Retain(a-1)), Tail, (Identity, Append(y')))
      | _ -> raise (Failure "Unreachable"))
    | _ -> raise (Invalid_argument "Bad instruction")
end

module TransformReducer = Reducer.Make(TransformApplicative)

let rev_compress l =
  let rec compress_list lst acc =
    match lst, acc with
    | [], _ -> acc
    | Mut.Retain(x) :: x_tl, Mut.Retain(y) :: y_tl -> compress_list x_tl (Mut.Retain(x + y) :: y_tl)
    | x :: tl, _ -> compress_list tl (x :: acc)
  in compress_list l []

let exec x y =
  let (a, b) = TransformReducer.reduce x y ([],[]) in
  rev_compress a, rev_compress b
