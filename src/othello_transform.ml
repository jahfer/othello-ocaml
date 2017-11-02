open Core

module M = Mutation

module TransformApplicative : (Reducer.Applicative with type t = M.t and type 'a u = 'a * 'a) = struct
  type t = M.t
  type 'a u = 'a * 'a

  let fmap f (t1, t2) = f t1, f t2
  let apply (f1, f2) (t1, t2) = f1 t1, f2 t2

  let instruct x y =
    let x' = (Option.value x ~default:M.Empty) in
    let y' = (Option.value y ~default:M.Empty) in
    let open List_iterator in
    match x', y' with
    | M.Insert(_), _           -> (Tail, Identity, (Append(x'), Append(M.Retain(1))))
    | _, M.Insert(_)           -> (Identity, Tail, (Append(M.Retain(1)), Append(y')))
    | M.Retain(a), M.Retain(b) ->
      if a > b then           (Swap(M.Retain(a-b)), Tail, (Append(y'), Append(y')))
      else if a < b then      (Tail, Swap(M.Retain(b-a)), (Append(x'), Append(x')))
      else                    (Tail, Tail, (Append(x'), Append(x')))
    | M.Delete, M.Delete       -> (Tail, Tail, (Identity, Identity))
    | M.Delete, M.Retain(1)    -> (Tail, Tail, (Append(x'), Identity))
    | M.Delete, M.Retain(b)    -> (Tail, Swap(M.Retain(b-1)), (Append(x'), Identity))
    | M.Retain(1), M.Delete    -> (Tail, Tail, (Identity, Append(y')))
    | M.Retain(a), M.Delete    -> (Swap(M.Retain(a-1)), Tail, (Identity, Append(y')))
    | _, _                 -> raise (Failure "Unreachable")
end

module TransformReducer = Reducer.Make(TransformApplicative)

let rev_compress l =
  let rec compress_list lst acc =
    match lst, acc with
    | [], _ -> acc
    | M.Retain(x) :: x_tl, M.Retain(y) :: y_tl -> compress_list x_tl (M.Retain(x + y) :: y_tl)
    | x :: tl, _ -> compress_list tl (x :: acc)
  in compress_list l []

let exec x y =
  let (a, b) = TransformReducer.reduce x y ([],[]) in
  rev_compress a, rev_compress b
