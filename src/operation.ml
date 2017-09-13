open Core

type mutation = Retain of int | Insert of char | Delete | Empty

module type Applicative = sig
  type t
  type _ u
  val instruct : t option -> t option ->
    (t List_iterator.instruction * t List_iterator.instruction * t List_iterator.instruction u)
  val fmap : ('a -> 'b) -> 'a u -> 'b u
  val apply : ('a -> 'b) u -> 'a u -> 'b u
end

module Reducer(M : Applicative) = struct
  type t = M.t
  type 'a u = 'a M.u

  let (<*>) f x = M.apply f x
  let (<$>) f x = M.fmap f x

  let rec reduce a b acc =
    match a, b with
    | [], [] -> acc
    | _ -> let ia, ib, iacc = M.instruct (List.hd a) (List.hd b) in
      reduce (List_iterator.step a ia) (List_iterator.step b ib) (List_iterator.step <$> acc <*> iacc)
end

module ComposeApplicative : (Applicative with type t = mutation and type 'a u = 'a) = struct
  type t = mutation
  type 'a u = 'a

  let fmap f x = f x
  let apply = fmap

  let instruct x y =
    let x' = (Option.value x ~default:Empty) in
    let y' = (Option.value y ~default:Empty) in
    let open List_iterator in
    match x', y' with
    | Retain(a), Retain(b) ->
      if a > b then           (Swap(Retain(a-b)), Tail, Append(y'))
      else if a < b then      (Tail, Swap(Retain(b-a)), Append(x'))
      else                    (Tail, Tail, Append(x'))
    | Delete,    _         -> (Tail, Identity, Append(x'))
    | _,         Insert(_) -> (Identity, Tail, Append(y'))
    | Insert(_), Retain(1) -> (Tail, Tail, Append(x'))
    | Insert(_), Retain(b) -> (Tail, Swap(Retain(b-1)), Append(x'))
    | Insert(_), Delete    -> (Tail, Tail, Identity)
    | Retain(_), Delete    -> (Tail, Tail, Append(y'))
    | _, _                 -> raise (Failure "Unreachable")
end

module TransformApplicative : (Applicative with type t = mutation and type 'a u = 'a * 'a) = struct
  type t = mutation
  type 'a u = 'a * 'a

  let fmap f (t1, t2) = f t1, f t2
  let apply (f1, f2) (t1, t2) = f1 t1, f2 t2

  let instruct x y =
    let x' = (Option.value x ~default:Empty) in
    let y' = (Option.value y ~default:Empty) in
    let open List_iterator in
    match x', y' with
    | Insert(_), _         -> (Tail, Identity, (Append(x'), Append(Retain(1))))
    | _, Insert(_)         -> (Identity, Tail, (Append(Retain(1)), Append(y')))
    | Retain(a), Retain(b) ->
      if a > b then           (Swap(Retain(a-b)), Tail, (Append(y'), Append(y')))
      else if a < b then      (Tail, Swap(Retain(b-a)), (Append(x'), Append(x')))
      else                    (Tail, Tail, (Append(x'), Append(x')))
    | Delete, Delete       -> (Tail, Tail, (Identity, Identity))
    | Delete, Retain(1)    -> (Tail, Tail, (Append(x'), Identity))
    | Delete, Retain(b)    -> (Tail, Swap(Retain(b-1)), (Append(x'), Identity))
    | Retain(1), Delete    -> (Tail, Tail, (Identity, Append(y')))
    | Retain(a), Delete    -> (Swap(Retain(a-1)), Tail, (Identity, Append(y')))
    | _, _                 -> raise (Failure "Unreachable")
end

module Compose = struct
  module ComposeReducer = Reducer(ComposeApplicative)

  let exec x y =
    List.rev @@ ComposeReducer.reduce x y []
end

module Transform = struct
  module TransformReducer = Reducer(TransformApplicative)

  let rev_compress l =
    let rec compress_list lst acc =
      match lst, acc with
      | [], _ -> acc
      | Retain(x) :: x_tl, Retain(y) :: y_tl -> compress_list x_tl (Retain(x + y) :: y_tl)
      | x :: tl, _ -> compress_list tl (x :: acc)
    in compress_list l []

  let exec x y =
    let (a, b) = TransformReducer.reduce x y ([],[]) in
    rev_compress a, rev_compress b
end
