open Core

module type Applicative = sig
  type t
  type _ u
  val instruct : t option -> t option ->
    (t List_iterator.instruction * t List_iterator.instruction * t List_iterator.instruction u)
  val fmap : ('a -> 'b) -> 'a u -> 'b u
  val apply : ('a -> 'b) u -> 'a u -> 'b u
end

module Make(M : Applicative) = struct
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
