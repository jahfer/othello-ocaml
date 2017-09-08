open Core

type 'a instruction = Identity | Tail | Append of 'a | Swap of 'a

let step l = function
| Identity -> l
| Append(x) -> x :: l
| Swap(x) -> x :: (List.tl_exn l)
| Tail -> List.tl_exn l
