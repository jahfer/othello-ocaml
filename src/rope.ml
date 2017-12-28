type t = Node of int * t * t | Leaf of string * int

let of_string str = Leaf(str, String.length str)

let string_of_rope rope =
  let rec cat node acc = match node with
    | Leaf(str, _) -> acc ^ str
    | Node(_, left, right) -> acc |> cat left |> cat right
  in cat rope ""

let rec node_weight = function
| Leaf(str, length) -> length
| Node(_, lhs, rhs) -> (node_weight lhs) + (node_weight rhs)

let concat lhs rhs = Node(node_weight lhs, lhs, rhs)

let rec append rope str =
  str |> of_string |> concat rope

let rec split_at rope pos = match rope with
  | Node(weight, lhs, rhs) when pos <= weight ->
    let (lhs', rhs') = split_at lhs pos in (lhs', concat rhs' rhs)
  | Node(weight, lhs, rhs) ->
    let (lhs', rhs') = split_at rhs (pos - weight) in (concat lhs lhs', rhs')
  | Leaf(str, length) ->
    let lhs = Str.string_before str pos and rhs = Str.string_after str pos in
    (of_string lhs, of_string rhs)

let rev_concat rhs lhs = concat lhs rhs

let insert str pos rope =
  let (lhs, rhs) = split_at rope pos in str |> append lhs |> rev_concat rhs
