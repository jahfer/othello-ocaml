open OUnit2
module T = Transform
module O = Operation

let op_tom = [O.Retain(2); O.Insert('a')]
let op_jerry = [O.Retain(2); O.Insert('t')]
let op_barry = [O.Retain(1); O.Delete]

let test_transform_two_operations _ =
  let lhs, rhs = O.TransformExecutor.reduce op_tom op_jerry in
  assert_equal [O.Retain(2); O.Insert('a'); O.Retain(1)] lhs;
  assert_equal [O.Retain(3); O.Insert('t')] rhs

let test_delete_insert _ =
  let lhs, rhs = O.TransformExecutor.reduce op_tom op_barry in
  assert_equal [O.Retain(1); O.Insert('a')] lhs;
  assert_equal [O.Retain(1); O.Delete; O.Retain(1)] rhs

let test_insert_delete _ =
  let lhs, rhs = O.TransformExecutor.reduce op_barry op_tom in
  assert_equal [O.Retain(1); O.Delete; O.Retain(1)] lhs;
  assert_equal [O.Retain(1); O.Insert('a')] rhs

let test_transforming_two_delete_operations _ =
  let lhs, rhs = O.TransformExecutor.reduce op_barry op_barry in
  assert_equal [O.Retain(1)] lhs;
  assert_equal [O.Retain(1)] rhs

let test_transform_deletes_at_different_points _ =
  let a, b = [O.Retain(1); O.Delete; O.Retain(2)], [O.Retain(2); O.Delete; O.Retain(1)] in
  let lhs, rhs = O.TransformExecutor.reduce a b in
  assert_equal [O.Retain(1); O.Delete; O.Retain(1)] lhs;
  assert_equal [O.Retain(1); O.Delete; O.Retain(1)] rhs

 let test_compress _ =
  let a = [O.Retain(2); O.Retain(1); O.Insert('a'); O.Retain(1); O.Retain(3)] in
  let compressed = T.compress a in
  assert_equal [O.Retain(3); O.Insert('a'); O.Retain(4)] compressed

let suite = "Transform" >:::
  [
    "Transforming two operations" >:: test_transform_two_operations;
    "Transforming a delete operation with an insert operation" >:: test_delete_insert;
    "Transforming an insert operation with a delete operation" >:: test_insert_delete;
    "Tranforming two delete operations" >:: test_transforming_two_delete_operations;
    "Transforming deletes at different points of the same document" >:: test_transform_deletes_at_different_points;
  ]
