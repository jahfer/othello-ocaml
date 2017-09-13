open OUnit2

let op_tom = [Operation.Retain(2); Operation.Insert('a')]
let op_jerry = [Operation.Retain(2); Operation.Insert('t')]
let op_barry = [Operation.Retain(1); Operation.Delete]

let test_transform_two_operations _ =
  let lhs, rhs = Operation.Transform.exec op_tom op_jerry in
  assert_equal [Operation.Retain(2); Operation.Insert('a'); Operation.Retain(1)] lhs;
  assert_equal [Operation.Retain(3); Operation.Insert('t')] rhs

let test_delete_insert _ =
  let lhs, rhs = Operation.Transform.exec op_tom op_barry in
  assert_equal [Operation.Retain(1); Operation.Insert('a')] lhs;
  assert_equal [Operation.Retain(1); Operation.Delete; Operation.Retain(1)] rhs

let test_insert_delete _ =
  let lhs, rhs = Operation.Transform.exec op_barry op_tom in
  assert_equal [Operation.Retain(1); Operation.Delete; Operation.Retain(1)] lhs;
  assert_equal [Operation.Retain(1); Operation.Insert('a')] rhs

let test_transforming_two_delete_operations _ =
  let lhs, rhs = Operation.Transform.exec op_barry op_barry in
  assert_equal [Operation.Retain(1)] lhs;
  assert_equal [Operation.Retain(1)] rhs

let test_transform_deletes_at_different_points _ =
  let a, b = [Operation.Retain(1); Operation.Delete; Operation.Retain(2)], [Operation.Retain(2); Operation.Delete; Operation.Retain(1)] in
  let lhs, rhs = Operation.Transform.exec a b in
  assert_equal [Operation.Retain(1); Operation.Delete; Operation.Retain(1)] lhs;
  assert_equal [Operation.Retain(1); Operation.Delete; Operation.Retain(1)] rhs

let suite = "Transform" >:::
  [
    "Transforming two operations" >:: test_transform_two_operations;
    "Transforming a delete operation with an insert operation" >:: test_delete_insert;
    "Transforming an insert operation with a delete operation" >:: test_insert_delete;
    "Tranforming two delete operations" >:: test_transforming_two_delete_operations;
    "Transforming deletes at different points of the same document" >:: test_transform_deletes_at_different_points;
  ]
