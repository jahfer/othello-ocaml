open OUnit2

module M = Mutation

let op_tom = [M.Retain(2); M.Insert('a')]
let op_jerry = [M.Retain(2); M.Insert('t')]
let op_barry = [M.Retain(1); M.Delete]

let test_transform_two_operations _ =
  let lhs, rhs = Othello_transform.exec op_tom op_jerry in
  assert_equal [M.Retain(2); M.Insert('a'); M.Retain(1)] lhs;
  assert_equal [M.Retain(3); M.Insert('t')] rhs

let test_delete_insert _ =
  let lhs, rhs = Othello_transform.exec op_tom op_barry in
  assert_equal [M.Retain(1); M.Insert('a')] lhs;
  assert_equal [M.Retain(1); M.Delete; M.Retain(1)] rhs

let test_insert_delete _ =
  let lhs, rhs = Othello_transform.exec op_barry op_tom in
  assert_equal [M.Retain(1); M.Delete; M.Retain(1)] lhs;
  assert_equal [M.Retain(1); M.Insert('a')] rhs

let test_transforming_two_delete_operations _ =
  let lhs, rhs = Othello_transform.exec op_barry op_barry in
  assert_equal [M.Retain(1)] lhs;
  assert_equal [M.Retain(1)] rhs

let test_transform_deletes_at_different_points _ =
  let a, b = [M.Retain(1); M.Delete; M.Retain(2)], [M.Retain(2); M.Delete; M.Retain(1)] in
  let lhs, rhs = Othello_transform.exec a b in
  assert_equal [M.Retain(1); M.Delete; M.Retain(1)] lhs;
  assert_equal [M.Retain(1); M.Delete; M.Retain(1)] rhs

let suite = "Transform" >:::
  [
    "Transforming two operations" >:: test_transform_two_operations;
    "Transforming a delete operation with an insert operation" >:: test_delete_insert;
    "Transforming an insert operation with a delete operation" >:: test_insert_delete;
    "Tranforming two delete operations" >:: test_transforming_two_delete_operations;
    "Transforming deletes at different points of the same document" >:: test_transform_deletes_at_different_points;
  ]
