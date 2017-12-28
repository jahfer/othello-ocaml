open OUnit2

module Mut = Othello.Mut

let op_tom = [Mut.Retain(2); Mut.Insert('a')]
let op_jerry = [Mut.Retain(2); Mut.Insert('t')]
let op_barry = [Mut.Retain(1); Mut.Delete]

let test_transform_two_operations _ =
  let lhs, rhs = Othello.Transform.exec op_tom op_jerry in
  assert_equal [Mut.Retain(2); Mut.Insert('a'); Mut.Retain(1)] lhs;
  assert_equal [Mut.Retain(3); Mut.Insert('t')] rhs

let test_delete_insert _ =
  let lhs, rhs = Othello.Transform.exec op_tom op_barry in
  assert_equal [Mut.Retain(1); Mut.Insert('a')] lhs;
  assert_equal [Mut.Retain(1); Mut.Delete; Mut.Retain(1)] rhs

let test_insert_delete _ =
  let lhs, rhs = Othello.Transform.exec op_barry op_tom in
  assert_equal [Mut.Retain(1); Mut.Delete; Mut.Retain(1)] lhs;
  assert_equal [Mut.Retain(1); Mut.Insert('a')] rhs

let test_transforming_two_delete_operations _ =
  let lhs, rhs = Othello.Transform.exec op_barry op_barry in
  assert_equal [Mut.Retain(1)] lhs;
  assert_equal [Mut.Retain(1)] rhs

let test_transform_deletes_at_different_points _ =
  let a, b = [Mut.Retain(1); Mut.Delete; Mut.Retain(2)], [Mut.Retain(2); Mut.Delete; Mut.Retain(1)] in
  let lhs, rhs = Othello.Transform.exec a b in
  assert_equal [Mut.Retain(1); Mut.Delete; Mut.Retain(1)] lhs;
  assert_equal [Mut.Retain(1); Mut.Delete; Mut.Retain(1)] rhs

let suite = "Transform" >:::
            [
              "Transforming two operations" >:: test_transform_two_operations;
              "Transforming a delete operation with an insert operation" >:: test_delete_insert;
              "Transforming an insert operation with a delete operation" >:: test_insert_delete;
              "Tranforming two delete operations" >:: test_transforming_two_delete_operations;
              "Transforming deletes at different points of the same document" >:: test_transform_deletes_at_different_points;
            ]
