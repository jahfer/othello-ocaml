open OUnit2
open Othello

let test_compose _ =
  let x, y = ([Mut.Insert('a'); Mut.Retain(1)], [Mut.Retain(2); Mut.Insert('b')]) in
  let comp = x |+| y in
  assert_equal [Mut.Insert('a'); Mut.Retain(1); Mut.Insert('b')] comp

let test_compose_booleans _ =
  let x, y = ([Mut.Insert(true); Mut.Retain(1)], [Mut.Retain(2); Mut.Insert(false)]) in
  let comp = x |+| y in
  assert_equal [Mut.Insert(true); Mut.Retain(1); Mut.Insert(false)] comp

let test_transform _ =
  let op_tom = [Mut.Retain(2); Mut.Insert('a')] in
  let op_jerry = [Mut.Retain(2); Mut.Insert('t')] in
  let lhs, rhs = op_tom |**| op_jerry in
  assert_equal [Mut.Retain(2); Mut.Insert('a'); Mut.Retain(1)] lhs;
  assert_equal [Mut.Retain(3); Mut.Insert('t')] rhs

let suite = "Othello Tests" >:::
  [
    "Compose" >:: test_compose;
    "Transform" >:: test_transform;
    "Compose booleans" >:: test_compose_booleans;
  ]
