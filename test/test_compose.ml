open OUnit2

let test_delete_any _ =
  let x, y = ([Mutation.Delete], [Mutation.Insert('a')]) in
  let comp = Othello_compose.exec x y in
  assert_equal [Mutation.Delete; Mutation.Insert('a')] comp

let test_any_insert _ =
  let x, y = ([Mutation.Retain(1)], [Mutation.Insert('a'); Mutation.Retain(1)]) in
  let comp = Othello_compose.exec x y in
  assert_equal [Mutation.Insert('a'); Mutation.Retain(1)] comp

let test_retain_retain _ =
  let x, y = ([Mutation.Retain(1); Mutation.Retain(1)], [Mutation.Retain(2)]) in
  let comp = Othello_compose.exec x y in
  assert_equal [Mutation.Retain(1); Mutation.Retain(1)] comp

let test_insert_retain _ =
  let x, y = ([Mutation.Insert('a'); Mutation.Retain(1)], [Mutation.Retain(2); Mutation.Insert('b')]) in
  let comp = Othello_compose.exec x y in
  assert_equal [Mutation.Insert('a'); Mutation.Retain(1); Mutation.Insert('b')] comp

let test_insert_delete _ =
  let x, y = ([Mutation.Insert('a'); Mutation.Retain(1)], [Mutation.Delete; Mutation.Retain(1)]) in
  let comp = Othello_compose.exec x y in
  assert_equal [Mutation.Retain(1)] comp

let test_retain_delete _ =
  let x, y = ([Mutation.Retain(1)], [Mutation.Delete]) in
  let comp = Othello_compose.exec x y in
  assert_equal [Mutation.Delete] comp

let suite = "Compose Tests" >:::
  [
    "Compose (Delete / _)" >:: test_delete_any;
    "Compose (_ / Insert)" >:: test_any_insert;
    "Compose (Retain / Retain)" >:: test_retain_retain;
    "Compose (Insert / Retain)" >:: test_insert_retain;
    "Compose (Insert / Delete)" >:: test_insert_delete;
    "Compose (Retain / Delete)" >:: test_retain_delete;
  ]
