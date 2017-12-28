open OUnit2

module Mut = Othello.Mut

let test_delete_any _ =
  let x, y = ([Mut.Delete], [Mut.Insert('a')]) in
  let comp = Othello.Compose.exec x y in
  assert_equal [Mut.Delete; Mut.Insert('a')] comp

let test_any_insert _ =
  let x, y = ([Mut.Retain(1)], [Mut.Insert('a'); Mut.Retain(1)]) in
  let comp = Othello.Compose.exec x y in
  assert_equal [Mut.Insert('a'); Mut.Retain(1)] comp

let test_retain_retain _ =
  let x, y = ([Mut.Retain(1); Mut.Retain(1)], [Mut.Retain(2)]) in
  let comp = Othello.Compose.exec x y in
  assert_equal [Mut.Retain(1); Mut.Retain(1)] comp

let test_insert_retain _ =
  let x, y = ([Mut.Insert('a'); Mut.Retain(1)], [Mut.Retain(2); Mut.Insert('b')]) in
  let comp = Othello.Compose.exec x y in
  assert_equal [Mut.Insert('a'); Mut.Retain(1); Mut.Insert('b')] comp

let test_insert_delete _ =
  let x, y = ([Mut.Insert('a'); Mut.Retain(1)], [Mut.Delete; Mut.Retain(1)]) in
  let comp = Othello.Compose.exec x y in
  assert_equal [Mut.Retain(1)] comp

let test_retain_delete _ =
  let x, y = ([Mut.Retain(1)], [Mut.Delete]) in
  let comp = Othello.Compose.exec x y in
  assert_equal [Mut.Delete] comp

let suite = "Compose Tests" >:::
            [
              "Compose (Delete / _)" >:: test_delete_any;
              "Compose (_ / Insert)" >:: test_any_insert;
              "Compose (Retain / Retain)" >:: test_retain_retain;
              "Compose (Insert / Retain)" >:: test_insert_retain;
              "Compose (Insert / Delete)" >:: test_insert_delete;
              "Compose (Retain / Delete)" >:: test_retain_delete;
            ]
