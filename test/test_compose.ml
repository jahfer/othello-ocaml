open OUnit2

let test_delete_any _ =
  let x, y = ([Operation.Delete], [Operation.Insert('a')]) in
  let comp = Operation.Compose.exec x y in
  assert_equal [Operation.Delete; Operation.Insert('a')] comp

let test_any_insert _ =
  let x, y = ([Operation.Retain(1)], [Operation.Insert('a'); Operation.Retain(1)]) in
  let comp = Operation.Compose.exec x y in
  assert_equal [Operation.Insert('a'); Operation.Retain(1)] comp

let test_retain_retain _ =
  let x, y = ([Operation.Retain(1); Operation.Retain(1)], [Operation.Retain(2)]) in
  let comp = Operation.Compose.exec x y in
  assert_equal [Operation.Retain(1); Operation.Retain(1)] comp

let test_insert_retain _ =
  let x, y = ([Operation.Insert('a'); Operation.Retain(1)], [Operation.Retain(2); Operation.Insert('b')]) in
  let comp = Operation.Compose.exec x y in
  assert_equal [Operation.Insert('a'); Operation.Retain(1); Operation.Insert('b')] comp

let test_insert_delete _ =
  let x, y = ([Operation.Insert('a'); Operation.Retain(1)], [Operation.Delete; Operation.Retain(1)]) in
  let comp = Operation.Compose.exec x y in
  assert_equal [Operation.Retain(1)] comp

let test_retain_delete _ =
  let x, y = ([Operation.Retain(1)], [Operation.Delete]) in
  let comp = Operation.Compose.exec x y in
  assert_equal [Operation.Delete] comp

let suite = "Compose Tests" >:::
  [
    "#compose_operations (Delete / _)" >:: test_delete_any;
    "#compose_operations (_ / Insert)" >:: test_any_insert;
    "#compose_operations (Retain / Retain)" >:: test_retain_retain;
    "#compose_operations (Insert / Retain)" >:: test_insert_retain;
    "#compose_operations (Insert / Delete)" >:: test_insert_delete;
    "#compose_operations (Retain / Delete)" >:: test_retain_delete;
  ]
