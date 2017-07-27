open OUnit2

module C = Compose
module O = Operation

let test_delete_any _ =
  let x, y = ([O.Delete], [O.Insert('a')]) in
  let comp = C.compose_operations x y in
  assert_equal [O.Delete; O.Insert('a')] comp

let test_any_insert _ =
  let x, y = ([O.Retain(1)], [O.Insert('a'); O.Retain(1)]) in
  let comp = C.compose_operations x y in
  assert_equal [O.Insert('a'); O.Retain(1)] comp

let test_retain_retain _ =
  let x, y = ([O.Retain(1); O.Retain(1)], [O.Retain(2)]) in
  let comp = C.compose_operations x y in
  assert_equal [O.Retain(1); O.Retain(1)] comp

let test_insert_retain _ =
  let x, y = ([O.Insert('a'); O.Retain(1)], [O.Retain(2); O.Insert('b')]) in
  let comp = C.compose_operations x y in
  assert_equal [O.Insert('a'); O.Retain(1); O.Insert('b')] comp

let test_insert_delete _ =
  let x, y = ([O.Insert('a'); O.Retain(1)], [O.Delete; O.Retain(1)]) in
  let comp = C.compose_operations x y in
  assert_equal [O.Retain(1)] comp

let test_retain_delete _ =
  let x, y = ([O.Retain(1)], [O.Delete]) in
  let comp = C.compose_operations x y in
  assert_equal [O.Delete] comp

let suite = "Compose Tests" >:::
  [
    "#compose_operations (Delete / _)" >:: test_delete_any;
    "#compose_operations (_ / Insert)" >:: test_any_insert;
    "#compose_operations (Retain / Retain)" >:: test_retain_retain;
    "#compose_operations (Insert / Retain)" >:: test_insert_retain;
    "#compose_operations (Insert / Delete)" >:: test_insert_delete;
    "#compose_operations (Retain / Delete)" >:: test_retain_delete;
  ]
