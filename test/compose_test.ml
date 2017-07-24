open OUnit

let teardown _ =
  ()

let test_del =
  let x = [Operation.Delete];
  let y = [Operation.Insert('a')];
  let comp = Compose.compose_operations x y;
  assert_equal [Operation.Delete; Operation.Insert('a')] comp

let suite = "Test Compose" >:::
  ["test_del" >:: (bracket test_del teardown)]

let _ = run_test_tt_main suite
