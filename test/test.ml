open OUnit2

let suite = "Othello" >:::
  [Test_compose.suite;
   Test_transform.suite;
   Test_document.suite;
   Test_othello.suite]

let _ =
  run_test_tt_main suite
