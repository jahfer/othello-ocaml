open OUnit2
open Othello

let test_operations _ =
  let doc = "ram" and
      ops = [Mut.Retain(1); Mut.Insert("o"); Mut.Retain(2); Mut.Insert("!")] in
  assert_equal "roam!" @@ StringDocument.apply doc ops

let suite = "Document Tests" >:::
  [
    "Operation list" >:: test_operations;
  ]

