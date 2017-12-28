open OUnit2

module Rope = Othello.Rope

let test_concat _ =
  let hello = Rope.of_string "hello" in
  let world = Rope.of_string " world" in
  let r = Rope.concat hello world in
  assert_equal "hello world" @@ Rope.string_of_rope(r)

let test_append _ =
  let r = Rope.of_string "hello world" in
  let r' = Rope.append r "!" in
  assert_equal "hello world!" @@ Rope.string_of_rope(r')

let test_split_at_lhs _ =
  let hello = Rope.of_string "hello" in
  let world = Rope.of_string " world" in
  let r = Rope.concat hello world in
  let (left, right) = Rope.split_at r 2 in
  assert_equal "he" @@ Rope.string_of_rope(left);
  assert_equal "llo world" @@ Rope.string_of_rope(right)

let test_split_at_rhs _ =
  let hello = Rope.of_string "hello" in
  let world = Rope.of_string " world" in
  let r = Rope.concat hello world in
  let (left, right) = Rope.split_at r 8 in
  assert_equal "hello wo" @@ Rope.string_of_rope(left);
  assert_equal "rld" @@ Rope.string_of_rope(right)

let suite = "Rope Tests" >:::
            [
              "Concat" >:: test_concat;
              "Append" >:: test_append;
              "Split At (LHS)" >:: test_split_at_lhs;
              "Split At (RHS)" >:: test_split_at_rhs;
            ]
