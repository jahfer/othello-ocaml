open OUnit2
open Othello

let test_operations _ =
  let doc = "ram" and
      ops = [Mut.Retain(1); Mut.Insert("o"); Mut.Retain(2); Mut.Insert("!")] in
  assert_equal "roam!" @@ StringDocument.apply doc ops

let test_nodes _ =
  let doc = NodeDocument.([
    { kind = Paragraph("r") };
    { kind = Paragraph("a") };
    { kind = Paragraph("m") };
  ]) in
  let new_node = NodeDocument.({ kind = Paragraph("o") }) in
  let ops = [Mut.Retain(1); Mut.Insert(new_node); Mut.Retain(1); Mut.Retain(1)] in
  (* let _ = NodeDocument.print_nodes @@ StructuredDocument.apply doc ops in *)
  assert_equal NodeDocument.([
    { kind = Paragraph("r") };
    { kind = Paragraph("o") };
    { kind = Paragraph("a") };
    { kind = Paragraph("m") };
  ]) @@ StructuredDocument.apply doc ops

let suite = "Document Tests" >:::
  [
    "Operation list" >:: test_operations;
    "Node list" >:: test_nodes;
  ]

