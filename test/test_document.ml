open Core
open OUnit2
open Othello

module StringDocument = Document.Make(struct
  type 'a t = string
  type 'a u = string
  let initial = ""
  let append_to_final x y = x ^ y

  let apply_operation doc = function
    | Mutation.Insert(x) -> Some(x), doc
    | Mutation.Delete    -> None, Str.string_after doc 1
    | Mutation.Retain(x) -> Some(Str.string_before doc x), Str.string_after doc x
end)

let test_string_document _ =
  let doc = "ram" and
      ops = [Mut.Retain(1); Mut.Insert("o"); Mut.Retain(2); Mut.Insert("!")] in
  assert_equal "roam!" @@ StringDocument.apply doc ops

(* Custom object *)

module ListDocument = Document.Make(struct
  type 'a t = 'a list
  type 'a u = 'a

  let initial = []
  let append_to_final doc el = doc @ [el]

  let apply_operation doc = function
    | Mutation.Insert(x) -> Some(x), doc
    | Mutation.Delete    -> None, List.tl_exn doc
    | Mutation.Retain(x) when x = 1 -> Some(List.hd_exn doc), List.tl_exn doc
    | _ -> raise (Invalid_argument "Bad instruction")
end)

module Node = struct
  type node_type = Paragraph of string
  type t = { kind : node_type }
end

let test_nodes _ =
  let open Node in
  let doc = [
    { kind = Paragraph("r") };
    { kind = Paragraph("a") };
    { kind = Paragraph("m") };
  ] in
  let new_node = { kind = Paragraph("o") } in
  let ops = [Mut.Retain(1); Mut.Insert(new_node); Mut.Retain(1); Mut.Retain(1)] in
  assert_equal [
    { kind = Paragraph("r") };
    { kind = Paragraph("o") };
    { kind = Paragraph("a") };
    { kind = Paragraph("m") };
  ] @@ ListDocument.apply doc ops

let suite = "Document Tests" >:::
  [
    "String document" >:: test_string_document;
    "Node list" >:: test_nodes;
  ]

