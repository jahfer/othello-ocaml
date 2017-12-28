open Core
open OUnit2
open Othello

(* Pure strings *)

module StringDocument = Othello.Document.Make(struct
  type 'a t = string
  type 'a u = string
  let initial = ""
  let concat x y = x ^ y

  let apply_operation doc = function
    | Othello.Mut.Insert(x) -> Some(x), doc
    | Othello.Mut.Delete    -> None, Str.string_after doc 1
    | Othello.Mut.Retain(x) -> Some(Str.string_before doc x), Str.string_after doc x
end)

let test_string_document _ =
  let doc = "ram" and
  ops = [Mut.Retain(1); Mut.Insert("o"); Mut.Retain(2); Mut.Insert("!")] in
  assert_equal "roam!" @@ StringDocument.apply doc ops

(* Rope *)

module RopeDocument = Othello.Document.Make(struct
  type 'a t = Rope.t
  type 'a u = Rope.t

  let initial = Rope.of_string ""

  let concat x y = Rope.concat x y

  let apply_operation doc = function
  | Othello.Mut.Insert(x) -> Some(x), doc
  | Othello.Mut.Delete -> let _, rhs = Rope.split_at doc 1 in None, rhs
  | Othello.Mut.Retain(x) -> let lhs, rhs = Rope.split_at doc x in Some(lhs), rhs
end)

let test_rope_document _ =
  let doc = Rope.of_string "ram" and
  ops = [Mut.Retain(1); Mut.Insert(Rope.of_string "o"); Mut.Retain(2); Mut.Insert(Rope.of_string "!")] in
  let result = RopeDocument.apply doc ops in
  assert_equal "roam!" @@ Rope.string_of_rope result

(* List *)

module ListDocument = Document.Make(struct
  type 'a t = 'a list
  type 'a u = 'a

  let initial = []
  let concat doc el = doc @ [el]

  let apply_operation doc = function
    | Othello.Mut.Insert(x) -> Some(x), doc
    | Othello.Mut.Delete    -> None, List.tl_exn doc
    | Othello.Mut.Retain(x) when x = 1 -> List.hd doc, List.tl_exn doc
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
              "Rope document" >:: test_rope_document;
              "Node list" >:: test_nodes;
            ]

