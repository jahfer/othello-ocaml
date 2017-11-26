(* Alias modules for convenience *)

module Mut = Mutation

module Compose = Othello_compose

module Transform = Othello_transform

(* Infix operators *)

let (|+|) = Compose.exec

let (|**|) = Transform.exec

(* The meat *)

module StringDocument = Document.Make(struct
  type t = string
  type u = string
  let initial = ""
  let append_to_final x y = x ^ y

  let apply_operation doc = function
    | Mutation.Insert(x) -> Some(x), doc
    | Mutation.Delete    -> None, Str.string_after doc 1
    | Mutation.Retain(x) -> Some(Str.string_before doc x), Str.string_after doc x
end)

module NodeDocument = struct
  type node_type = Paragraph of string
  type node = { kind : node_type }
  let print_nodes nodes =
    let print_node = (fun n ->
      match n.kind with Paragraph(str) -> print_string str) in
    let () = print_string "\n" in
    List.map print_node

  type t = node list
  type u = node

  let initial = []
  let append_to_final doc node = doc @ [node]

  let apply_operation doc = function
    | Mutation.Insert(x) -> Some(x), doc
    | Mutation.Delete    -> None, List.tl doc
    | Mutation.Retain(x) when x = 1 -> Some(List.hd doc), List.tl doc
    | _ -> raise (Invalid_argument "Bad instruction")
end

module StructuredDocument = Document.Make(NodeDocument)
