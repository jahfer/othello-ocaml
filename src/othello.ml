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
  let initial = ""
  let append_to_final x y = x ^ y

  let apply_operation doc = function
    | Mutation.Insert(x) -> Some(x), doc
    | Mutation.Delete    -> None, Str.string_after doc 1
    | Mutation.Retain(x) -> Some(Str.string_before doc x), Str.string_after doc x
end)
