(* Alias modules for convenience *)

module Mut = Mutation

module Compose = Othello_compose

module Transform = Othello_transform

(* Infix operators *)

let (|+|) = Compose.exec

let (|**|) = Transform.exec
