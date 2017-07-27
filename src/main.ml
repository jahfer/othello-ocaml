open Core

let rec print_list = function
  | [] -> print_string "\n"
  | hd::tl -> print_string hd ; print_string " " ; print_list tl

let print_oplist l =
  let strings = List.map ~f:Operation.to_string l in
  print_list strings

 let () =
  let ops1 = [Operation.Retain(2); Operation.Insert('a')] in
  let ops2 = [Operation.Retain(2); Operation.Insert('t')] in
  let xf1, xf2 = Transform.transform_operations ops1 ops2 in
  let () = print_oplist xf1 in
  print_oplist xf2

  let () =
  let ops1 = [Operation.Insert('a'); Operation.Retain(1)] in
  let ops2 = [Operation.Retain(2); Operation.Insert('b')] in
  let composed = Compose.compose_operations ops1 ops2 in
  let strings = List.map ~f:Operation.to_string composed in
  print_list strings
