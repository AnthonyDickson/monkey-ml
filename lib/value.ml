(** The representation of values for evaluating a Monkeylang AST. *)

type t =
  | Integer of int
  | Boolean of bool
  | Null

let to_string = function
  | Integer value -> Int.to_string value
  | Boolean value -> Bool.to_string value
  | Null -> "null"
;;
