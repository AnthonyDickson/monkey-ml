(** The representation of values for evaluating a Monkeylang AST. *)

type t =
  | Integer of int
  | Boolean of bool
  | Return of t
  | Error of string
  | Null

(** Get the string representation of a value *)
let rec to_string = function
  | Integer value -> Int.to_string value
  | Boolean value -> Bool.to_string value
  | Return value -> to_string value
  | Error value -> value
  | Null -> "null"
;;

(** Get the string representation of the type of a value *)
let to_type_string = function
  | Integer _ -> "INTEGER"
  | Boolean _ -> "BOOLEAN"
  | Return _ -> "RETURN"
  | Error _ -> "ERROR"
  | Null -> "NULL"
