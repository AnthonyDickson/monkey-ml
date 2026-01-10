(** The representation of values for evaluating a Monkeylang AST. *)

type t =
  | Integer of int
  | Boolean of bool
  | Return of t
  | Function of
      { parameters : Ast.identifier list
      ; body : Ast.Statement.t list
      ; environment : t Environment.t
      }
  | Null

let function_to_string parameters body =
  Printf.sprintf
    "fn(%s) {\n%s\n}"
    (String.concat "\n" parameters)
    (Ast.Program.to_string body)
;;

(** Get the string representation of a value *)
let rec to_string = function
  | Integer value -> Int.to_string value
  | Boolean value -> Bool.to_string value
  | Return value -> to_string value
  | Null -> "null"
  | Function { parameters; body; _ } -> function_to_string parameters body
;;

(** Get the string representation of the type of a value *)
let to_type_string = function
  | Integer _ -> "INTEGER"
  | Boolean _ -> "BOOLEAN"
  | Return _ -> "RETURN"
  | Function _ -> "FUNCTION"
  | Null -> "NULL"
;;
