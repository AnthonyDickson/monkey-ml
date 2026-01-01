type identifier = string
type expression = Identifier of identifier | IntLiteral of int

let string_of_expression = function
  | Identifier ident -> ident
  | IntLiteral integer -> Int.to_string integer
;;

type statement =
  | Let of { identifier : identifier }
  | Return
  | Expression of expression

let string_of_statement = function
  | Let { identifier = indentifier' } -> Printf.sprintf "let %s = _" indentifier'
  | Return -> "return _"
  | Expression expr -> string_of_expression expr
;;

type program = statement list
