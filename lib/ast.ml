type identifier = string
type statement = Let of { identifier : identifier } | Return

let string_of_statement = function
  | Let { identifier = indentifier' } -> Printf.sprintf "let %s" indentifier'
  | Return -> "return _"
;;

type program = statement list
