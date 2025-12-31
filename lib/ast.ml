type identifier = string
type statement = Let of { identifier : identifier }

let string_of_statement = function
  | Let { identifier = indentifier' } -> Printf.sprintf "let %s" indentifier'
;;

type program = statement list
