type identifier = string

module Expression = struct
  type expression =
    | Identifier of identifier
    | IntLiteral of int
    | Prefix of Token.token * expression

  let rec to_string = function
    | Identifier ident -> ident
    | IntLiteral integer -> Int.to_string integer
    | Prefix (operator, expression) -> Token.to_string operator ^ to_string expression
  ;;
end

module Statement = struct
  type statement =
    | Let of { identifier : identifier }
    | Return
    | Expression of Expression.expression

  let to_string = function
    | Let { identifier = indentifier' } -> Printf.sprintf "let %s = _" indentifier'
    | Return -> "return _"
    | Expression expr -> Expression.to_string expr
  ;;
end

type program = Statement.statement list
