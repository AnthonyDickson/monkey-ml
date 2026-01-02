type identifier = string

module PrefixOp = struct
  type t =
    | Bang
    | Minus

  let from_token = function
    | Token.Bang -> Bang
    | Token.Minus -> Minus
    | _ -> assert false
  ;;

  let to_token = function
    | Bang -> Token.Bang
    | Minus -> Token.Minus
  ;;

  let to_string operator = operator |> to_token |> Token.to_string
end

module InfixOp = struct
  type t =
    | Plus
    | Minus
    | Multiply
    | Divide
    | Eq
    | NotEq
    | Lt
    | Gt

  let from_token = function
    | Token.Plus -> Plus
    | Token.Minus -> Minus
    | Token.Asterisk -> Multiply
    | Token.Slash -> Divide
    | Token.Eq -> Eq
    | Token.NotEq -> NotEq
    | Token.Lt -> Lt
    | Token.Gt -> Gt
    | _ -> assert false
  ;;

  let to_token = function
    | Plus -> Token.Plus
    | Minus -> Token.Minus
    | Multiply -> Token.Asterisk
    | Divide -> Token.Slash
    | Eq -> Token.Eq
    | NotEq -> Token.NotEq
    | Lt -> Token.Lt
    | Gt -> Token.Gt
  ;;

  let to_string operator = operator |> to_token |> Token.to_string
end

module rec Statement : sig
  type t =
    | Let of { identifier : identifier }
    | Return
    | Expression of Expression.t

  val to_string : t -> string
end = struct
  type t =
    | Let of { identifier : identifier }
    | Return
    | Expression of Expression.t

  let to_string = function
    | Let { identifier = indentifier' } -> Printf.sprintf "let %s = _" indentifier'
    | Return -> "return _"
    | Expression expr -> Expression.to_string expr
  ;;
end

and Expression : sig
  type t =
    | Identifier of identifier
    | IntLiteral of int
    | BoolLiteral of bool
    | Prefix of PrefixOp.t * t
    | Infix of t * InfixOp.t * t
    | If of
        { condition : t
        ; consequent : Statement.t list
        ; alternative : Statement.t list option
        }
    | Fn of
        { arguments : identifier list
        ; body : Statement.t list
        }

  val to_string : t -> string
end = struct
  type t =
    | Identifier of identifier
    | IntLiteral of int
    | BoolLiteral of bool
    | Prefix of PrefixOp.t * t
    | Infix of t * InfixOp.t * t
    | If of
        { condition : t
        ; consequent : Statement.t list
        ; alternative : Statement.t list option
        }
    | Fn of
        { arguments : identifier list
        ; body : Statement.t list
        }

  let rec to_string = function
    | Identifier ident -> ident
    | IntLiteral integer -> Int.to_string integer
    | BoolLiteral bool -> Bool.to_string bool
    | Prefix (operator, expression) ->
      Printf.sprintf "(%s%s)" (PrefixOp.to_string operator) (to_string expression)
    | Infix (lhs, operator, rhs) ->
      Printf.sprintf
        "(%s %s %s)"
        (to_string lhs)
        (InfixOp.to_string operator)
        (to_string rhs)
    | If { condition; consequent; alternative } ->
      let condition = to_string condition
      and consequence = String.concat "; " (List.map Statement.to_string consequent) in
      (match alternative with
       | Some expressions ->
         let alternative =
           String.concat "; " (List.map Statement.to_string expressions)
         in
         Printf.sprintf "(if (%s) { %s } else { %s })" condition consequence alternative
       | None -> Printf.sprintf "(if (%s) { %s })" condition consequence)
    | Fn { arguments; body } ->
      Printf.sprintf
        "(fn (%s) { %s })"
        (String.concat ", " arguments)
        (String.concat "; " @@ List.map Statement.to_string body)
  ;;
end

type program = Statement.t list
