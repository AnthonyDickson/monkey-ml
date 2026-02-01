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
    | Let of
        { identifier : identifier
        ; expression : Expression.t
        }
    | Return of Expression.t
    | Expression of Expression.t

  val to_string : t -> string
end = struct
  type t =
    | Let of
        { identifier : identifier
        ; expression : Expression.t
        }
    | Return of Expression.t
    | Expression of Expression.t

  let to_string = function
    | Let { identifier; expression } ->
      Printf.sprintf "let %s = %s" identifier (Expression.to_string expression)
    | Return expr -> Printf.sprintf "return %s" (Expression.to_string expr)
    | Expression expr -> Expression.to_string expr
  ;;
end

and Expression : sig
  type t =
    | Identifier of identifier
    | IntLiteral of int
    | BoolLiteral of bool
    | StringLiteral of string
    | ArrayLiteral of t list
    | Index of { left: t; index: t}
    | Prefix of PrefixOp.t * t
    | Infix of t * InfixOp.t * t
    | If of
        { condition : t
        ; consequent : Statement.t list
        ; alternative : Statement.t list option
        }
    | FunctionLiteral of
        { parameters : identifier list
        ; body : Statement.t list
        }
    | Call of
        { func : t
        ; arguments : t list
        }

  val to_string : t -> string
end = struct
  type t =
    | Identifier of identifier
    | IntLiteral of int
    | BoolLiteral of bool
    | StringLiteral of string
    | ArrayLiteral of t list
    | Index of { left: t; index: t}
    | Prefix of PrefixOp.t * t
    | Infix of t * InfixOp.t * t
    | If of
        { condition : t
        ; consequent : Statement.t list
        ; alternative : Statement.t list option
        }
    | FunctionLiteral of
        { parameters : identifier list
        ; body : Statement.t list
        }
    | Call of
        { func : t
        ; arguments : t list
        }

  let rec to_string = function
    | Identifier ident -> ident
    | IntLiteral integer -> Int.to_string integer
    | BoolLiteral bool -> Bool.to_string bool
    (* Print string literal with surrounding quotes, this helps differentiate
      string literals from identifiers *)
    | StringLiteral str -> Printf.sprintf {|"%s"|} str
    | ArrayLiteral elements -> Printf.sprintf "[%s]" (String.concat ", " (List.map to_string elements))
    | Index {left; index} -> Printf.sprintf "(%s[%s])" (to_string left) (to_string index)
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
    | FunctionLiteral { parameters; body } ->
      Printf.sprintf
        "(fn (%s) { %s })"
        (String.concat ", " parameters)
        (String.concat "; " @@ List.map Statement.to_string body)
    | Call { func; arguments } ->
      Printf.sprintf
        "%s(%s)"
        (to_string func)
        (String.concat ", " (List.map to_string arguments))
  ;;
end

module Program = struct
  type t = Statement.t list

  let to_string program = String.concat "\n" (List.map Statement.to_string program)
end
