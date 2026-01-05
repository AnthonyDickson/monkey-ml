let ( let* ) = Result.bind

let rec evaluate_expression expression =
  let open Ast in
  let open Ast.Expression in
  match expression with
  | IntLiteral integer -> Ok (Value.Integer integer)
  | BoolLiteral boolean -> Ok (Value.Boolean boolean)
  | Prefix (PrefixOp.Bang, sub_expression) -> evaluate_bang_operator sub_expression
  | Prefix (PrefixOp.Minus, sub_expression) -> evaluate_minus_operator sub_expression
  | Infix (left, operator, right) -> evaluate_infix_expression left operator right
  | _ ->
    Error
      (Printf.sprintf
         "could not evaluate expression: \"%s\""
         (Expression.to_string expression))

and evaluate_bang_operator expression =
  match evaluate_expression expression with
  | Ok (Value.Boolean boolean) -> Ok (Value.Boolean (not boolean))
  | Ok other ->
    Error
      (Printf.sprintf
         "cannot use prefix \"%s\" on value \"%s\""
         (Token.to_string Token.Bang)
         (Value.to_string other))
  | Error error -> Error error

and evaluate_minus_operator expression =
  match evaluate_expression expression with
  | Ok (Value.Integer integer) -> Ok (Value.Integer (-integer))
  | Ok other ->
    Error
      (Printf.sprintf
         "cannot use prefix \"%s\" on value \"%s\""
         (Token.to_string Token.Bang)
         (Value.to_string other))
  | Error error -> Error error

and evaluate_infix_expression left operator right =
  let open Ast in
  let open Value in
  let* left = evaluate_expression left in
  let* right = evaluate_expression right in
  match left, right, operator with
  (* Integers *)
  | Integer l, Integer r, InfixOp.Plus -> Ok (Integer (l + r))
  | Integer l, Integer r, InfixOp.Minus -> Ok (Integer (l - r))
  | Integer l, Integer r, InfixOp.Multiply -> Ok (Integer (l * r))
  | Integer l, Integer r, InfixOp.Divide -> Ok (Integer (l / r))
  | Integer l, Integer r, InfixOp.Lt -> Ok (Boolean (l < r))
  | Integer l, Integer r, InfixOp.Gt -> Ok (Boolean (l > r))
  | Integer l, Integer r, InfixOp.Eq -> Ok (Boolean (l = r))
  | Integer l, Integer r, InfixOp.NotEq -> Ok (Boolean (l <> r))
  (* Booleans *)
  | Boolean l, Boolean r, InfixOp.Eq -> Ok (Boolean (l = r))
  | Boolean l, Boolean r, InfixOp.NotEq -> Ok (Boolean (l <> r))
  | _, _, _ ->
    Error
      (Printf.sprintf
         "cannot apply operator \"%s\" to \"%s\" and \"%s\""
         (InfixOp.to_string operator)
         (Value.to_string left)
         (Value.to_string right))
;;

let evaluate_statement = function
  | Ast.Statement.Expression expression -> evaluate_expression expression
  | statement ->
    Error
      (Printf.sprintf
         "could not evaluate statement \"%s\""
         (Ast.Statement.to_string statement))
;;

let evaluate program =
  let rec loop program value =
    match program with
    | [] -> Ok value
    | h :: t ->
      let* value = evaluate_statement h in
      loop t (Some value)
  in
  let* value_opt = loop program None in
  match value_opt with
  | Some value -> Ok value
  | None -> Error "program did evaluate to a value"
;;
