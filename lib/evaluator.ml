(* TODO: Try eliminate use of result type, use Value.Error instead *)
let ( let* ) = Result.bind

let evaluate_integer_infix lhs rhs operator =
  let open Value in
  let open Ast.InfixOp in
  match operator with
  | Plus -> Integer (lhs + rhs)
  | Minus -> Integer (lhs - rhs)
  | Multiply -> Integer (lhs * rhs)
  | Divide -> if rhs = 0 then Value.Error "division by zero" else Integer (lhs / rhs)
  | Lt -> Boolean (lhs < rhs)
  | Gt -> Boolean (lhs > rhs)
  | Eq -> Boolean (lhs = rhs)
  | NotEq -> Boolean (lhs <> rhs)
;;

let evaluate_boolean_infix lhs rhs operator =
  let open Ast in
  match operator with
  | InfixOp.Eq -> Value.Boolean (lhs = rhs)
  | InfixOp.NotEq -> Value.Boolean (lhs <> rhs)
  | _ ->
    Value.Error
      (Printf.sprintf
         "unknown operator: %s %s %s"
         (Value.to_type_string (Boolean lhs))
         (InfixOp.to_string operator)
         (Value.to_type_string (Boolean rhs)))
;;

let infix_type_mismatch lhs operator rhs =
  Value.Error
    (Printf.sprintf
       "type mismatch: %s %s %s"
       (Value.to_type_string lhs)
       (Ast.InfixOp.to_string operator)
       (Value.to_type_string rhs))
;;

let unknown_prefix_operator operator rhs =
  Value.Error
    (Printf.sprintf
       "unknown operator: %s%s"
       (Ast.PrefixOp.to_string operator)
       (Value.to_type_string rhs))
;;

let unsupported_expression statement =
  Printf.sprintf
    "could not evaluate expression \"%s\""
    (Ast.Expression.to_string statement)
;;

let unsupported_statement statement =
  Printf.sprintf "could not evaluate statement \"%s\"" (Ast.Statement.to_string statement)
;;

let rec evaluate_statements statements =
  let rec loop statements value =
    match statements with
    | [] -> Ok value
    | h :: t ->
      let* value = evaluate_statement h in
      (match value with
       | Value.Return _ as return_value -> Ok return_value
       | Value.Error _ as error_value -> Ok error_value
       | value -> loop t value)
  in
  loop statements Value.Null

and evaluate_statement = function
  | Ast.Statement.Expression expression -> evaluate_expression expression
  | Ast.Statement.Return expression ->
    let* return_value = evaluate_expression expression in
    Ok (Value.Return return_value)
  | statement -> Error (unsupported_statement statement)

and evaluate_expression expression =
  let open Ast in
  let open Ast.Expression in
  match expression with
  | IntLiteral integer -> Ok (Value.Integer integer)
  | BoolLiteral boolean -> Ok (Value.Boolean boolean)
  | Prefix (PrefixOp.Bang, sub_expression) -> evaluate_bang_operator sub_expression
  | Prefix (PrefixOp.Minus, sub_expression) -> evaluate_minus_operator sub_expression
  | Infix (left, operator, right) -> evaluate_infix_expression left operator right
  | If { condition; consequent; alternative } ->
    evaluate_if_else_expression condition consequent alternative
  | expression -> Error (unsupported_expression expression)

and evaluate_bang_operator expression =
  let* value = evaluate_expression expression in
  match value with
  | Value.Boolean boolean -> Ok (Value.Boolean (not boolean))
  | value -> Ok (unknown_prefix_operator Ast.PrefixOp.Bang value)

and evaluate_minus_operator expression =
  let* value = evaluate_expression expression in
  match value with
  | Value.Integer integer -> Ok (Value.Integer (-integer))
  | value -> Ok (unknown_prefix_operator Ast.PrefixOp.Minus value)

and evaluate_infix_expression left operator right =
  let* left = evaluate_expression left in
  let* right = evaluate_expression right in
  match left, right with
  | Value.Integer lhs, Value.Integer rhs -> Ok (evaluate_integer_infix lhs rhs operator)
  | Value.Boolean lhs, Value.Boolean rhs -> Ok (evaluate_boolean_infix lhs rhs operator)
  | (Value.Error _ as error), _ | _, (Value.Error _ as error) -> Ok error
  | lhs, rhs -> Ok (infix_type_mismatch lhs operator rhs)

and evaluate_if_else_expression condition consequent alternative =
  let* condition_value = evaluate_expression condition in
  match condition_value with
  | Value.Boolean boolean ->
    if boolean
    then evaluate_statements consequent
    else (
      match alternative with
      | Some alternative -> evaluate_statements alternative
      | None -> Ok Value.Null)
  | Value.Error _ as error -> Ok error
  | value ->
    Ok
      (Value.Error (Printf.sprintf "type mismatch: if (%s)" (Value.to_type_string value)))
;;

let evaluate program =
  let rec loop program value =
    match program with
    | [] -> Ok value
    | h :: t ->
      let* value = evaluate_statement h in
      (match value with
       | Value.Return return_value -> Ok return_value
       | Value.Error _ as error_value -> Ok error_value
       | value -> loop t value)
  in
  loop program Value.Null
;;
