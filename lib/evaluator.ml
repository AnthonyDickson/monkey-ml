(* TODO: Try eliminate use of result type, use Value.Error instead *)
let ( let* ) = Result.bind

let infix_type_mismatch lhs operator rhs =
  Value.Error
    (Printf.sprintf
       "type mismatch: %s %s %s"
       (Value.to_type_string lhs)
       (Ast.InfixOp.to_string operator)
       (Value.to_type_string rhs))
;;

let if_type_mismatch condition_value =
  Value.Error
    (Printf.sprintf "type mismatch: if (%s)" (Value.to_type_string condition_value))
;;

let unknown_prefix_operator operator rhs =
  Value.Error
    (Printf.sprintf
       "unknown operator: %s%s"
       (Ast.PrefixOp.to_string operator)
       (Value.to_type_string rhs))
;;

let identifier_not_found identifier =
  Value.Error (Printf.sprintf "identifier not found: %s" identifier)
;;

let unsupported_expression statement =
  Printf.sprintf
    "could not evaluate expression \"%s\""
    (Ast.Expression.to_string statement)
;;

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

let rec evaluate_statements env statements =
  let rec loop env statements value =
    match statements with
    | [] -> Ok (env, value)
    | h :: t ->
      let* env, value = evaluate_statement env h in
      (match value with
       | Value.Return _ as return_value -> Ok (env, return_value)
       | Value.Error _ as error_value -> Ok (env, error_value)
       | value -> loop env t value)
  in
  loop env statements Value.Null

and evaluate_statement env = function
  | Ast.Statement.Expression expression -> evaluate_expression env expression
  | Ast.Statement.Return expression ->
    let* env, return_value = evaluate_expression env expression in
    Ok (env, Value.Return return_value)
  | Ast.Statement.Let { identifier; expression } ->
    let* env, value = evaluate_expression env expression in
    Ok (Environment.bind env identifier value, value)

and evaluate_expression env expression =
  let open Ast in
  let open Ast.Expression in
  match expression with
  | IntLiteral integer -> Ok (env, Value.Integer integer)
  | BoolLiteral boolean -> Ok (env, Value.Boolean boolean)
  | Prefix (PrefixOp.Bang, sub_expression) -> evaluate_bang_operator env sub_expression
  | Prefix (PrefixOp.Minus, sub_expression) -> evaluate_minus_operator env sub_expression
  | Infix (left, operator, right) -> evaluate_infix_expression env left operator right
  | If { condition; consequent; alternative } ->
    evaluate_if_else_expression env condition consequent alternative
  | Identifier identifier ->
    (match Environment.get env identifier with
     | Some value -> Ok (env, value)
     | None -> Ok (env, identifier_not_found identifier))
  | expression -> Error (unsupported_expression expression)

and evaluate_bang_operator env expression =
  let* env, value = evaluate_expression env expression in
  Ok
    ( env
    , match value with
      | Value.Boolean boolean -> Value.Boolean (not boolean)
      | value -> unknown_prefix_operator Ast.PrefixOp.Bang value )

and evaluate_minus_operator env expression =
  let* env, value = evaluate_expression env expression in
  Ok
    ( env
    , match value with
      | Value.Integer integer -> Value.Integer (-integer)
      | value -> unknown_prefix_operator Ast.PrefixOp.Minus value )

and evaluate_infix_expression env left operator right =
  let* env, left = evaluate_expression env left in
  let* env, right = evaluate_expression env right in
  Ok
    ( env
    , match left, right with
      | Value.Integer lhs, Value.Integer rhs -> evaluate_integer_infix lhs rhs operator
      | Value.Boolean lhs, Value.Boolean rhs -> evaluate_boolean_infix lhs rhs operator
      | (Value.Error _ as error), _ | _, (Value.Error _ as error) -> error
      | lhs, rhs -> infix_type_mismatch lhs operator rhs )

and evaluate_if_else_expression env condition consequent alternative =
  let* env, condition_value = evaluate_expression env condition in
  match condition_value with
  | Value.Boolean boolean ->
    if boolean
    then evaluate_statements env consequent
    else (
      match alternative with
      | Some alternative -> evaluate_statements env alternative
      | None -> Ok (env, Value.Null))
  | Value.Error _ as error -> Ok (env, error)
  | value -> Ok (env, if_type_mismatch value)
;;

let evaluate env program =
  let rec loop env program value =
    match program with
    | [] -> Ok (env, value)
    | h :: t ->
      let* env, value = evaluate_statement env h in
      (match value with
       | Value.Return return_value -> Ok (env, return_value)
       | Value.Error _ as error_value -> Ok (env, error_value)
       | value -> loop env t value)
  in
  loop env program Value.Null
;;
