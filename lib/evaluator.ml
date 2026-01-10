module Expression = Ast.Expression
module InfixOp = Ast.InfixOp
module PrefixOp = Ast.PrefixOp
module Statement = Ast.Statement

let ( let* ) = Result.bind

let infix_type_mismatch lhs operator rhs =
  Printf.sprintf
    "type mismatch: %s %s %s"
    (Value.to_type_string lhs)
    (InfixOp.to_string operator)
    (Value.to_type_string rhs)
;;

let if_type_mismatch condition_value =
  Printf.sprintf "type mismatch: if (%s)" (Value.to_type_string condition_value)
;;

let unknown_prefix_operator operator rhs =
  Printf.sprintf
    "unknown operator: %s%s"
    (PrefixOp.to_string operator)
    (Value.to_type_string rhs)
;;

let unknown_infix_operator lhs operator rhs =
  Printf.sprintf
    "unknown operator: %s %s %s"
    (Value.to_type_string lhs)
    (InfixOp.to_string operator)
    (Value.to_type_string rhs)
;;

let identifier_not_found identifier = Printf.sprintf "identifier not found: %s" identifier

let invalid_function value =
  Printf.sprintf
    "cannot use %s for function application: %s"
    (Value.to_type_string value)
    (Value.to_string value)
;;

let evaluate_integer_infix lhs rhs operator =
  let open Value in
  let open InfixOp in
  match operator with
  | Plus -> Ok (Integer (lhs + rhs))
  | Minus -> Ok (Integer (lhs - rhs))
  | Multiply -> Ok (Integer (lhs * rhs))
  | Divide -> if rhs = 0 then Error "division by zero" else Ok (Integer (lhs / rhs))
  | Lt -> Ok (Boolean (lhs < rhs))
  | Gt -> Ok (Boolean (lhs > rhs))
  | Eq -> Ok (Boolean (lhs = rhs))
  | NotEq -> Ok (Boolean (lhs <> rhs))
;;

let evaluate_boolean_infix lhs rhs operator =
  match operator with
  | InfixOp.Eq -> Ok (Value.Boolean (lhs = rhs))
  | InfixOp.NotEq -> Ok (Value.Boolean (lhs <> rhs))
  | _ -> Error (unknown_infix_operator (Boolean lhs) operator (Boolean rhs))
;;

let evaluate_identifier env identifier =
  match Environment.get env identifier with
  | Some value -> Ok (env, value)
  | None -> Error (identifier_not_found identifier)
;;

let rec evaluate_statements env statements =
  let rec loop env statements value =
    match statements with
    | [] -> Ok (env, value)
    | h :: t ->
      let* env, value = evaluate_statement env h in
      (match value with
       | Value.Return _ as return_value -> Ok (env, return_value)
       | value -> loop env t value)
  in
  loop env statements Value.Null

and evaluate_statement env = function
  | Statement.Expression expression -> evaluate_expression env expression
  | Statement.Return expression ->
    let* env, return_value = evaluate_expression env expression in
    Ok (env, Value.Return return_value)
  | Statement.Let { identifier; expression } ->
    let* env, value = evaluate_expression env expression in
    Ok (Environment.bind env identifier value, value)

and evaluate_expressions env expressions =
  let rec loop env expressions values =
    match expressions with
    | [] -> Ok (env, List.rev values)
    | h :: t ->
      let* env, value = evaluate_expression env h in
      loop env t (value :: values)
  in
  loop env expressions []

and evaluate_expression env expression =
  let open Expression in
  match expression with
  | IntLiteral integer -> Ok (env, Value.Integer integer)
  | BoolLiteral boolean -> Ok (env, Value.Boolean boolean)
  | Prefix (PrefixOp.Bang, sub_expression) -> evaluate_bang_operator env sub_expression
  | Prefix (PrefixOp.Minus, sub_expression) -> evaluate_minus_operator env sub_expression
  | Infix (left, operator, right) -> evaluate_infix_expression env left operator right
  | If { condition; consequent; alternative } ->
    evaluate_if_else_expression env condition consequent alternative
  | Identifier identifier -> evaluate_identifier env identifier
  | FunctionLiteral { parameters; body } ->
    Ok (env, Value.Function { parameters; body; environment = env })
  | Call { func; arguments } -> evaluate_function_application env func arguments

and evaluate_bang_operator env expression =
  let* env, value = evaluate_expression env expression in
  match value with
  | Value.Boolean boolean -> Ok (env, Value.Boolean (not boolean))
  | value -> Error (unknown_prefix_operator PrefixOp.Bang value)

and evaluate_minus_operator env expression =
  let* env, value = evaluate_expression env expression in
  match value with
  | Value.Integer integer -> Ok (env, Value.Integer (-integer))
  | value -> Error (unknown_prefix_operator PrefixOp.Minus value)

and evaluate_infix_expression env left operator right =
  let* env, left = evaluate_expression env left in
  let* env, right = evaluate_expression env right in
  match left, right with
  | Value.Integer lhs, Value.Integer rhs ->
    let* result = evaluate_integer_infix lhs rhs operator in
    Ok (env, result)
  | Value.Boolean lhs, Value.Boolean rhs ->
    let* result = evaluate_boolean_infix lhs rhs operator in
    Ok (env, result)
  | lhs, rhs -> Error (infix_type_mismatch lhs operator rhs)

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
  | value -> Error (if_type_mismatch value)

and evaluate_function_application env fn arguments =
  let make_function_environment parameters args =
    List.fold_left
      (fun acc (param, arg) -> Environment.bind acc param arg)
      (Environment.make ())
      (List.combine parameters args)
  and unwrap_return_value = function
    | Value.Return return_value -> return_value
    | value -> value
  in
  let apply_function env parameters body fn_environment =
    let* _, args = evaluate_expressions env arguments in
    let fn_env =
      Environment.union (make_function_environment parameters args) fn_environment
    in
    let* _, value = evaluate_statements fn_env body in
    Ok (unwrap_return_value value)
  in
  let* _, func = evaluate_expression env fn in
  match func with
  | Value.Function { parameters; body; environment = fn_environment } ->
    let* result = apply_function env parameters body fn_environment in
    Ok (env, result)
  | value -> Error (invalid_function value)
;;

let evaluate env program =
  let rec loop env program value =
    match program with
    | [] -> Ok (env, value)
    | h :: t ->
      let* env, value = evaluate_statement env h in
      (match value with
       | Value.Return return_value -> Ok (env, return_value)
       | value -> loop env t value)
  in
  loop env program Value.Null
;;
