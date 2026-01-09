module Expression = Ast.Expression
module InfixOp = Ast.InfixOp
module PrefixOp = Ast.PrefixOp
module Statement = Ast.Statement

let infix_type_mismatch lhs operator rhs =
  Value.Error
    (Printf.sprintf
       "type mismatch: %s %s %s"
       (Value.to_type_string lhs)
       (InfixOp.to_string operator)
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
       (PrefixOp.to_string operator)
       (Value.to_type_string rhs))
;;

let identifier_not_found identifier =
  Value.Error (Printf.sprintf "identifier not found: %s" identifier)
;;

let invalid_function value =
  Value.Error
    (Printf.sprintf
       "cannot use %s for function application: %s"
       (Value.to_type_string value)
       (Value.to_string value))
;;

let evaluate_integer_infix lhs rhs operator =
  let open Value in
  let open InfixOp in
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

let evaluate_return_statement env identifier =
  match Environment.get env identifier with
  | Some value -> env, value
  | None -> env, identifier_not_found identifier
;;

let rec evaluate_statements env statements =
  let rec loop env statements value =
    match statements with
    | [] -> env, value
    | h :: t ->
      let env, value = evaluate_statement env h in
      (match value with
       | Value.Return _ as return_value -> env, return_value
       | Value.Error _ as error_value -> env, error_value
       | value -> loop env t value)
  in
  loop env statements Value.Null

and evaluate_statement env = function
  | Statement.Expression expression -> evaluate_expression env expression
  | Statement.Return expression ->
    let env, return_value = evaluate_expression env expression in
    env, Value.Return return_value
  | Statement.Let { identifier; expression } ->
    let env, value = evaluate_expression env expression in
    Environment.bind env identifier value, value

and evaluate_expressions env expressions =
  let rec loop env expressions values =
    match expressions with
    | [] -> env, Ok (List.rev values)
    | h :: t ->
      let env, value = evaluate_expression env h in
      (match value with
       | Value.Error _ as error -> env, Error error
       | value -> loop env t (value :: values))
  in
  loop env expressions []

and evaluate_expression env expression =
  let open Expression in
  match expression with
  | IntLiteral integer -> env, Value.Integer integer
  | BoolLiteral boolean -> env, Value.Boolean boolean
  | Prefix (PrefixOp.Bang, sub_expression) -> evaluate_bang_operator env sub_expression
  | Prefix (PrefixOp.Minus, sub_expression) -> evaluate_minus_operator env sub_expression
  | Infix (left, operator, right) -> evaluate_infix_expression env left operator right
  | If { condition; consequent; alternative } ->
    evaluate_if_else_expression env condition consequent alternative
  | Identifier identifier -> evaluate_return_statement env identifier
  | FunctionLiteral { parameters; body } ->
    env, Value.Function { parameters; body; environment = env }
  | Call { func; arguments } -> env, evaluate_function_application env func arguments

and evaluate_bang_operator env expression =
  let env, value = evaluate_expression env expression in
  ( env
  , match value with
    | Value.Boolean boolean -> Value.Boolean (not boolean)
    | value -> unknown_prefix_operator PrefixOp.Bang value )

and evaluate_minus_operator env expression =
  let env, value = evaluate_expression env expression in
  ( env
  , match value with
    | Value.Integer integer -> Value.Integer (-integer)
    | value -> unknown_prefix_operator PrefixOp.Minus value )

and evaluate_infix_expression env left operator right =
  let env, left = evaluate_expression env left in
  let env, right = evaluate_expression env right in
  ( env
  , match left, right with
    | Value.Integer lhs, Value.Integer rhs -> evaluate_integer_infix lhs rhs operator
    | Value.Boolean lhs, Value.Boolean rhs -> evaluate_boolean_infix lhs rhs operator
    | (Value.Error _ as error), _ | _, (Value.Error _ as error) -> error
    | lhs, rhs -> infix_type_mismatch lhs operator rhs )

and evaluate_if_else_expression env condition consequent alternative =
  let env, condition_value = evaluate_expression env condition in
  match condition_value with
  | Value.Boolean boolean ->
    if boolean
    then evaluate_statements env consequent
    else (
      match alternative with
      | Some alternative -> evaluate_statements env alternative
      | None -> env, Value.Null)
  | Value.Error _ as error -> env, error
  | value -> env, if_type_mismatch value

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
    let _, args = evaluate_expressions env arguments in
    match args with
    | Error error -> error
    | Ok args ->
      let fn_env =
        Environment.union (make_function_environment parameters args) fn_environment
      in
      let _, value = evaluate_statements fn_env body in
      unwrap_return_value value
  in
  let _, func = evaluate_expression env fn in
  match func with
  | Value.Function { parameters; body; environment = fn_environment } ->
    apply_function env parameters body fn_environment
  | Value.Error _ as error -> error
  | value -> invalid_function value
;;

let evaluate env program =
  let rec loop env program value =
    match program with
    | [] -> env, value
    | h :: t ->
      let env, value = evaluate_statement env h in
      (match value with
       | Value.Return return_value -> env, return_value
       | Value.Error _ as error_value -> env, error_value
       | value -> loop env t value)
  in
  loop env program Value.Null
;;
