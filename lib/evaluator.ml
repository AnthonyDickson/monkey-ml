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

let incorrect_arity parameter_count argument_count =
  Printf.sprintf
    "wrong number of arguments: expected %d, got %d"
    parameter_count
    argument_count
;;

let make_function_environment parameters args =
  List.fold_left2
    (fun acc param arg -> Environment.bind acc param arg)
    (Environment.make ())
    parameters
    args
;;

let unwrap_return_value = function
  | Value.Return return_value -> return_value
  | value -> value
;;

let check_arity parameters args =
  let param_count = List.length parameters in
  let arg_count = List.length args in
  if param_count = arg_count then Ok () else Error (incorrect_arity param_count arg_count)
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
  | _ -> Error (unknown_infix_operator (Value.Boolean lhs) operator (Value.Boolean rhs))
;;

let evaluate_string_infix lhs rhs operator =
  match operator with
  | InfixOp.Plus -> Ok (Value.String (lhs ^ rhs))
  | _ -> Error (unknown_infix_operator (Value.String lhs) operator (Value.String rhs))
;;

let evaluate_identifier env identifier =
  match Environment.get env identifier with
  | Some value -> Ok value
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
  | Statement.Expression expression ->
    let* value = evaluate_expression env expression in
    Ok (env, value)
  | Statement.Return expression ->
    let* return_value = evaluate_expression env expression in
    Ok (env, Value.Return return_value)
  | Statement.Let { identifier; expression } ->
    let* value = evaluate_expression env expression in
    Ok (Environment.bind env identifier value, value)

and evaluate_expressions env expressions =
  let rec loop env expressions values =
    match expressions with
    | [] -> Ok (List.rev values)
    | h :: t ->
      let* value = evaluate_expression env h in
      loop env t (value :: values)
  in
  loop env expressions []

and evaluate_expression env expression =
  let open Expression in
  match expression with
  | IntLiteral integer -> Ok (Value.Integer integer)
  | BoolLiteral boolean -> Ok (Value.Boolean boolean)
  | StringLiteral str -> Ok (Value.String str)
  | Prefix (PrefixOp.Bang, sub_expression) -> evaluate_bang_operator env sub_expression
  | Prefix (PrefixOp.Minus, sub_expression) -> evaluate_minus_operator env sub_expression
  | Infix (left, operator, right) -> evaluate_infix_expression env left operator right
  | If { condition; consequent; alternative } ->
    evaluate_if_else_expression env condition consequent alternative
  | Identifier identifier -> evaluate_identifier env identifier
  | FunctionLiteral { parameters; body } ->
    Ok (Value.Function { parameters; body; environment = env })
  | Call { func; arguments } -> evaluate_function_application env func arguments

and evaluate_bang_operator env expression =
  let* value = evaluate_expression env expression in
  match value with
  | Value.Boolean boolean -> Ok (Value.Boolean (not boolean))
  | value -> Error (unknown_prefix_operator PrefixOp.Bang value)

and evaluate_minus_operator env expression =
  let* value = evaluate_expression env expression in
  match value with
  | Value.Integer integer -> Ok (Value.Integer (-integer))
  | value -> Error (unknown_prefix_operator PrefixOp.Minus value)

and evaluate_infix_expression env left operator right =
  let* left = evaluate_expression env left in
  let* right = evaluate_expression env right in
  match left, right with
  | Value.Integer lhs, Value.Integer rhs ->
    let* result = evaluate_integer_infix lhs rhs operator in
    Ok result
  | Value.Boolean lhs, Value.Boolean rhs ->
    let* result = evaluate_boolean_infix lhs rhs operator in
    Ok result
  | Value.String lhs, Value.String rhs ->
    let* result = evaluate_string_infix lhs rhs operator in
    Ok result
  | lhs, rhs -> Error (infix_type_mismatch lhs operator rhs)

and evaluate_if_else_expression env condition consequent alternative =
  let* condition_value = evaluate_expression env condition in
  match condition_value with
  | Value.Boolean boolean ->
    if boolean
    then
      let* _, value = evaluate_statements env consequent in
      Ok value
    else (
      match alternative with
      | Some alternative ->
        let* _, value = evaluate_statements env alternative in
        Ok value
      | None -> Ok Value.Null)
  | value -> Error (if_type_mismatch value)

and evaluate_function_application env fn arguments =
  let apply_function env parameters body fn_environment =
    let* args = evaluate_expressions env arguments in
    let* () = check_arity parameters args in
    let fn_env =
      Environment.extend
        ~base:fn_environment
        ~bindings:(make_function_environment parameters args)
    in
    let* _, value = evaluate_statements fn_env body in
    Ok (unwrap_return_value value)
  in
  let* func = evaluate_expression env fn in
  match func with
  | Value.Function { parameters; body; environment = fn_environment } ->
    apply_function env parameters body fn_environment
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
