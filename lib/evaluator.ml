let evaluate_expression = function
  | Ast.Expression.IntLiteral integer -> Value.Integer integer
  | _ -> Value.Null
;;

let evaluate_statement = function
  | Ast.Statement.Expression expression -> Ok (evaluate_expression expression)
  | statement ->
    Error
      (Printf.sprintf
         "could not evaluate statement \"%s\""
         (Ast.Statement.to_string statement))
;;

let evaluate program =
  match program with
  | [] -> Error "cannot evaluate an empty program"
  | h :: _ -> evaluate_statement h
;;
