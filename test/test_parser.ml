open Monkeylang

let statement_testable =
  let pp_statement fmt statement =
    Format.pp_print_string fmt (Ast.Statement.to_string statement)
  in
  Alcotest.testable pp_statement ( = )
;;

let token_testable =
  let open Monkeylang in
  let pp_token fmt token = Format.pp_print_string fmt (Token.to_string token) in
  Alcotest.testable pp_token ( = )
;;

let check_same_statement_count expected actual =
  Printf.eprintf "%s" (String.concat "\n" (List.map Ast.Statement.to_string actual));
  Alcotest.(check int) "same statement count" (List.length expected) (List.length actual)
;;

let check_same_statements expected actual =
  check_same_statement_count expected actual;
  List.combine expected actual
  |> List.iter (fun (expected_statement, actual_statement) ->
    Alcotest.(check statement_testable)
      "same statement"
      expected_statement
      actual_statement)
;;

let test_parse_let_statement () =
  let input =
    {|
      let x = 5;
      let y = 10;
      let foobar = 838383;
    |}
  in
  let expected_program =
    [ Ast.Statement.Let { identifier = "x" }
    ; Ast.Statement.Let { identifier = "y" }
    ; Ast.Statement.Let { identifier = "foobar" }
    ]
  in
  let lexer = Result.get_ok @@ Lexer.make input in
  let parser = Parser.make lexer in
  match Parser.parse_program parser with
  | Ok actual_program -> check_same_statements expected_program actual_program
  | Error msg -> Alcotest.failf "Got unexpected error: %s" msg
;;

let test_parse_return_statement () =
  let open Ast in
  let input =
    {|
      return 5;
      return 10;
      return add(15);
    |}
  in
  let expected_program = [ Statement.Return; Statement.Return; Statement.Return ] in
  let lexer = Result.get_ok @@ Lexer.make input in
  let parser = Parser.make lexer in
  match Parser.parse_program parser with
  | Ok actual_program -> check_same_statements expected_program actual_program
  | Error msg -> Alcotest.failf "Got unexpected error: %s" msg
;;

let test_parse_literal_expression () =
  let open Ast in
  let input =
    {|
      foobar;
      5;
    |}
  in
  let expected_program =
    [ Statement.Expression (Expression.Identifier "foobar")
    ; Statement.Expression (Expression.IntLiteral 5)
    ]
  in
  let lexer = Result.get_ok @@ Lexer.make input in
  let parser = Parser.make lexer in
  match Parser.parse_program parser with
  | Ok actual_program -> check_same_statements expected_program actual_program
  | Error msg -> Alcotest.failf "Got unexpected error: %s" msg
;;

let test_parse_prefix_expression () =
  let open Ast in
  let input =
    {|
      !5;
      -15;
    |}
  in
  let expected_program =
    [ Statement.Expression (Expression.Prefix (PrefixOp.Bang, Expression.IntLiteral 5))
    ; Statement.Expression (Expression.Prefix (PrefixOp.Minus, Expression.IntLiteral 15))
    ]
  in
  let lexer = Result.get_ok @@ Lexer.make input in
  let parser = Parser.make lexer in
  match Parser.parse_program parser with
  | Ok actual_program -> check_same_statements expected_program actual_program
  | Error msg -> Alcotest.failf "Got unexpected error: %s" msg
;;

let test_parse_infix_expression () =
  let open Ast in
  let input =
    {|
      5 + 5;
      5 - 5;
      5 * 5;
      5 / 5;
      5 > 5;
      5 < 5;
      5 == 5;
      5 != 5;
    |}
  in
  let five = Expression.IntLiteral 5 in
  let make_infix operator = Expression.Infix (five, operator, five) in
  let expected_program =
    [ Statement.Expression (make_infix InfixOp.Plus)
    ; Statement.Expression (make_infix InfixOp.Minus)
    ; Statement.Expression (make_infix InfixOp.Multiply)
    ; Statement.Expression (make_infix InfixOp.Divide)
    ; Statement.Expression (make_infix InfixOp.Gt)
    ; Statement.Expression (make_infix InfixOp.Lt)
    ; Statement.Expression (make_infix InfixOp.Eq)
    ; Statement.Expression (make_infix InfixOp.NotEq)
    ]
  in
  let lexer = Result.get_ok @@ Lexer.make input in
  let parser = Parser.make lexer in
  match Parser.parse_program parser with
  | Ok actual_program -> check_same_statements expected_program actual_program
  | Error msg -> Alcotest.failf "Got unexpected error: %s" msg
;;

let test_operator_precedence () =
  let tests =
    [ "-a * b", "((-a) * b)"
    ; "!-a", "(!(-a))"
    ; "a + b + c", "((a + b) + c)"
    ; "a + b - c", "((a + b) - c)"
    ; "a * b * c", "((a * b) * c)"
    ; "a * b / c", "((a * b) / c)"
    ; "a + b / c", "(a + (b / c))"
    ; "a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"
    ; "5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"
    ; "5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"
    ; "3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
    ]
  in
  List.iter
    (fun (input, expected) ->
       let lexer = Result.get_ok @@ Lexer.make input in
       let parser = Parser.make lexer in
       match Parser.parse_program parser with
       | Ok [ Ast.Statement.Expression expr ] ->
         let actual = Ast.Expression.to_string expr in
         Alcotest.(check string) ("parse: " ^ input) expected actual
       | Ok _ -> Alcotest.failf "Expected single expression statement"
       | Error msg -> Alcotest.failf "Got unexpected error: %s" msg)
    tests
;;

let test_suite =
  [ Alcotest.test_case "let statements" `Quick test_parse_let_statement
  ; Alcotest.test_case "return statements" `Quick test_parse_return_statement
  ; Alcotest.test_case
      "literal expression statements"
      `Quick
      test_parse_literal_expression
  ; Alcotest.test_case "prefix expression statements" `Quick test_parse_prefix_expression
  ; Alcotest.test_case "infix expression statements" `Quick test_parse_infix_expression
  ; Alcotest.test_case "operator precedence" `Quick test_operator_precedence
  ]
;;
