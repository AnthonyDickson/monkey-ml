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

let run_parser_tests tests to_string =
  List.iter
    (fun (input, expected) ->
       let lexer = Result.get_ok @@ Lexer.make input in
       let parser = Parser.make lexer in
       match Parser.parse_program parser with
       | Ok [ statement ] ->
         let actual = to_string statement in
         Alcotest.(check string) ("parse: " ^ input) expected actual
       | Ok _ -> Alcotest.failf "Expected single statement"
       | Error (actual, msg) ->
         Alcotest.failf
           "Got unexpected error for input \"%s\": %s\nOutput was: \"%s\""
           input
           msg
           (String.concat "\n" (List.map to_string actual)))
    tests
;;

let test_parse_literal_expression () =
  let tests = [ "foobar;", "foobar"; "5;", "5"; "true;", "true"; "false;", "false" ] in
  run_parser_tests tests (fun statement ->
    match statement with
    | Ast.Statement.Expression expr -> Ast.Expression.to_string expr
    | _ -> Alcotest.failf "Expected expression statement")
;;

let test_parse_let_statement () =
  let tests =
    [ "let x = 5;", "let x = 5"
    ; "let y = 10;", "let y = 10"
    ; "let foobar = 838383;", "let foobar = 838383"
    ; "let foobar = true;", "let foobar = true"
    ; "let barfoo = false;", "let barfoo = false"
    ]
  in
  run_parser_tests tests Ast.Statement.to_string
;;

let test_parse_return_statement () =
  let tests =
    [ "return 5;", "return 5"; "return 10;", "return 10"; "return add(15);", "return add(15)" ]
  in
  run_parser_tests tests Ast.Statement.to_string
;;

let test_parse_if_expression () =
  let tests =
    [ "if (x < y) { x }", "(if ((x < y)) { x })"
    ; ( "if (x < y) { x * y } else { y + 314159 }"
      , "(if ((x < y)) { (x * y) } else { (y + 314159) })" )
    ]
  in
  run_parser_tests tests (fun statement ->
    match statement with
    | Ast.Statement.Expression expr -> Ast.Expression.to_string expr
    | _ -> Alcotest.failf "Expected expression statement")
;;

let test_parse_fn_expression () =
  let tests =
    [ "fn(x, y) { x + y }", "(fn (x, y) { (x + y) })"
    ; "fn() { 42 }", "(fn () { 42 })"
    ; "fn(x, y) { let z = x + y; return z + 5; }", "(fn (x, y) { let z = (x + y); return (z + 5) })"
    ]
  in
  run_parser_tests tests (fun statement ->
    match statement with
    | Ast.Statement.Expression expr -> Ast.Expression.to_string expr
    | _ -> Alcotest.failf "Expected expression statement")
;;

let test_parse_fn_call_expression () =
  let tests = [ "add(1, 2 * 3, 4 + 5);", "add(1, (2 * 3), (4 + 5))" ] in
  run_parser_tests tests (fun statement ->
    match statement with
    | Ast.Statement.Expression expr -> Ast.Expression.to_string expr
    | _ -> Alcotest.failf "Expected expression statement")
;;

let test_parse_prefix_expression () =
  let tests =
    [ "!5;", "(!5)"; "-15;", "(-15)"; "!true;", "(!true)"; "!false;", "(!false)" ]
  in
  run_parser_tests tests (fun statement ->
    match statement with
    | Ast.Statement.Expression expr -> Ast.Expression.to_string expr
    | _ -> Alcotest.failf "Expected expression statement")
;;

let test_parse_infix_expression () =
  let tests =
    [ "5 + 5;", "(5 + 5)"
    ; "5 - 5;", "(5 - 5)"
    ; "5 * 5;", "(5 * 5)"
    ; "5 / 5;", "(5 / 5)"
    ; "5 > 5;", "(5 > 5)"
    ; "5 < 5;", "(5 < 5)"
    ; "5 == 5;", "(5 == 5)"
    ; "5 != 5;", "(5 != 5)"
    ; "true == true;", "(true == true)"
    ; "false == false;", "(false == false)"
    ; "false != true;", "(false != true)"
    ]
  in
  run_parser_tests tests (fun statement ->
    match statement with
    | Ast.Statement.Expression expr -> Ast.Expression.to_string expr
    | _ -> Alcotest.failf "Expected expression statement")
;;

let test_operator_precedence () =
  let tests =
    (* input, expected *)
    [ "-a * b", "((-a) * b)"
    ; "!-a", "(!(-a))"
    ; "a + b + c", "((a + b) + c)"
    ; "a + b - c", "((a + b) - c)"
    ; "a + b * -c", "(a + (b * (-c)))"
    ; "a * b * c", "((a * b) * c)"
    ; "a * b / c", "((a * b) / c)"
    ; "a + b / c", "(a + (b / c))"
    ; "a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"
    ; "5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"
    ; "5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"
    ; "3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
    ; "3 > 5 == false", "((3 > 5) == false)"
    ; "3 < 5 == true", "((3 < 5) == true)"
    ; "1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"
    ; "(5 + 5) * 2", "((5 + 5) * 2)"
    ; "2 / (5 + 5)", "(2 / (5 + 5))"
    ; "-(5 + 5)", "(-(5 + 5))"
    ; "!(true == true)", "(!(true == true))"
    ; "a + add(b * c) + d", "((a + add((b * c))) + d)"
    ; ( "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))"
      , "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))" )
    ; "add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"
    ]
  in
  run_parser_tests tests (fun statement ->
    match statement with
    | Ast.Statement.Expression expr -> Ast.Expression.to_string expr
    | _ -> Alcotest.failf "Expected expression statement")
;;

let test_suite =
  [ Alcotest.test_case
      "literal expression statements"
      `Quick
      test_parse_literal_expression
  ; Alcotest.test_case "let statements" `Quick test_parse_let_statement
  ; Alcotest.test_case "return statements" `Quick test_parse_return_statement
  ; Alcotest.test_case "if expression" `Quick test_parse_if_expression
  ; Alcotest.test_case "function expression" `Quick test_parse_fn_expression
  ; Alcotest.test_case "function call expression" `Quick test_parse_fn_call_expression
  ; Alcotest.test_case "prefix expression statements" `Quick test_parse_prefix_expression
  ; Alcotest.test_case "infix expression statements" `Quick test_parse_infix_expression
  ; Alcotest.test_case "operator precedence" `Quick test_operator_precedence
  ]
;;
