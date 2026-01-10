let value_testable =
  let open Monkeylang in
  let pp_value fmt value =
    match value with
    | Ok (_, value) -> Format.pp_print_string fmt (Value.to_string value)
    | Error msg -> Format.pp_print_string fmt msg
  in
  let equal a b =
    match a, b with
    | Ok (_, val_a), Ok (_, val_b) -> val_a = val_b
    | Error err_a, Error err_b -> err_a = err_b
    | _, _ -> false
  in
  Alcotest.testable pp_value equal
;;

let run_evaluator_tests tests wrap_expected_value =
  let open Monkeylang in
  let ( let* ) = Result.bind in
  let run_test input =
    let lexer = Lexer.make input in
    let parser = Parser.make lexer in
    let* program =
      Parser.parse_program parser |> Result.map_error (fun (_program, error) -> error)
    in
    let environment = Environment.make () in
    Evaluator.evaluate environment program
  in
  List.iter
    (fun (input, expected) ->
       let actual = run_test input in
       let expected = wrap_expected_value expected in
       Alcotest.(check value_testable) ("evaluate: " ^ input) expected actual)
    tests
;;

let check_value_ok tests =
  let placeholder_env = Monkeylang.Environment.make () in
  let wrap_expected_value value = Ok (placeholder_env, value) in
  run_evaluator_tests tests wrap_expected_value
;;

let check_error tests =
  let wrap_expected_value value = value in
  run_evaluator_tests tests wrap_expected_value
;;

let test_evaluate_integer_experessions () =
  let open Monkeylang in
  let tests =
    [ "5;", Value.Integer 5
    ; "10;", Value.Integer 10
    ; "-5", Value.Integer (-5)
    ; "-10", Value.Integer (-10)
    ]
  in
  check_value_ok tests
;;

let test_evaluate_boolean_expressions () =
  let open Monkeylang in
  let tests =
    [ "true;", Value.Boolean true
    ; "false;", Value.Boolean false
    ; "1 < 2", Value.Boolean true
    ; "1 > 2", Value.Boolean false
    ; "1 < 1", Value.Boolean false
    ; "1 > 1", Value.Boolean false
    ; "1 == 1", Value.Boolean true
    ; "1 != 1", Value.Boolean false
    ; "1 == 2", Value.Boolean false
    ; "1 != 2", Value.Boolean true
    ]
  in
  check_value_ok tests
;;

let test_evaluate_bang_operator () =
  let open Monkeylang in
  let tests =
    [ "!true;", Value.Boolean false
    ; "!false", Value.Boolean true
    ; "!!true", Value.Boolean true
    ; "!!false", Value.Boolean false
    ]
  in
  check_value_ok tests
;;

let test_evaluate_infix_expressions () =
  let open Monkeylang in
  let tests =
    [ "5", Value.Integer 5
    ; "10", Value.Integer 10
    ; "-5", Value.Integer (-5)
    ; "-10", Value.Integer (-10)
    ; "5 + 5 + 5 + 5 - 10", Value.Integer 10
    ; "2 * 2 * 2 * 2 * 2", Value.Integer 32
    ; "-50 + 100 + -50", Value.Integer 0
    ; "5 * 2 + 10", Value.Integer 20
    ; "5 + 2 * 10", Value.Integer 25
    ; "20 + 2 * -10", Value.Integer 0
    ; "50 / 2 * 2 + 10", Value.Integer 60
    ; "2 * (5 + 10)", Value.Integer 30
    ; "3 * 3 * 3 + 10", Value.Integer 37
    ; "3 * (3 * 3) + 10", Value.Integer 37
    ; "(5 + 10 * 2 + 15 / 3) * 2 + -10", Value.Integer 50
    ]
  in
  check_value_ok tests
;;

let test_evaluate_if_else_expressions () =
  let open Monkeylang in
  let tests =
    [ "if (true) { 10 }", Value.Integer 10
    ; "if (false) { 10 }", Value.Null
    ; "if (1 < 2) { 10 }", Value.Integer 10
    ; "if (1 > 2) { 10 }", Value.Null
    ; "if (1 > 2) { 10 } else { 20 }", Value.Integer 20
    ; "if (1 < 2) { 10 } else { 20 }", Value.Integer 10
    ]
  in
  check_value_ok tests
;;

let test_evaluate_return_statement () =
  let open Monkeylang in
  let tests =
    [ "return 10", Value.Integer 10
    ; "return 10; 9", Value.Integer 10
    ; "return 2 * 5; 9", Value.Integer 10
    ; "9; return 2 * 5; 8", Value.Integer 10
    ; ( {|
        if (10 > 1) {
          if (10 > 1) {
            return 10;
          }

          return 1;
        }
      |}
      , Value.Integer 10 )
    ]
  in
  check_value_ok tests
;;

let test_evaluate_let_statement () =
  let open Monkeylang in
  let tests =
    [ "let a = 5; a", Value.Integer 5
    ; "let a = 5 * 5; a", Value.Integer 25
    ; "let a = 5; let b = a; b", Value.Integer 5
    ; "let a = 5; let b = a; let c = a + b + 5; c", Value.Integer 15
    ]
  in
  check_value_ok tests
;;

let test_evaluate_function_literal () =
  let open Monkeylang in
  let tests =
    [ ( "fn(x) { x + 2 }"
      , Value.Function
          { parameters = [ "x" ]
          ; body =
              [ Ast.Statement.Expression
                  (Ast.Expression.Infix
                     ( Ast.Expression.Identifier "x"
                     , Ast.InfixOp.Plus
                     , Ast.Expression.IntLiteral 2 ))
              ]
          ; environment = Environment.make ()
          } )
    ]
  in
  check_value_ok tests
;;

let test_evaluate_function_application () =
  let open Monkeylang in
  let tests =
    [ "let identity = fn(x) { x }; identity(5)", Value.Integer 5
    ; "let identity = fn(x) { return x }; identity(5)", Value.Integer 5
    ; "let double = fn(x) { x * 2 }; double(5)", Value.Integer 10
    ; "let add = fn(x, y) { x + y }; add(5, 5)", Value.Integer 10
    ; "let add = fn(x, y) { return x + y }; add(5 + 5, add(5, 5))", Value.Integer 20
    ; "fn(x) { x }(5)", Value.Integer 5
    ]
  in
  check_value_ok tests
;;

let test_evaluate_closure () =
  let open Monkeylang in
  let tests =
    [ ( {|
        let newAdder = fn(x) { fn (y) { x + y}; };
        let addTwo = newAdder(2);
        addTwo(2);
    |}
      , Value.Integer 4 )
    ]
  in
  check_value_ok tests
;;

let test_evaluate_error () =
  let tests =
    [ "5 + true;", Error "type mismatch: INTEGER + BOOLEAN"
    ; "5 + true; 5;", Error "type mismatch: INTEGER + BOOLEAN"
    ; "-true", Error "unknown operator: -BOOLEAN"
    ; "!5", Error "unknown operator: !INTEGER"
    ; "true + false;", Error "unknown operator: BOOLEAN + BOOLEAN"
    ; "5; true + false; 5", Error "unknown operator: BOOLEAN + BOOLEAN"
    ; "if (10 > 1) { true + false };", Error "unknown operator: BOOLEAN + BOOLEAN"
    ; ( {|
          if (10 > 1) {
            if (10 > 1) {
              return true + false;
            }

            return 1;
          }
        |}
      , Error "unknown operator: BOOLEAN + BOOLEAN" )
    ; "return 5 + false", Error "type mismatch: INTEGER + BOOLEAN"
    ; "3 * 7 - 5 / false", Error "type mismatch: INTEGER / BOOLEAN"
    ; "3 * true - 5 / 9", Error "type mismatch: INTEGER * BOOLEAN"
    ; "if (3) { true }", Error "type mismatch: if (INTEGER)"
    ; "if (true - false) { true }", Error "unknown operator: BOOLEAN - BOOLEAN"
    ; "8 / (5 - 5)", Error "division by zero"
    ; "foobar", Error "identifier not found: foobar"
    ; ( "let add = fn(x, y) { x + y }; add(1)"
      , Error "wrong number of arguments: expected 2, got 1" )
    ; ( "let add = fn(x, y) { x + y }; add(1, 2, 3)"
      , Error "wrong number of arguments: expected 2, got 3" )
    ]
  in
  check_error tests
;;

let test_suite =
  [ Alcotest.test_case "integer expressions" `Quick test_evaluate_integer_experessions
  ; Alcotest.test_case "boolean expressions" `Quick test_evaluate_boolean_expressions
  ; Alcotest.test_case "bang operator" `Quick test_evaluate_bang_operator
  ; Alcotest.test_case "infix expressions" `Quick test_evaluate_infix_expressions
  ; Alcotest.test_case "if else expressions" `Quick test_evaluate_if_else_expressions
  ; Alcotest.test_case "function literal" `Quick test_evaluate_function_literal
  ; Alcotest.test_case "function application" `Quick test_evaluate_function_application
  ; Alcotest.test_case "closures" `Quick test_evaluate_closure
  ; Alcotest.test_case "return statement" `Quick test_evaluate_return_statement
  ; Alcotest.test_case "let statement" `Quick test_evaluate_let_statement
  ; Alcotest.test_case "evaluate error" `Quick test_evaluate_error
  ]
;;
