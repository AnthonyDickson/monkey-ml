let value_testable =
  let open Monkeylang in
  let pp_token fmt token = Format.pp_print_string fmt (Value.to_string token) in
  Alcotest.testable pp_token ( = )
;;

let run_evaluator_tests tests =
  let open Monkeylang in
  let ( let* ) = Result.bind in
  let run_test input =
    let lexer = Lexer.make input in
    let parser = Parser.make lexer in
    let* program =
      Parser.parse_program parser |> Result.map_error (fun (_program, error) -> error)
    in
    let* value = Evaluator.evaluate program in
    Ok value
  in
  List.iter
    (fun (input, expected) ->
       match run_test input with
       | Ok actual ->
         Alcotest.(check value_testable) ("evaluate: " ^ input) expected actual
       | Error msg -> Alcotest.failf "Got unexpected error for input \"%s\": %s" input msg)
    tests
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
  run_evaluator_tests tests
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
  run_evaluator_tests tests
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
  run_evaluator_tests tests
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
  run_evaluator_tests tests
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
  run_evaluator_tests tests
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
  run_evaluator_tests tests
;;

let test_evaluate_error () =
  let open Monkeylang in
  let tests =
    [ "5 + true;", Value.Error "type mismatch: INTEGER + BOOLEAN"
    ; "5 + true; 5;", Value.Error "type mismatch: INTEGER + BOOLEAN"
    ; "-true", Value.Error "unknown operator: -BOOLEAN"
    ; "!5", Value.Error "unknown operator: !INTEGER"
    ; "true + false;", Value.Error "unknown operator: BOOLEAN + BOOLEAN"
    ; "5; true + false; 5", Value.Error "unknown operator: BOOLEAN + BOOLEAN"
    ; "if (10 > 1) { true + false };", Value.Error "unknown operator: BOOLEAN + BOOLEAN"
    ; ( {|
          if (10 > 1) {
            if (10 > 1) {
              return true + false;
            }

            return 1;
          }
        |}
      , Value.Error "unknown operator: BOOLEAN + BOOLEAN" )
    ; "return 5 + false", Value.Error "type mismatch: INTEGER + BOOLEAN"
    ; "3 * 7 - 5 / false", Value.Error "type mismatch: INTEGER / BOOLEAN"
    ; "3 * true - 5 / 9", Value.Error "type mismatch: INTEGER * BOOLEAN"
    ; "if (3) { true }", Value.Error "type mismatch: if (INTEGER)"
    ; "if (true - false) { true }", Value.Error "unknown operator: BOOLEAN - BOOLEAN"
    ; "8 / (5 - 5)", Value.Error "division by zero"
    ]
  in
  run_evaluator_tests tests
;;

let test_suite =
  [ Alcotest.test_case "integer expressions" `Quick test_evaluate_integer_experessions
  ; Alcotest.test_case "boolean expressions" `Quick test_evaluate_boolean_expressions
  ; Alcotest.test_case "bang operator" `Quick test_evaluate_bang_operator
  ; Alcotest.test_case "infix expressions" `Quick test_evaluate_infix_expressions
  ; Alcotest.test_case "if else expressions" `Quick test_evaluate_if_else_expressions
  ; Alcotest.test_case "return statement" `Quick test_evaluate_return_statement
  ; Alcotest.test_case "evaluate error" `Quick test_evaluate_error
  ]
;;
