let value_testable =
  let open Monkeylang in
  let pp_token fmt token = Format.pp_print_string fmt (Value.to_string token) in
  Alcotest.testable pp_token ( = )
;;

let run_evaluator_tests tests =
  let open Monkeylang in
  let ( let* ) = Result.bind in
  let run_test input =
    let* lexer = Lexer.make input in
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

let test_evaluate_integers () =
  let open Monkeylang in
  let tests = [ "5;", Value.Integer 5; "10;", Value.Integer 10 ] in
  run_evaluator_tests tests
;;

let test_suite = [ Alcotest.test_case "integers" `Quick test_evaluate_integers ]
