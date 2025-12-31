open Monkeylang

let statement_testable =
  let open Monkeylang in
  let pp_statement fmt statement =
    Format.pp_print_string fmt (Ast.string_of_statement statement)
  in
  Alcotest.testable pp_statement ( = )
;;

let token_testable =
  let open Monkeylang in
  let pp_token fmt token = Format.pp_print_string fmt (Token.to_string token) in
  Alcotest.testable pp_token ( = )
;;

let check_same_statement_count expected actual =
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
    [ Ast.Let { identifier = "x" }
    ; Ast.Let { identifier = "y" }
    ; Ast.Let { identifier = "foobar" }
    ]
  in
  let lexer = Result.get_ok @@ Lexer.create input in
  let parser = Parser.create lexer in
  match Parser.parse_program parser with
  | Ok actual_program -> check_same_statements expected_program actual_program
  | Error msg -> Alcotest.failf "Got unexpected error: %s" msg
;;

let test_parse_return_statement () =
  let input =
    {|
      return 5;
      return 10;
      return add(15);
    |}
  in
  let expected_program = [ Ast.Return; Ast.Return; Ast.Return ] in
  let lexer = Result.get_ok @@ Lexer.create input in
  let parser = Parser.create lexer in
  match Parser.parse_program parser with
  | Ok actual_program -> check_same_statements expected_program actual_program
  | Error msg -> Alcotest.failf "Got unexpected error: %s" msg
;;

let test_suite =
  [ Alcotest.test_case "let statements" `Quick test_parse_let_statement
  ; Alcotest.test_case "return statements" `Quick test_parse_return_statement
  ]
;;
