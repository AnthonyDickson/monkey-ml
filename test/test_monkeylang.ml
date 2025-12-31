let () =
  let open Alcotest in
  run "Monkeylang" [ "Lexer", Test_lexer.test_suite; "Parser", Test_parser.test_suite ]
;;
