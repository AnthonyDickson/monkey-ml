let () =
  Alcotest.run
    "Monkey ML"
    [ "Lexer", Test_lexer.test_suite
    ; "Parser", Test_parser.test_suite
    ; "Evaluator", Test_evaluator.test_suite
    ]
;;
