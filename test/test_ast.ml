open Monkeylang

let test_parses_let_statement () =
  let input = [Token.Let, ]
  Alcotest.(check bool) "foo" true true

let test_suite = [ Alcotest.test_case "let statements" `Quick test_parses_let_statement ]
