let token_testable =
  let open Monkeylang in
  let pp_token fmt token = Format.pp_print_string fmt (Token.to_string token) in
  Alcotest.testable pp_token ( = )
;;

let test_next_token () =
  let open Monkeylang in
  let input =
    String.concat
      "\n"
      [ "let five = 5;"
      ; "let ten = 10;"
      ; ""
      ; "let add = fn(x, y) {"
      ; "  x + y;"
      ; "};"
      ; ""
      ; "let result = add(five, ten);"
      ; "!-/*5;"
      ; "5 < 10 > 5;"
      ; ""
      ; "if (5 < 10) {"
      ; "    return true;"
      ; "} else {"
      ; "    return false;"
      ; "}"
      ; ""
      ; "10 == 10;"
      ; "10 != 9;"
      ; "#"
      ]
  and expected =
    [ Token.Let
    ; Token.Ident "five"
    ; Token.Assign
    ; Token.Int 5
    ; Token.Semicolon
    ; Token.Let
    ; Token.Ident "ten"
    ; Token.Assign
    ; Token.Int 10
    ; Token.Semicolon
    ; Token.Let
    ; Token.Ident "add"
    ; Token.Assign
    ; Token.Function
    ; Token.Lparen
    ; Token.Ident "x"
    ; Token.Comma
    ; Token.Ident "y"
    ; Token.Rparen
    ; Token.Lbrace
    ; Token.Ident "x"
    ; Token.Plus
    ; Token.Ident "y"
    ; Token.Semicolon
    ; Token.Rbrace
    ; Token.Semicolon
    ; Token.Let
    ; Token.Ident "result"
    ; Token.Assign
    ; Token.Ident "add"
    ; Token.Lparen
    ; Token.Ident "five"
    ; Token.Comma
    ; Token.Ident "ten"
    ; Token.Rparen
    ; Token.Semicolon
    ; Token.Bang
    ; Token.Minus
    ; Token.Slash
    ; Token.Asterisk
    ; Token.Int 5
    ; Token.Semicolon
    ; Token.Int 5
    ; Token.Lt
    ; Token.Int 10
    ; Token.Gt
    ; Token.Int 5
    ; Token.Semicolon
    ; Token.If
    ; Token.Lparen
    ; Token.Int 5
    ; Token.Lt
    ; Token.Int 10
    ; Token.Rparen
    ; Token.Lbrace
    ; Token.Return
    ; Token.True
    ; Token.Semicolon
    ; Token.Rbrace
    ; Token.Else
    ; Token.Lbrace
    ; Token.Return
    ; Token.False
    ; Token.Semicolon
    ; Token.Rbrace
    ; Token.Int 10
    ; Token.Eq
    ; Token.Int 10
    ; Token.Semicolon
    ; Token.Int 10
    ; Token.NotEq
    ; Token.Int 9
    ; Token.Semicolon
    ; Token.Illegal '#'
    ; Token.Eof
    ]
  in
  let rec lex l = function
    | [] -> ()
    | expected_token :: t ->
      let token, ll = Lexer.next_token l in
      Alcotest.(check token_testable) "same token" expected_token token;
      lex ll t
  in
  let lexer = Result.get_ok @@ Lexer.create input in
  lex lexer expected
;;

let test_suite = [ Alcotest.test_case "small example" `Quick test_next_token ]
