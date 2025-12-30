let token_type_testable =
  let open Monkeylang in
  let open Token in
  let pp_token_type fmt = function
    | Illegal -> Format.fprintf fmt "Illegal"
    | Eof -> Format.fprintf fmt "Eof"
    | Ident -> Format.fprintf fmt "Ident"
    | Int -> Format.fprintf fmt "Int"
    | True -> Format.fprintf fmt "True"
    | False -> Format.fprintf fmt "False"
    | Assign -> Format.fprintf fmt "Assign"
    | Plus -> Format.fprintf fmt "Plus"
    | Minus -> Format.fprintf fmt "Minus"
    | Asterisk -> Format.fprintf fmt "Asterisk"
    | Slash -> Format.fprintf fmt "Slash"
    | Bang -> Format.fprintf fmt "Bang"
    | Lt -> Format.fprintf fmt "Lt"
    | Gt -> Format.fprintf fmt "Gt"
    | Eq -> Format.fprintf fmt "Eq"
    | NotEq -> Format.fprintf fmt "NotEq"
    | Comma -> Format.fprintf fmt "Comma"
    | Semicolon -> Format.fprintf fmt "Semicolon"
    | Lparen -> Format.fprintf fmt "Lparen"
    | Rparen -> Format.fprintf fmt "Rparen"
    | Lbrace -> Format.fprintf fmt "Lbrace"
    | Rbrace -> Format.fprintf fmt "Rbrace"
    | Function -> Format.fprintf fmt "Function"
    | Let -> Format.fprintf fmt "Let"
    | If -> Format.fprintf fmt "If"
    | Else -> Format.fprintf fmt "Else"
    | Return -> Format.fprintf fmt "Return"
  in
  Alcotest.testable pp_token_type ( = )
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
      ]
  and expected =
    [ "let", Token.Let
    ; "five", Token.Ident
    ; "=", Token.Assign
    ; "5", Token.Int
    ; ";", Token.Semicolon
    ; "let", Token.Let
    ; "ten", Token.Ident
    ; "=", Token.Assign
    ; "10", Token.Int
    ; ";", Token.Semicolon
    ; "let", Token.Let
    ; "add", Token.Ident
    ; "=", Token.Assign
    ; "fn", Token.Function
    ; "(", Token.Lparen
    ; "x", Token.Ident
    ; ",", Token.Comma
    ; "y", Token.Ident
    ; ")", Token.Rparen
    ; "{", Token.Lbrace
    ; "x", Token.Ident
    ; "+", Token.Plus
    ; "y", Token.Ident
    ; ";", Token.Semicolon
    ; "}", Token.Rbrace
    ; ";", Token.Semicolon
    ; "let", Token.Let
    ; "result", Token.Ident
    ; "=", Token.Assign
    ; "add", Token.Ident
    ; "(", Token.Lparen
    ; "five", Token.Ident
    ; ",", Token.Comma
    ; "ten", Token.Ident
    ; ")", Token.Rparen
    ; ";", Token.Semicolon
    ; "!", Token.Bang
    ; "-", Token.Minus
    ; "/", Token.Slash
    ; "*", Token.Asterisk
    ; "5", Token.Int
    ; ";", Token.Semicolon
    ; "5", Token.Int
    ; "<", Token.Lt
    ; "10", Token.Int
    ; ">", Token.Gt
    ; "5", Token.Int
    ; ";", Token.Semicolon
    ; "if", Token.If
    ; "(", Token.Lparen
    ; "5", Token.Int
    ; "<", Token.Lt
    ; "10", Token.Int
    ; ")", Token.Rparen
    ; "{", Token.Lbrace
    ; "return", Token.Return
    ; "true", Token.True
    ; ";", Token.Semicolon
    ; "}", Token.Rbrace
    ; "else", Token.Else
    ; "{", Token.Lbrace
    ; "return", Token.Return
    ; "false", Token.False
    ; ";", Token.Semicolon
    ; "}", Token.Rbrace
    ; "10", Token.Int
    ; "==", Token.Eq
    ; "10", Token.Int
    ; ";", Token.Semicolon
    ; "10", Token.Int
    ; "!=", Token.NotEq
    ; "9", Token.Int
    ; ";", Token.Semicolon
    ; "", Token.Eof
    ]
  in
  let rec lex l = function
    | [] -> ()
    | (expected_identifier, expected_token) :: t ->
      let token, ll = Lexer.next_token l in
      Alcotest.(check string) "same literal" expected_identifier token.literal;
      Alcotest.(check token_type_testable) "same token type" expected_token token.type_;
      lex ll t
  in
  let lexer = Result.get_ok @@ Lexer.create input in
  lex lexer expected
;;

let test_suite = [ Alcotest.test_case "small example" `Quick test_next_token ]
