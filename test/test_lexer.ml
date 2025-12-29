let token_type_testable =
  let open Monkeylang in
  let open Token in
  let pp_token_type fmt = function
    | Illegal -> Format.fprintf fmt "Illegal"
    | Eof -> Format.fprintf fmt "Eof"
    | Ident -> Format.fprintf fmt "Ident"
    | Int -> Format.fprintf fmt "Int"
    | Assign -> Format.fprintf fmt "Assign"
    | Plus -> Format.fprintf fmt "Plus"
    | Comma -> Format.fprintf fmt "Comma"
    | Semicolon -> Format.fprintf fmt "Semicolon"
    | Lparen -> Format.fprintf fmt "Lparen"
    | Rparen -> Format.fprintf fmt "Rparen"
    | Lbrace -> Format.fprintf fmt "Lbrace"
    | Rbrace -> Format.fprintf fmt "Rbrace"
    | Function -> Format.fprintf fmt "Function"
    | Let -> Format.fprintf fmt "Let"
  in
  let eq_token_type a b =
    match a, b with
    | Illegal, Illegal -> true
    | Eof, Eof -> true
    | Ident, Ident -> true
    | Int, Int -> true
    | Assign, Assign -> true
    | Plus, Plus -> true
    | Comma, Comma -> true
    | Semicolon, Semicolon -> true
    | Lparen, Lparen -> true
    | Rparen, Rparen -> true
    | Lbrace, Lbrace -> true
    | Rbrace, Rbrace -> true
    | Function, Function -> true
    | Let, Let -> true
    | _ -> false
  in
  Alcotest.testable pp_token_type eq_token_type
;;

let test_next_token_special_chars () =
  let open Monkeylang in
  let input = "=+(){},;" in
  let expected =
    [ "=", Token.Assign
    ; "+", Token.Plus
    ; "(", Token.Lparen
    ; ")", Token.Rparen
    ; "{", Token.Lbrace
    ; "}", Token.Rbrace
    ; ",", Token.Comma
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
  and lexer = Result.get_ok @@ Lexer.create input in
  lex lexer expected
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
      ; ""
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

let test_suite =
  [ Alcotest.test_case "special characters" `Quick test_next_token_special_chars
  ; Alcotest.test_case "small example" `Quick test_next_token
  ]
;;
