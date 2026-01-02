(* TODO: When OCaml 5.4.0 is available through NixPkgs, upgrade and replace
   below let statement with `open Result.Syntax` *)
let ( let* ) = Result.bind

type t =
  { lexer : Lexer.t
  ; curr_token : Token.t
  ; next_token : Token.t
  }

let make lexer =
  let curr_token, lexer = Lexer.next_token lexer in
  let next_token, lexer = Lexer.next_token lexer in
  { lexer; curr_token; next_token }
;;

(** Advance the parser to the next token *)
let advance parser =
  let curr_token = parser.next_token in
  let next_token, lexer = Lexer.next_token parser.lexer in
  { lexer; curr_token; next_token }
;;

let rec advance_while parser predicate =
  match parser.curr_token with
  | Token.Eof -> parser
  | token when predicate token -> advance_while (advance parser) predicate
  | _ -> parser
;;

let expect_peek_token parser token =
  if parser.next_token = token
  then Ok ()
  else
    Error
      (Printf.sprintf
         "expected the token after \"%s\" to be %s, got %s"
         (Token.to_string parser.curr_token)
         (Token.to_string token)
         (Token.to_string parser.next_token))
;;

let consume_token parser token =
  if parser.curr_token = token
  then Ok (advance parser)
  else
    Error
      (Printf.sprintf
         "expected the token %s, got %s (next is %s)"
         (Token.to_string token)
         (Token.to_string parser.curr_token)
         (Token.to_string parser.next_token))
;;

let consume_identifier parser =
  match parser.curr_token with
  | Token.Ident identifier -> Ok (advance parser, identifier)
  | _ ->
    Error
      (Printf.sprintf
         "expected an identifier, got %s (next is %s)"
         (Token.to_string parser.curr_token)
         (Token.to_string parser.next_token))
;;

let parse_let_statement parser =
  let* parser= consume_token parser Token.Let in
  let* parser, identifier = consume_identifier parser in
  let* parser = consume_token parser Token.Assign in
  (* TODO: Parse expression instead of skipping over it. *)
  let parser = advance_while parser (fun token -> token <> Token.Semicolon) in
  Ok (parser, Ast.Statement.Let { identifier })
;;

let parse_return_statement parser =
  let* parser = consume_token parser Token.Return in
  (* TODO: Parse expression instead of skipping over it. *)
  let parser = advance_while parser (fun token -> token <> Token.Semicolon) in
  Ok (parser, Ast.Statement.Return)
;;

let rec parse_statement parser =
  match parser.curr_token with
  | Token.Let -> parse_let_statement parser
  | Token.Return -> parse_return_statement parser
  | _ -> parse_expression_statement parser

and parse_statement_block parser =
  let rec parse_statement_block_loop parser statements =
    match parser.curr_token with
    | Token.Rbrace | Token.Eof -> Ok (parser, List.rev statements)
    | _ ->
      let* parser, statement = parse_statement parser in
      parse_statement_block_loop (advance parser) (statement :: statements)
  in
  let* parser = consume_token parser Token.Lbrace in
  let* parser, block = parse_statement_block_loop parser [] in
  Ok (parser, block)

and parse_expression_statement parser =
  let* parser, expression = parse_expression parser Precedence.Lowest in
  let parser = if parser.next_token = Token.Semicolon then advance parser else parser in
  Ok (parser, Ast.Statement.Expression expression)

and parse_expression parser precedence =
  let* parser, left = prefix parser in
  match infix parser precedence left with
  | Ok result -> Ok result
  | Error _ -> Ok (parser, left)

and prefix parser =
  match parser.curr_token with
  | Token.Ident identifier -> Ok (parser, Ast.Expression.Identifier identifier)
  | Token.Int integer -> Ok (parser, Ast.Expression.IntLiteral integer)
  | Token.True -> Ok (parser, Ast.Expression.BoolLiteral true)
  | Token.False -> Ok (parser, Ast.Expression.BoolLiteral false)
  | Token.Lparen -> parse_grouped_expression parser
  | (Token.Minus | Token.Bang) as operator -> parse_prefix_expression parser operator
  | Token.If -> parse_if_expression parser
  | Token.Function -> parse_fn_expression parser
  | token ->
    Error
      (Printf.sprintf
         "unexpected prefix token \"%s\", next token is: %s"
         (Token.to_string token)
         (Token.to_string parser.next_token))

and parse_grouped_expression parser =
  let* parser, expression = parse_expression (advance parser) Precedence.Lowest in
  let* () = expect_peek_token parser Token.Rparen in
  Ok (advance parser, expression)

and parse_if_expression parser =
  let* parser = consume_token parser Token.If in
  let* parser = consume_token parser Token.Lparen in
  let* parser, condition = parse_expression parser Precedence.Lowest in
  let parser = advance parser in
  let* parser = consume_token parser Token.Rparen in
  let* parser, consequent = parse_statement_block parser in
  let* parser, alternative =
    match parser.next_token with
    | Token.Else ->
      let parser = advance parser in
      let* () = expect_peek_token parser Token.Lbrace in
      let* parser, alt = parse_statement_block (advance parser) in
      Ok (parser, Some alt)
    | _ -> Ok (parser, None)
  in
  Ok (parser, Ast.Expression.If { condition; consequent; alternative })

and parse_fn_expression parser =
  let rec parse_fn_args parser args =
    match parser.curr_token with
    | Token.Rparen | Token.Eof -> Ok (parser, List.rev args)
    | Token.Ident identifier -> parse_fn_args (advance parser) (identifier :: args)
    | Token.Comma ->
      let* parser = consume_token parser Token.Comma in
      let* parser, identifier = consume_identifier parser in
      parse_fn_args parser (identifier :: args)
    | token ->
      Error
        (Token.to_string token
         ^ " is not allowed as a function argument, expected an identifier")
  in
  let* parser = consume_token parser Token.Function in
  let* parser = consume_token parser Token.Lparen in
  let* parser, arguments = parse_fn_args parser [] in
  let* parser = consume_token parser Token.Rparen in
  (* let* parser = consume_token parser Token.Lbrace in *)
  let* parser, body = parse_statement_block parser in
  let parser =
    match parser.next_token with
    | Token.Semicolon -> advance parser
    | _ -> parser
  in
  Ok (parser, Ast.Expression.Fn { arguments; body })

and parse_prefix_expression parser operator =
  let* parser, rhs = parse_expression (advance parser) Precedence.Prefix in
  let operator = Ast.PrefixOp.from_token operator in
  Ok (parser, Ast.Expression.Prefix (operator, rhs))

and infix parser precedence lhs =
  let open Token in
  let next_token = parser.next_token in
  let next_precedence = Precedence.from_token next_token in
  match next_token with
  | (Eq | NotEq | Lt | Gt | Plus | Minus | Asterisk | Slash) as operator
    when Precedence.binds_tighter precedence next_precedence ->
    let* parser, expression = parse_infix_expression (advance parser) lhs operator in
    infix parser precedence expression
  | _ -> Ok (parser, lhs)

and parse_infix_expression parser lhs operator =
  let precedence = Precedence.from_token operator in
  let* parser, rhs = parse_expression (advance parser) precedence in
  let operator = Ast.InfixOp.from_token operator in
  Ok (parser, Ast.Expression.Infix (lhs, operator, rhs))
;;

let format_errors errors =
  Printf.sprintf
    "Parser had %d errors:\n%s"
    (List.length errors)
    (String.concat "\n" errors)
;;

let recover_to_next_statement parser =
  let is_start_of_statement = function
    | Token.Let | Token.Return | Token.Eof -> true
    | _ -> false
  in
  let rec skip parser =
    match parser.curr_token with
    | Token.Eof -> parser
    | Token.Semicolon -> advance parser
    | token when is_start_of_statement token -> parser
    | _ -> skip (advance parser)
  in
  skip (advance parser)
;;

let parse_program parser =
  let rec loop parser statements errors =
    match parser.curr_token with
    | Token.Eof ->
      if errors = []
      then Ok (List.rev statements)
      else Error (List.rev statements, format_errors (List.rev errors))
    | _ ->
      (match parse_statement parser with
       | Ok (parser', statement) ->
         loop (advance parser') (statement :: statements) errors
       | Error error ->
         let parser' = recover_to_next_statement parser in
         loop parser' statements (error :: errors))
  in
  loop parser [] []
;;
