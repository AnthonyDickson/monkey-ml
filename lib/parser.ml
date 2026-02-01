(* TODO: When OCaml 5.4.0 is available through NixPkgs, upgrade and replace
   below let statement with `open Result.Syntax` *)
open Token
module Expression = Ast.Expression
module InfixOp = Ast.InfixOp
module PrefixOp = Ast.PrefixOp
module Statement = Ast.Statement

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

let expect_peek_token parser token =
  if parser.next_token = token
  then Ok ()
  else
    Error
      (Printf.sprintf
         "expected the token after \"%s\" to be %s, got %s"
         (to_string parser.curr_token)
         (to_string token)
         (to_string parser.next_token))
;;

let consume_token parser token =
  if parser.curr_token = token
  then Ok (advance parser)
  else
    Error
      (Printf.sprintf
         "expected the token %s, got %s (next is %s)"
         (to_string token)
         (to_string parser.curr_token)
         (to_string parser.next_token))
;;

let consume_identifier parser =
  match parser.curr_token with
  | Ident identifier -> Ok (advance parser, identifier)
  | _ ->
    Error
      (Printf.sprintf
         "expected an identifier, got %s (next is %s)"
         (to_string parser.curr_token)
         (to_string parser.next_token))
;;

let consume_next_if_matches parser token =
  if parser.next_token = token then advance parser else parser
;;

let rec parse_statement parser =
  match parser.curr_token with
  | Let -> parse_let_statement parser
  | Return -> parse_return_statement parser
  | _ -> parse_expression_statement parser

and parse_let_statement parser =
  let* parser = consume_token parser Let in
  let* parser, identifier = consume_identifier parser in
  let* parser = consume_token parser Assign in
  let* parser, expression = parse_expression parser Precedence.Lowest in
  let parser = consume_next_if_matches parser Semicolon in
  Ok (parser, Statement.Let { identifier; expression })

and parse_return_statement parser =
  let* parser = consume_token parser Return in
  let* parser, expression = parse_expression parser Precedence.Lowest in
  let parser = consume_next_if_matches parser Semicolon in
  Ok (parser, Statement.Return expression)

and parse_statement_block parser =
  let rec parse_statement_block_loop parser statements =
    match parser.curr_token with
    | Rbrace | Eof -> Ok (parser, List.rev statements)
    | _ ->
      let* parser, statement = parse_statement parser in
      parse_statement_block_loop (advance parser) (statement :: statements)
  in
  let* parser = consume_token parser Lbrace in
  let* parser, block = parse_statement_block_loop parser [] in
  Ok (parser, block)

and parse_expression_statement parser =
  let* parser, expression = parse_expression parser Precedence.Lowest in
  let parser = consume_next_if_matches parser Semicolon in
  Ok (parser, Statement.Expression expression)

and parse_expression parser precedence =
  let* parser, left = prefix parser in
  match infix parser precedence left with
  | Ok result -> Ok result
  | Error _ -> Ok (parser, left)

and prefix parser =
  match parser.curr_token with
  | Ident identifier -> Ok (parser, Expression.Identifier identifier)
  | Int integer -> Ok (parser, Expression.IntLiteral integer)
  | True -> Ok (parser, Expression.BoolLiteral true)
  | False -> Ok (parser, Expression.BoolLiteral false)
  | String str -> Ok (parser, Expression.StringLiteral str)
  | Lparen -> parse_grouped_expression parser
  | Lbracket -> parse_array_literal parser
  | (Minus | Bang) as operator -> parse_prefix_expression parser operator
  | If -> parse_if_expression parser
  | Function -> parse_fn_expression parser
  | token ->
    Error
      (Printf.sprintf
         "unexpected prefix token \"%s\", next token is: %s"
         (to_string token)
         (to_string parser.next_token))

and parse_grouped_expression parser =
  let* parser, expression = parse_expression (advance parser) Precedence.Lowest in
  let* () = expect_peek_token parser Rparen in
  Ok (advance parser, expression)

and parse_array_literal parser =
  let* parser = consume_token parser Token.Lbracket in
  let* parser, elements = parse_expression_list parser Token.Rbracket in
  Ok (parser, Expression.ArrayLiteral elements)

and parse_if_expression parser =
  let* parser = consume_token parser If in
  let* parser = consume_token parser Lparen in
  let* parser, condition = parse_expression parser Precedence.Lowest in
  let parser = advance parser in
  let* parser = consume_token parser Rparen in
  let* parser, consequent = parse_statement_block parser in
  let* parser, alternative =
    match parser.next_token with
    | Else ->
      let parser = advance parser in
      let* () = expect_peek_token parser Lbrace in
      let* parser, alt = parse_statement_block (advance parser) in
      Ok (parser, Some alt)
    | _ -> Ok (parser, None)
  in
  Ok (parser, Expression.If { condition; consequent; alternative })

and parse_fn_expression parser =
  let rec parse_fn_args parser args =
    match parser.curr_token with
    | Rparen | Eof -> Ok (parser, List.rev args)
    | Ident identifier -> parse_fn_args (advance parser) (identifier :: args)
    | Comma ->
      let* parser = consume_token parser Comma in
      let* parser, identifier = consume_identifier parser in
      parse_fn_args parser (identifier :: args)
    | token ->
      Error
        (to_string token
         ^ " is not allowed as a function argument, expected an identifier")
  in
  let* parser = consume_token parser Function in
  let* parser = consume_token parser Lparen in
  let* parser, parameters = parse_fn_args parser [] in
  let* parser = consume_token parser Rparen in
  let* parser, body = parse_statement_block parser in
  let parser =
    match parser.next_token with
    | Semicolon -> advance parser
    | _ -> parser
  in
  Ok (parser, Expression.FunctionLiteral { parameters; body })

and parse_prefix_expression parser operator =
  let* parser, rhs = parse_expression (advance parser) Precedence.Prefix in
  let operator = PrefixOp.from_token operator in
  Ok (parser, Expression.Prefix (operator, rhs))

and infix parser precedence lhs =
  let next_token = parser.next_token in
  let next_precedence = Precedence.from_token next_token in
  match next_token with
  | (Eq | NotEq | Lt | Gt | Plus | Minus | Asterisk | Slash) as operator
    when Precedence.binds_tighter precedence next_precedence ->
    let* parser, expression = parse_infix_expression (advance parser) lhs operator in
    infix parser precedence expression
  | Lparen when Precedence.binds_tighter precedence next_precedence ->
    let* parser, expression = parse_function_call (advance parser) lhs in
    infix parser precedence expression
  | _ -> Ok (parser, lhs)

and parse_infix_expression parser lhs operator =
  let precedence = Precedence.from_token operator in
  let* parser, rhs = parse_expression (advance parser) precedence in
  let operator = InfixOp.from_token operator in
  Ok (parser, Expression.Infix (lhs, operator, rhs))

and parse_function_call parser function_expression =
  let* parser = consume_token parser Token.Lparen in
  let* parser, arguments = parse_expression_list parser Token.Rparen in
  Ok (parser, Expression.Call { func = function_expression; arguments })

and parse_expression_list parser end_token =
  let rec loop parser args =
    match parser.curr_token with
    | token when token = end_token -> Ok (parser, List.rev args)
    | Eof -> Ok (parser, List.rev args)
    | Comma -> loop (advance parser) args
    | _ ->
      let* parser, expression = parse_expression parser Precedence.Lowest in
      loop (advance parser) (expression :: args)
  in
  loop parser []
;;

let string_of_errors errors =
  Printf.sprintf
    "Parser had %d errors:\n%s"
    (List.length errors)
    (String.concat "\n" errors)
;;

let recover_to_next_statement parser =
  let is_start_of_statement = function
    | Let | Return | Eof -> true
    | _ -> false
  in
  let rec skip parser =
    match parser.curr_token with
    | Eof -> parser
    | Semicolon -> advance parser
    | token when is_start_of_statement token -> parser
    | _ -> skip (advance parser)
  in
  skip (advance parser)
;;

let parse_program parser =
  let rec loop parser statements errors =
    match parser.curr_token with
    | Eof ->
      if errors = []
      then Ok (List.rev statements)
      else Error (List.rev statements, string_of_errors (List.rev errors))
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
