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

let expect_peek_identifier parser =
  match parser.next_token with
  | Token.Ident literal -> Ok literal
  | _ ->
    Error
      (Printf.sprintf
         "expected the next token to be an identifier, got %s"
         (Token.to_string parser.next_token))
;;

let expect_peek_token parser token =
  if parser.next_token = token
  then Ok ()
  else
    Error
      (Printf.sprintf
         "expected the next token to be %s, got %s"
         (Token.to_string token)
         (Token.to_string parser.next_token))
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

let parse_let_statement parser =
  let* identifier = expect_peek_identifier parser in
  let parser = advance parser in
  let* () = expect_peek_token parser Token.Assign in
  let* parser = Ok (advance_while parser (fun token -> token <> Token.Semicolon)) in
  Ok (Ast.Statement.Let { identifier }, parser)
;;

let parse_return_statement parser =
  let parser = advance parser in
  let* parser = Ok (advance_while parser (fun token -> token <> Token.Semicolon)) in
  Ok (Ast.Statement.Return, parser)
;;

let rec parse_expression parser precedence =
  let* left, parser = prefix parser in
  match infix parser precedence left with
  | Ok result -> Ok result
  | Error _ -> Ok (left, parser)

and prefix parser =
  match parser.curr_token with
  | Token.Ident identifier -> Ok (Ast.Expression.Identifier identifier, parser)
  | Token.Int integer -> Ok (Ast.Expression.IntLiteral integer, parser)
  | (Token.Minus | Token.Bang) as operator ->
    let operator = Ast.PrefixOp.from_token operator in
    parse_prefix_expression parser operator
  | token -> Error (Printf.sprintf "unexpected prefix token %s" (Token.to_string token))

and parse_prefix_expression parser operator =
  let* rhs, parser = parse_expression (advance parser) Precedence.Prefix in
  Ok (Ast.Expression.Prefix (operator, rhs), parser)

and infix parser precedence lhs =
  let open Token in
  let next_token = parser.next_token in
  let next_precedence = Precedence.from_token next_token in
  match next_token with
  | (Eq | NotEq | Lt | Gt | Plus | Minus | Asterisk | Slash) as operator
    when Precedence.binds_tighter precedence next_precedence ->
    let* expression, parser = parse_infix_expression (advance parser) lhs operator in
    infix parser precedence expression
  | _ -> Ok (lhs, parser)

and parse_infix_expression parser lhs operator =
  let precedence = Precedence.from_token operator in
  let* rhs, parser = parse_expression (advance parser) precedence in
  let operator = Ast.InfixOp.from_token operator in
  Ok (Ast.Expression.Infix (lhs, operator, rhs), parser)
;;

let parse_expression_statement parser =
  let* expression, parser = parse_expression parser Precedence.Lowest in
  let parser = if parser.next_token = Token.Semicolon then advance parser else parser in
  Ok (Ast.Statement.Expression expression, parser)
;;

let parse_statement parser =
  match parser.curr_token with
  | Token.Let -> parse_let_statement parser
  | Token.Return -> parse_return_statement parser
  | _ -> parse_expression_statement parser
;;

let format_errors errors =
  Printf.sprintf
    "Parser had %d errors:\n%s"
    (List.length errors)
    (String.concat "\n" errors)
;;

let parse_program parser =
  let rec loop parser program errors =
    match parser.curr_token with
    | Token.Eof ->
      if errors = []
      then Ok (List.rev program)
      else Error (format_errors @@ List.rev errors)
    | _ ->
      (match parse_statement parser with
       | Ok (statement, parser') -> loop (advance parser') (statement :: program) errors
       | Error error -> loop (advance parser) program (error :: errors))
  in
  loop parser [] []
;;
