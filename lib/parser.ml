(* TODO: When OCaml 5.4.0 is available through NixPkgs, upgrade and replace
below let statement with `open Result.Syntax` *)
let ( let* ) = Result.bind

type parser =
  { lexer : Lexer.lexer
  ; curr_token : Token.token
  ; next_token : Token.token
  }

let create lexer =
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
  let* parser = Ok (advance parser) in
  let* () = expect_peek_token parser Token.Assign in
  let* parser = Ok (advance_while parser (fun token -> token <> Token.Semicolon)) in
  Ok (Ast.Let { identifier }, parser)
;;

let parse_return_statement parser =
  let* parser = Ok (advance parser) in
  let* parser = Ok (advance_while parser (fun token -> token <> Token.Semicolon)) in
  Ok (Ast.Return, parser)
;;

let parse_prefix parser =
  match parser.curr_token with
  | Token.Ident identifier -> Ok (Ast.Identifier identifier, parser)
  | token -> Error (Printf.sprintf "unexpected prefix token %s" (Token.to_string token))
;;

type precedence = Lowest
(* TODO *)
(* | Equals (* == *) *)
(* | LessGreater (* < or > *) *)
(* | Sum (* + *) *)
(* | Product (* * *) *)
(* | Prefix (* -x or !x *) *)
(* | Call (* myFunction() *) *)

let parse_expression parser _precedence =
  parse_prefix parser
  |> Result.map_error
       (Printf.sprintf
          "could not parse expression starting with token %s: %s"
          (Token.to_string parser.curr_token))
;;

let parse_expression_statement parser =
  let* expression, parser = parse_expression parser Lowest in
  let parser = if parser.next_token == Token.Semicolon then advance parser else parser in
  Ok (Ast.Expression expression, parser)
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
