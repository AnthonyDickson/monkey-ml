type t =
  { input : string (** [input] is the text to parse. *)
  ; position : int (** The current position in [input], points to [ch] *)
  ; read_position : int
    (** The next position to read from [input], points to the char after [ch]. *)
  ; current_char : char option (** The current char that is being processed *)
  }

let make input =
  if String.length input = 0
  then { input; position = 0; read_position = 0; current_char = None }
  else { input; position = 0; read_position = 1; current_char = Some input.[0] }
;;

let peek_next_char lexer =
  if lexer.read_position < String.length lexer.input
  then Some lexer.input.[lexer.read_position]
  else None
;;

let advance lexer =
  let is_eof = lexer.current_char = None in
  if is_eof
  then lexer
  else
    { lexer with
      position = lexer.read_position
    ; read_position = lexer.read_position + 1
    ; current_char = peek_next_char lexer
    }
;;

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let keyword_or_identifier_of_string = function
  | "fn" -> Token.Function
  | "let" -> Token.Let
  | "if" -> Token.If
  | "else" -> Token.Else
  | "return" -> Token.Return
  | "true" -> Token.True
  | "false" -> Token.False
  | literal -> Token.Ident literal
;;

let read_multichar_token_and_advance lexer is_valid token_of_literal =
  let rec aux lexer' =
    match lexer'.current_char with
    | Some ch -> if is_valid ch then aux (advance lexer') else lexer'
    | None -> lexer'
  in
  let lexer' = aux lexer in
  let start_pos = lexer.position in
  let length = lexer'.position - start_pos in
  let literal = String.sub lexer.input start_pos length in
  let token = token_of_literal literal in
  token, lexer'
;;

let read_identifier_and_advance lexer =
  read_multichar_token_and_advance lexer is_letter keyword_or_identifier_of_string
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let read_number_and_advance lexer =
  read_multichar_token_and_advance lexer is_digit (fun str ->
    (* int_of_string is could potentially raise an exception, consider handling
    the exception or using int_of_string_opt *)
    Token.Int (int_of_string str))
;;

(** A string starts with a double quote and ends with the next double quote or EOF.
    The surrounding double quotes are consumed by this function but are not
    included in the string literal.

    Escape sequences are not supported. *)
let read_string_and_advance lexer =
  (* This function is assumed to be called when the current char is '"'.
  We must skip over the opening double quote. *)
  let lexer = advance lexer in
  let token, lexer =
    read_multichar_token_and_advance
      lexer
      (fun ch -> ch <> '"')
      (fun str -> Token.String str)
  in
  if lexer.current_char = Some '"' then token, advance lexer else token, lexer
;;

let rec next_token lexer =
  let one_char token = token, advance lexer
  and two_char token = token, advance (advance lexer) in
  match lexer.current_char with
  | Some '=' ->
    if peek_next_char lexer = Some '=' then two_char Token.Eq else one_char Token.Assign
  | Some '+' -> one_char Token.Plus
  | Some '-' -> one_char Token.Minus
  | Some '*' -> one_char Token.Asterisk
  | Some '/' -> one_char Token.Slash
  | Some '!' ->
    if peek_next_char lexer = Some '=' then two_char Token.NotEq else one_char Token.Bang
  | Some '<' -> one_char Token.Lt
  | Some '>' -> one_char Token.Gt
  | Some '(' -> one_char Token.Lparen
  | Some ')' -> one_char Token.Rparen
  | Some '{' -> one_char Token.Lbrace
  | Some '}' -> one_char Token.Rbrace
  | Some '[' -> one_char Token.Lbracket
  | Some ']' -> one_char Token.Rbracket
  | Some ',' -> one_char Token.Comma
  | Some ';' -> one_char Token.Semicolon
  | Some '"' -> read_string_and_advance lexer
  | Some ch when is_letter ch -> read_identifier_and_advance lexer
  | Some ch when is_digit ch -> read_number_and_advance lexer
  | Some ch when is_whitespace ch -> next_token (advance lexer)
  | Some ch -> one_char (Token.Illegal ch)
  | None -> one_char Token.Eof
;;
