type lexer =
  { input : string (** [input] is the text to parse. *)
  ; position : int (** The current position in [input], points to [ch] *)
  ; read_position : int
    (** The next position to read from [input], points to the char after [ch]. *)
  ; current_char : char option (** The current char that is being processed *)
  }

let create input =
  if String.length input = 0
  then Error "Input must be a non-zero length string"
  else Ok { input; position = 0; read_position = 1; current_char = Some input.[0] }
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

let token_type_of_string = function
  | "fn" -> Token.Function
  | "let" -> Token.Let
  | "if" -> Token.If
  | "else" -> Token.Else
  | "return" -> Token.Return
  | "true" -> Token.True
  | "false" -> Token.False
  | _ -> Token.Ident
;;

let read_multichar_token_and_advance lexer is_valid token_type_of_string =
  let rec aux lexer' =
    match lexer'.current_char with
    | Some ch -> if is_valid ch then aux (advance lexer') else lexer'
    | None -> lexer'
  in
  let lexer' = aux lexer in
  let start_pos = lexer.position in
  let length = lexer'.position - start_pos in
  let literal = String.sub lexer.input start_pos length in
  let token : Token.token = { type_ = token_type_of_string literal; literal } in
  token, lexer'
;;

let read_identifier_and_advance lexer =
  read_multichar_token_and_advance lexer is_letter token_type_of_string
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let read_number_and_advance lexer =
  read_multichar_token_and_advance lexer is_digit (fun _ -> Token.Int)
;;

let rec next_token lexer =
  let make_token type_ ch : Token.token * lexer =
    { type_; literal = String.make 1 ch }, advance lexer
  in
  (* Assumes [literal] is two chars *)
  let make_token' type_ literal : Token.token * lexer =
    (* Advance lexer twice to consume two char token. *)
    { type_; literal }, advance @@ advance lexer
  in
  let curr_char = lexer.current_char in
  match curr_char with
  | Some '=' ->
    if peek_next_char lexer = Some '='
    then make_token' Token.Eq "=="
    else make_token Token.Assign '='
  | Some '+' -> make_token Token.Plus '+'
  | Some '-' -> make_token Token.Minus '-'
  | Some '*' -> make_token Token.Asterisk '*'
  | Some '/' -> make_token Token.Slash '/'
  | Some '!' ->
    if peek_next_char lexer = Some '='
    then make_token' Token.NotEq "!="
    else make_token Token.Bang '!'
  | Some '<' -> make_token Token.Lt '<'
  | Some '>' -> make_token Token.Gt '>'
  | Some '(' -> make_token Token.Lparen '('
  | Some ')' -> make_token Token.Rparen ')'
  | Some '{' -> make_token Token.Lbrace '{'
  | Some '}' -> make_token Token.Rbrace '}'
  | Some ',' -> make_token Token.Comma ','
  | Some ';' -> make_token Token.Semicolon ';'
  | Some ch when is_letter ch -> read_identifier_and_advance lexer
  | Some ch when is_digit ch -> read_number_and_advance lexer
  | Some ch when is_whitespace ch -> next_token (advance lexer)
  | Some ch -> make_token Token.Illegal ch
  | None -> { type_ = Token.Eof; literal = "" }, advance lexer
;;
