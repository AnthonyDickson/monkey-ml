type token_type =
  | Illegal
  | Eof
  (* Identifiers + literals *)
  | Ident (* add, foobar, x, y, ... *)
  | Int (* 1343456 *)
  | True
  | False
  (* Operators *)
  | Assign (* = *)
  | Plus (* + *)
  | Minus (* - *)
  | Asterisk (* * *)
  | Slash (* / *)
  | Bang (* ! *)
  | Lt (* < *)
  | Gt (* > *)
  | Eq (* == *)
  | NotEq (* != *)
  (* Delimiters *)
  | Comma (* , *)
  | Semicolon (* ; *)
  | Lparen (* ( *)
  | Rparen (* ) *)
  | Lbrace (* { *)
  | Rbrace (* } *)
  (* Keywords *)
  | Function
  | Let
  | If
  | Else
  | Return

let string_of_token_type = function
  | Illegal -> "Illegal"
  | Eof -> "Eof"
  | Ident -> "Ident"
  | Int -> "Int"
  | True -> "True"
  | False -> "False"
  | Assign -> "Assign"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Asterisk -> "Asterisk"
  | Slash -> "Slash"
  | Bang -> "Bang"
  | Lt -> "Lt"
  | Gt -> "Gt"
  | Eq -> "Eq"
  | NotEq -> "NotEq"
  | Comma -> "Comma"
  | Semicolon -> "Semicolon"
  | Lparen -> "Lparen"
  | Rparen -> "Rparen"
  | Lbrace -> "Lbrace"
  | Rbrace -> "Rbrace"
  | Function -> "Function"
  | Let -> "Let"
  | If -> "If"
  | Else -> "Else"
  | Return -> "Return"
;;

type token =
  { type_ : token_type
  ; literal : string
  }

let string_of_token token =
  let { type_ = token_type; literal } : token = token in
  let token_string = string_of_token_type token_type  in
  Printf.sprintf "{Type:%s  Literal:%s}" token_string literal
;;
