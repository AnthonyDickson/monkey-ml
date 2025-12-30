type token =
  | Illegal of char
  | Eof
  (* Identifiers + literals *)
  | Ident of string (* add, foobar, x, y, ... *)
  | Int of int (* 1343456 *)
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

let string_of_token = function
  | Illegal ch -> Printf.sprintf "Illegal %c" ch
  | Eof -> "Eof"
  | Ident literal -> Printf.sprintf "Ident %s" literal
  | Int literal -> Printf.sprintf "Int %d" literal
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
