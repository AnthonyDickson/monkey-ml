type t =
  | Illegal of char
  | Eof
  (* Identifiers + literals *)
  | Ident of string (* add, foobar, x, y, ... *)
  | Int of int (* 1343456 *)
  | String of string (* "camel camel" *)
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
  | LBracket (* [ *)
  | RBracket (* ] *)
  (* Keywords *)
  | Function
  | Let
  | If
  | Else
  | Return

let to_string = function
  | Illegal ch -> Printf.sprintf "Illegal %c" ch
  | Eof -> "Eof"
  | Ident literal -> literal
  | Int literal -> Int.to_string literal
  | String literal -> literal
  | True -> "true"
  | False -> "false"
  | Assign -> "="
  | Plus -> "+"
  | Minus -> "-"
  | Asterisk -> "*"
  | Slash -> "/"
  | Bang -> "!"
  | Lt -> "<"
  | Gt -> ">"
  | Eq -> "=="
  | NotEq -> "!="
  | Comma -> ","
  | Semicolon -> ";"
  | Lparen -> "("
  | Rparen -> ")"
  | Lbrace -> "{"
  | Rbrace -> "}"
  | LBracket -> "["
  | RBracket -> "]"
  | Function -> "fn"
  | Let -> "let"
  | If -> "if"
  | Else -> "else"
  | Return -> "return"
;;
