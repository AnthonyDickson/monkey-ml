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

type token = {
  type_: token_type;
  literal: string
}
