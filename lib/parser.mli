type parser

val create : Lexer.lexer -> parser
val parse_program:  parser -> (Ast.program, string) result
