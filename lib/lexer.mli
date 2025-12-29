type lexer

val create : string -> (lexer, string) result
val next_token : lexer -> Token.token * lexer
