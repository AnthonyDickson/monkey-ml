type t

val make : Lexer.t -> t
val parse_program : t -> (Ast.program, string) result
