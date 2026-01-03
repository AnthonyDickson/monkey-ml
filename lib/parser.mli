type t

val make : Lexer.t -> t
val parse_program : t -> (Ast.Program.t, Ast.Program.t * string) result
