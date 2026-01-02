type t

val make : string -> (t, string) result
val next_token : t -> Token.t * t
