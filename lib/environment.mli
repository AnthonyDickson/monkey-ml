type t

val make : unit -> t
val get : t -> string -> Value.t option
val bind: t -> string -> Value.t -> t
