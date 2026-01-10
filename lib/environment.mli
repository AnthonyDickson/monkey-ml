(** A wrapper around a map for storing a program's bindings *)

(** The envrionment will generally store [Value.t], however [Environment.t] is made
generic to avoid a circular definiton. *)
type 'a t

val make : unit -> 'a t
val get : 'a t -> string -> 'a option
val bind : 'a t -> string -> 'a -> 'a t

(** Creates a new environment by extending [base]
    with [bindings]. When a key exists in both environments, the value from
    [bindings] takes precedence (shadowing).

    This is typically used to extend a function's closure environment with
    parameter bindings during function application. *)
val extend : base:'a t -> bindings:'a t -> 'a t
