(** The envrionment will generally store [Value.t], however [Environment.t] is made
generic to avoid a circular definiton. *)
type 'a t

val make : unit -> 'a t
val get : 'a t -> string -> 'a option
val bind : 'a t -> string -> 'a -> 'a t

(** Merge environments. If an identifier exists in both environments, the
value is taken from the environment that is first argument. *)
val union : 'a t -> 'a t -> 'a t
