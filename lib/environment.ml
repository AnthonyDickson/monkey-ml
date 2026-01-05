(** A wrapper around a map for storing a programs bindings *)

module ValueMap = Map.Make (String)

type t = Value.t ValueMap.t

let make () = ValueMap.empty
let get environment binding = ValueMap.find_opt binding environment
let bind environment binding value = ValueMap.add binding value environment
