(** A wrapper around a map for storing a program's bindings *)

module ValueMap = Map.Make (String)

type 'a t = 'a ValueMap.t

let make () = ValueMap.empty
let get environment binding = ValueMap.find_opt binding environment
let bind environment binding value = ValueMap.add binding value environment

let union left right =
  let pick_left _key left' _right = Some left' in
  ValueMap.union pick_left left right
;;
