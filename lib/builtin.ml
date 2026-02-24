(**
Builtin function identifiers.

See `value.ml` for the related `Value` variant.
See `evaluator.ml` for the related evaluation code.
  *)

type t =
  | Len
  | First
  | Last
  | Rest
  | Push

let from_string_opt  = function
  | "len" -> Some Len
  | "first" -> Some First
  | "last" -> Some Last
  | "rest" -> Some Rest
  | "push" -> Some Push
  | _ -> None
;;

let to_string = function
  | Len -> "len"
  | First -> "first"
  | Last -> "last"
  | Rest -> "rest"
  | Push -> "push"
;;
