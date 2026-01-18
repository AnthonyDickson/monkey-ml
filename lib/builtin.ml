(**
Builtin function identifiers.

See `value.ml` for the related `Value` variant.
See `evaluator.ml` for the related evaluation code.
  *)

type t = Len

let get = function
  | "len" -> Some Len
  | _ -> None
;;

let to_string = function
  | Len -> "len"
;;
