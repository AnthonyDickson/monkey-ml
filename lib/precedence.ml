type t =
  | Lowest
  | Equals (* == *)
  | LessGreater (* < or > *)
  | Sum (* + *)
  | Product (* * *)
  | Prefix (* -x or !x *)
  | Call (* myFunction() *)

let to_int = function
  | Lowest -> 0
  | Equals -> 1
  | LessGreater -> 2
  | Sum -> 3
  | Product -> 4
  | Prefix -> 5
  | Call -> 6
;;

let from_token token =
  let open Token in
  match token with
  | Eq | NotEq -> Equals
  | Lt | Gt -> LessGreater
  | Plus | Minus -> Sum
  | Asterisk | Slash -> Product
  | Lparen -> Call
  | _ -> Lowest
;;

let binds_tighter a b = to_int a < to_int b
