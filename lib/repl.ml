let ( let* ) = Result.bind
let prompt = ">> "

let monkey_face =
  {|
            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
|}
;;

let read () =
  print_string prompt;
  read_line ()
;;

let evaluate environment line =
  let lexer = Lexer.make line in
  let parser = Parser.make lexer in
  let* program =
    Parser.parse_program parser |> Result.map_error (fun (_, errors) -> errors)
  in
  Ok (Evaluator.evaluate environment program)
;;

let rec loop environment =
  let line = read () in
  match evaluate environment line with
  | Ok (environment, value) ->
    print_endline (Value.to_string value);
    loop environment
  | Error error ->
    print_endline monkey_face;
    print_endline "Woops! We ran into some monkey business here!";
    Printf.printf "%s\n" error;
    loop environment
;;

let start () =
  let environment = Environment.make () in
  loop environment
;;
