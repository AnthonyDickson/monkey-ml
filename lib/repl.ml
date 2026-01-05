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

let evaluate line =
  let lexer = Lexer.make line in
  let parser = Parser.make lexer in
  let* program =
    Parser.parse_program parser |> Result.map_error (fun (_, errors) -> errors)
  in
  Evaluator.evaluate program
;;

let rec start () =
  print_string prompt;
  let line = read_line () in
  match evaluate line with
  | Ok value ->
    print_endline (Value.to_string value);
    start ()
  | Error error ->
    print_endline monkey_face;
    print_endline "Woops! We ran into some monkey business here!";
    Printf.printf "%s\n" error;
    start ()
;;
