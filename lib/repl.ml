let ( let* ) = Result.bind
let prompt = ">> "

let monkey_face = {|
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

let loop () =
  print_string prompt;
  let line = read_line () in
  let* lexer = Lexer.make line in
  let parser = Parser.make lexer in
  match Parser.parse_program parser with
  | Ok program ->
    print_endline (Ast.Program.to_string program);
    Ok ()
  | Error (_, errors) -> Error errors
;;

let rec start () =
  match loop () with
  | Ok () -> start ()
  | Error error ->
    print_endline monkey_face;
    print_endline "Woops! We ran into some monkey business here!";
    Printf.printf "%s\n" error;
    start ()
;;
