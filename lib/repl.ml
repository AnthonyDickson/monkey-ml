open Result.Syntax

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

let read_all () =
  let buffer = Buffer.create 1024 in
  let rec loop () =
    match read_line () with
    | line ->
      Buffer.add_string buffer line;
      Buffer.add_char buffer '\n';
      loop ()
    | exception End_of_file -> Buffer.contents buffer
  in
  loop ()
;;

let evaluate environment line =
  let lexer = Lexer.make line in
  let parser = Parser.make lexer in
  let* program =
    Parser.parse_program parser |> Result.map_error (fun (_, errors) -> errors)
  in
  Evaluator.evaluate environment program
;;

let rec loop environment =
  match read () with
  | line ->
    let result = evaluate environment line in
    (match result with
     | Ok (environment, value) ->
       print_endline (Value.to_string value);
       loop environment
     | Error error ->
       print_endline monkey_face;
       print_endline "Woops! We ran into some monkey business here!";
       Printf.printf "%s\n" error;
       loop environment)
  | exception End_of_file -> ()
;;

let start () =
  let environment = Environment.make () in
  if Unix.isatty Unix.stdin
  then loop environment
  else (
    let program = read_all () in
    match evaluate environment program with
    | Ok (_environment, value) -> print_endline (Value.to_string value)
    | Error error ->
      print_endline monkey_face;
      print_endline "Woops! We ran into some monkey business here!";
      Printf.printf "%s\n" error)
;;
