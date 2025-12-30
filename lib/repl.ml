let prompt = ">> "


let rec start () =
  print_string prompt;
  let rec loop lexer' =
    match Lexer.next_token lexer' with
    | Token.Eof, _ -> start ()
    | token, lexer'' ->
      print_endline (Token.string_of_token token);
      loop lexer''
  in
  let line = read_line () in
  let lexer = Lexer.create line in
  match lexer with
  | Ok lexer -> loop lexer
  | Error msg -> print_endline msg;
  start ()
;;
