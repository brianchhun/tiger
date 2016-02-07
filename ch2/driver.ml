let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Lexer.token lexbuf in
      print_string (Tokens.string_of_token result); print_newline(); flush stdout
    done
  with Lexer.End_of_file ->
    exit 0
