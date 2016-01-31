try
  let lexbuf = Lexing.from_channel stdin in
  while true do
    let result = Parser.prog Lexer.token lexbuf in
    print_string result; print_newline(); flush stdout
  done
with Lexer.Eof ->
  exit 0
