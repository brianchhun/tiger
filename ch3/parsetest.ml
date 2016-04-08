
let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      Parser.program Lexer.token lexbuf
    done
  with Lexer.End_of_file ->
    exit 0
