let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let absyn = Parser.program Lexer.token lexbuf in
      Parsing.clear_parser();
      ignore (Semant.trans_prog absyn)
  with Parsing.Parse_error ->
    Error_msg.parse_error "syntax error"
