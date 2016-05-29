let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let absyn = Parser.program Lexer.token lexbuf in
    (* print_endline (Prabsyn.print absyn); *)
    Parsing.clear_parser();
    Semant.trans_prog absyn
  with Parsing.Parse_error ->
    Error_msg.syntax_error()
    
