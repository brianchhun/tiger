let _ =
  let lexbuf = Lexing.from_channel stdin in
  let absyn = Parser.program Lexer.token lexbuf in
  Prabsyn.print absyn;
  Semant.trans_prog absyn
