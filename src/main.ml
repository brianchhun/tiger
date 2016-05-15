let _ =
  let lexbuf = Lexing.from_channel stdin in
  let absyn = Parser.program Lexer.token lexbuf in
  (* print_endline (Prabsyn.print absyn); *)
  Semant.trans_prog absyn
