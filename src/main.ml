let emitproc = function
    Frame.PROC (body, frame) ->
      let stms = body |>
                 Canon.linearize |>
                 Canon.basic_blocks |>
                 Canon.trace_schedule in
      let instrs = List.concat (List.map (Codegen.codegen frame) stms) in
        List.iter (fun i -> print_string (Assem.format (Temp.string_of_temp) i)) instrs
  | Frame.STRING (label, s) ->
      print_string (Frame.string label s)

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let absyn = Parser.program Lexer.token lexbuf in
      Parsing.clear_parser();
      let frags = Semant.trans_prog absyn in
        List.iter emitproc frags
  with Parsing.Parse_error ->
    Error_msg.parse_error "syntax error"
